app_environment <- "production" # Change to production for the real world version  OR testing for testing

path_for_files <- ifelse(app_environment=="andres","G:/My Drive/EPPIcenter/Data/NICD/DatabaseExports","C:/Data/NICD/DatabaseExports")

prefix_files<-ifelse(app_environment=="testing","Testing_",ifelse(app_environment=="andres","AndresTesting_",""))
### GLOBAL FUNCTIONS

get_input_or_other <- function(input_val, other_input_val) {
  ifelse(input_val == "Other", other_input_val, input_val)
}

get_google_sheet_url <- function(app_environment){
  google_sheet_urls <- list(
    testing = "https://docs.google.com/spreadsheets/d/1A0d3CuMQrZqMVknMuVqmx2uhYr2mJIzuP1k1cawxGqs",
    production = "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s",
    andres = "https://docs.google.com/spreadsheets/d/16kV1t7m__MHgAclKrcBQf_qBLpgW_K6fxhF9Nf9D_e0"
  )
  
  google_sheet_url <- google_sheet_urls[[app_environment]]
  return(google_sheet_url)
}

get_local_database_paths <- function(app_environment){
  
  local_database_paths <- list(
    testing = ifelse(app_environment=="andres","G:/My Drive/EPPIcenter/Data/NICD/DatabaseTesting.xlsx","C:/Data/NICD/DatabaseTesting.xlsx"),
    production = ifelse(app_environment=="andres","G:/My Drive/EPPIcenter/Data/NICD/Database.xlsx","C:/Data/NICD/Database.xlsx"),
    andres = ifelse(app_environment=="andres","G:/My Drive/EPPIcenter/Data/NICD/DatabaseTestingAndres.xlsx","C:/Data/NICD/DatabaseTestingAndres.xlsx")
  )
  
  local_database_path <- local_database_paths[[app_environment]]
  return(local_database_path)
}



initialize_app <- function(google_sheet_url,local_database_path) {
  local_database = read_all_sheets(local_database_path)
  # Check for Internet connection by attempting to access Google Sheet
  online <- FALSE
  tryCatch({
    gs <- gs4_get(google_sheet_url)
    modifications_online_sheet = read_sheet(gs, sheet = "Modifications")
    online <- TRUE
  }, error = function(e) {
    warning("Could not connect to internet, using local copy")
  })
  
  # If online, synchronize local database with Google Sheet
  if (online) {
    local_database_last_modification = local_database[["Modifications"]]$DateTime[nrow(local_database[["Modifications"]])]
    online_database_last_modification = modifications_online_sheet$DateTime[nrow(modifications_online_sheet)]
    if(is.null(online_database_last_modification) || local_database_last_modification>online_database_last_modification ){ # if local is newer or doesn't have Modifications, update
      tryCatch({
        write_all_sheets_to_google(google_sheet_url,local_database)
      },error = function(e) {
        warning("Could not connect to internet, online copy may now be corrupted, which is ok if you reinitialize the App")
      }
      )
      upload_online = TRUE
    }else if( local_database_last_modification == online_database_last_modification){
      message("Online database up to date")
    }else{
      warning("Online database had newer data than the local copy. That can't be right, online copy won't be updated in this session, please contact Andres")
      upload_online = FALSE
    }
  }
  return(local_database)
}

read_all_sheets <- function(path) {
  # Get the names of all sheets in the Excel file
  sheet_names <- excel_sheets(path)
  
  # Read each sheet and store it in a named list
  sheets <- lapply(sheet_names, function(sheet) {
    read_excel(path, sheet = sheet)
  })
  names(sheets) <- sheet_names
  
  return(sheets)
}

read_all_sheets_from_google <- function(url) {
  # Get the Google Sheet by URL
  gs <- gs4_get(url)
  
  # Extract the names of the sheets
  sheet_names <- sheet_names(gs)
  
  # Read each sheet and store it in a named list
  sheets <- lapply(sheet_names, function(sheet) {
    read_sheet(gs, sheet = sheet)
  })
  names(sheets) <- sheet_names
  
  return(sheets)
}

write_all_sheets_to_google <- function(google_sheet_url,local_database){
  gs <- gs4_get(google_sheet_url)
  
  for(sheet_name in names(local_database)) {
    sheet_data <- local_database[[sheet_name]]
    # Delete the existing sheet
    if(sheet_name %in% sheet_names(gs)){
      sheet_delete(gs, sheet = sheet_name)
    }
    sheet_add(gs, sheet = sheet_name)
    # Add a new sheet and write the data
    write_sheet(gs, data = sheet_data, sheet = sheet_name)
  }
  
  # Optionally, print a success message
  message("Online database has been updated with local data.")
}

update_database <- function(local_database,local_database_path,tab,google_sheet_url){
  
  # Update sheet in excel file 
  wb <- loadWorkbook(local_database_path)
  writeData(wb, sheet = tab, x = local_database[[tab]])
  protectWorksheet(wb, sheet = tab, password = "armmorlab2023!")
  
  
  # Add row to Modifications
  
  nrows <- nrow(readWorkbook(local_database_path, sheet = "Modifications"))
  data_to_append <- data.frame(DateTime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Tab = tab)
  writeData(wb, sheet = "Modifications", x = data_to_append, startRow = nrows + 2, colNames = FALSE)
  protectWorksheet(wb, sheet = "Modifications", password = "armmorlab2023!")
  saveWorkbook(wb, local_database_path, overwrite = TRUE)
  database_data = local_database
  database_data[["Modifications"]] = rbind(database_data[["Modifications"]],data_to_append)
  
  
  tryCatch({
    write_all_sheets_to_google(google_sheet_url,database_data)
  },error = function(e) {
    warning("Could not connect to internet, online copy may now be corrupted, which is ok if you reinitialize the App")
  })
  
  shinyalert::shinyalert(title = "Success!", text = "Database updated successfully", type = "success")
  
  return(database_data)
  
}




global_get_default_value <- function(sheet_name, pattern) {
  1 + suppressWarnings(max(as.numeric(gsub(pattern, "", database_data[[sheet_name]][[pattern]])), na.rm = TRUE))
}

global_get_default_malex_value <- function() {
  global_get_default_value("DNAStorage", "MALEX")
}

global_get_default_qfalid_value <- function() {
  global_get_default_value("qFALIDResults", "qFALID")
}

global_get_default_rec_value <- function() {
  global_get_default_value("Receiving", "REV")
}





global_study_codes = c("GenE8","BF_SMC","Controls","Test")

global_provinces_names = list("South Africa" =c("KwaZuluNatal","Mpumalanga","Limpopo"),
                              "Angola" = c(sort(c("Benguela","Lunda Sul","Namibe","Bie","Uige","Zaire","Cuando Cubango","Moxico")),"NA"),
                              "Burkina Faso" = "None",
                              "Eswatini" = "Placeholder",
                              "Namibia" = "Placeholder",
                              "Zambia" = sort(c("Central","Copperbelt","Eastern","Luapula","Lusaka","Muchinga","North-Western","Northern","Southern","Western","NA")),
                              "Lab" = "Controls")

global_healthfacilities_names = list("South Africa" =c("NA"),
                              "Angola" = c(sort(c("HG Benguela","HM Benguela","HM Saurimo","CS Txizainga","CS Zona Sul","CS 5 de Abril","CS Piloto","HM Nharea","CS Quitexe","HM Damba","HM M Congo","HMI M Congo","HG Menongue","HM Calai","HM Luena","HM Luau")),"NA"),
                              "Burkina Faso" = "None",
                              "Eswatini" = "Placeholder",
                              "Namibia" = "Placeholder",
                              "Zambia" = c(sort(c("Roan","Mpumba","Twatasha","Mporokoso","Cheeba","Chembe","Chingi","Chitanda","Chiwempala","Kankumba","Katima","Masaiti","Milenge","Mulilima","Mwinilunga","Mzigawa","Nakachenje","Namseche","Nkeyema","Nsama","Nzoche","Siamuleya")),"NA"),
                              "Lab" = "Controls")


global_country_names = names(global_provinces_names)

local_database_path = get_local_database_paths(app_environment)
google_sheet_url = get_google_sheet_url(app_environment)

database_data <- initialize_app(google_sheet_url,local_database_path)

