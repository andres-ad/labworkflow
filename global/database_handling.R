
google_sheet_urls <- list(
  testing = "https://docs.google.com/spreadsheets/d/1A0d3CuMQrZqMVknMuVqmx2uhYr2mJIzuP1k1cawxGqs",
  production = "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
)

local_cache_paths <- list(
  testing = "C:/Data/NICD/DatabaseTesting.xlsx",
  production = "C:/Data/NICD/Database.xlsx"
)


local_cache_path <- local_cache_paths[[app_environment]]

google_sheet_url <- google_sheet_urls[[app_environment]]




initialize_app <- function(google_sheet_url,local_cache_path) {
  local_database = read_all_sheets(local_cache_path)
  # Check for Internet connection by attempting to access Google Sheet
  online <- FALSE
  tryCatch({
    online_database = read_all_sheets_from_google(google_sheet_url)
    online <- TRUE
  }, error = function(e) {
    warning("Could not connect to internet, using local copy")
  })
  
  # If online, synchronize local cache with Google Sheet
  if (online) {
    local_database_last_modification = local_database[["Modifications"]]$DateTime[nrow(local_database[["Modifications"]])]
    online_database_last_modification = online_database[["Modifications"]]$DateTime[nrow(online_database[["Modifications"]])]
    if(local_database_last_modification>online_database_last_modification | is.null(online_database_last_modification)){ # if local is newer or doesn't have Modifications, update
      tryCatch({
        write_all_sheets_to_google(google_sheet_url,local_database)
      },error = function(e) {
        warning("Could not connect to internet, online copy may now be corrupted, which is ok if you reinitialize the App")
      }
      )
      upload_online = TRUE
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
  print("Online database has been updated with local data.")
}

update_database <- function(local_database,local_cache_path,tab,google_sheet_url){
  
  # Update sheet in excel file 
  wb <- loadWorkbook(local_cache_path)
  writeData(wb, sheet = tab, x = local_database[[tab]])
  protectWorksheet(wb, sheet = tab, password = "armmorlab2023!")
  
  
  # Add row to Modifications
  
  nrows <- nrow(readWorkbook(local_cache_path, sheet = "Modifications"))
  data_to_append <- data.frame(DateTime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Tab = tab)
  writeData(wb, sheet = "Modifications", x = data_to_append, startRow = nrows + 2, colNames = FALSE)
  protectWorksheet(wb, sheet = "Modifications", password = "armmorlab2023!")
  saveWorkbook(wb, local_cache_path, overwrite = TRUE)
  
  tryCatch({
    write_all_sheets_to_google(google_sheet_url,local_database)
  },error = function(e) {
    warning("Could not connect to internet, online copy may now be corrupted, which is ok if you reinitialize the App")
  })
  
  
}

