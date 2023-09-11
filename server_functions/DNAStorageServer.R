library(lubridate)
read_custom_format_malex <- function(file_path) {
  # Read the custom header
  custom_header <- readLines(file_path, n = 3)
  
  # Extract relevant info if needed (can be expanded upon)
  name <- gsub("Name:", "", custom_header[1])
  malex <- gsub("MALEX:", "", custom_header[2])
  date <- gsub("Date:", "", custom_header[3])
  
  # Read the actual data, skipping the first 6 lines
  data <- read.csv(file_path, skip = 4, header = TRUE)
  
  list(
    name = str_trim(name),
    malex = str_trim(malex),
    date = str_trim(date),
    data = data
  )
}





DNAStorageServer <- function(input, output, session) {
  
  database_data_reactive = reactiveVal(database_data)
  
  malex_micronic_joinButton = reactiveVal(FALSE)
  
  malex_dna_data_noheader <- reactive({
    malex_dna_input_file <- input$malex_file_input
    if (is.null(malex_dna_input_file)) {
      return(NULL)
    }
    
    # Read the entire file as lines
    malex_all_lines <- readLines(malex_dna_input_file$datapath)
    # Find the index of the header
    malex_dna_header_row_index <- which(grepl("Position.*LabID.*FieldID", malex_all_lines))
    
    # Check if the header was found
    if (length(malex_dna_header_row_index) > 0) {
      data_txt <- paste(malex_all_lines[malex_dna_header_row_index:length(malex_all_lines)], collapse = "\n")
      actual_data <- read.csv(textConnection(data_txt), header = TRUE)
      return(actual_data)
    } else {
      return(NULL)
    }
  })
  
  malex_file_input_header_data <- reactive({
    # If no file uploaded, return NULL
    if (is.null(input$malex_file_input)) {
      return(NULL)
    }
    read_custom_format_malex(input$malex_file_input$datapath)
  })
  
  output$malexfileCustomHeaderDisplay <- renderUI({
    if(is.null(malex_file_input_header_data())) {
      return(HTML("Please upload a CSV file."))
    }
    
    # Otherwise, display the custom header in a formatted way
    malex_file_input_custom_header <- malex_file_input_header_data()
    
    HTML(paste0(
      "Name: ", malex_file_input_custom_header$name,"<br/>",
      "MALEX: ", malex_file_input_custom_header$malex,"<br/>",
      "Date: ", malex_file_input_custom_header$date,"<br/>"
    ))
  })
  
  
  
  micronic_data <- reactive({
    malex_dna_input_file <- input$micronic_file_input
    if (is.null(malex_dna_input_file)) {
      return(NULL)
    }
    # Read the Micronic CSV file
    data <- read.csv(malex_dna_input_file$datapath, header = TRUE,check.names = FALSE)
    return(data)
  })
  
  observe({
    malex_dna_input_file <- input$micronic_file_input
    if (!is.null(malex_dna_input_file)) {
      
      micronic_file_inputname <- malex_dna_input_file$name
      
      # Get the path to the uploaded file
      filePath <- malex_dna_input_file$datapath
      tryCatch({
        drive_folder <- drive_get(as_id("1QwHB7ZUpWyNimYXWIUJ8MGUWHJIQ8zBa"))
        drive_files <- drive_ls(path = drive_folder)
        
        target_file_name <- paste0(prefix_files, micronic_file_inputname)
        
        # Check if file already exists
        if (!target_file_name %in% drive_files$name) {
          drive_upload(filePath, path = drive_folder, name = target_file_name)
          shinyalert::shinyalert(title = "Success!", text = "Micronics scan backed up in Google Drive", type = "success")
        } else {
          shinyalert::shinyalert(title = "Info", text = "Micronics scan already exists in Google Drive", type = "info")
        }
      }, error = function(e) {
        shinyalert::shinyalert(title = "Warning!", text = "Could not back up Micronics scan in Google Drive", type = "warning")
      })
    }
  })
  
  output$micronicDetailsDisplay <- renderUI({
    data <- micronic_data()
    if (is.null(data) || nrow(data) < 1) {
      return(NULL)
    }
    # Extract the values from the first row
    rack_id <- data$'Rack ID'[1]
    # Format the date
    date <- ymd(data$Date[1])
    formatted_date <- format(date, "%d %b %Y")
    
    # Format the time
    time_num <- as.character(data$Time[1])
    # Pad the time with leading zeros to ensure it's always 6 characters
    time_num <- str_pad(time_num, 6, pad = "0")
    hours <- substr(time_num, 1, 2)
    minutes <- substr(time_num, 3, 4)
    seconds <- substr(time_num, 5, 6)
    formatted_time <- paste(hours, minutes, seconds, sep = ":")
    
    # Create a UI display for the values
    tagList(
      tags$div(paste("Rack ID:", rack_id)),
      tags$div(paste("Date:", formatted_date)),
      tags$div(paste("Time:", formatted_time))
    )
  })
  
  
  output$malex_micronic_join_button <- renderUI({
    if (!is.null(micronic_data()) & !is.null(malex_dna_data_noheader())) {
      actionButton("malex_micronic_join_button", "Join Data")
    }
  })
  
  
  # Function to extract joined data
  get_joined_data <- function(malex, micronic) {
    dna_subset <- malex[c("Position", "LabID", "FieldID","Study_Code","Specimen_Type")]
    micronic_subset <- micronic[, c("Tube Position", "Tube ID")]
    colnames(micronic_subset) = c("TubePosition","TubeID")
    joined_data = right_join(dna_subset, micronic_subset, by = c("Position" = "TubePosition"))
    return(joined_data)
  }
  
  # Function to populate layout matrix
  populate_layout <- function(joined_df, layout_mat) {
    for(i in 1:nrow(joined_df)){
      row_index <- which(LETTERS == substr(joined_df$Position[i], 1, 1))
      col_index <- as.integer(substr(joined_df$Position[i], 2, 3))
      combined_info <- paste(joined_df$LabID[i], joined_df$FieldID[i], joined_df$`Tube ID`[i], sep = "<br/>")
      layout_mat[row_index, col_index] <- combined_info
    }
    return(layout_mat)
  }
  
  
  malex_micronic_joined_data = reactiveVal(NULL)
  
  observeEvent(input$malex_micronic_join_button, {
    if(is.null(malex_dna_data_noheader()) || is.null(micronic_data())) return(NULL)
    
    joined_data <- get_joined_data(malex_dna_data_noheader(), micronic_data())
    rack_barcode <- micronic_data()$`Rack ID`[1]
    
    problematic_rows <- joined_data %>% filter(
      ((TubeID=="No Code") & (!is.na(FieldID) | !is.na(LabID))) |
        (!(TubeID=="No Code") & (is.na(FieldID) | is.na(LabID)))
    )
    print(joined_data)
    if (nrow(problematic_rows) > 0) {
      shinyalert::shinyalert(
        title = "Warning!", 
        text = "There are some tubes with no data or samples with no tube. The resulting file will only contain those who have matches.", 
        type = "warning"
      )
    }
    
    joined_df <- joined_data %>% 
      mutate(PlateName = input$dna_extraction_plate_name_input,
             Freezer = input$dna_extraction_freezer_input,
             Shelf = input$dna_extraction_shelf_input,
             Basket = input$dna_extraction_basket_input,
             PlateBarcode = rack_barcode) %>%
      select(Position, TubeID, Study_Code, FieldID, Specimen_Type,
             PlateName, Freezer, Shelf, Basket, PlateBarcode, LabID) %>%
      filter(!(TubeID == "No Code" & FieldID == "NA" & LabID == "NA"))
    
    # Modify column names and layout
    colnames(joined_df) <- c("Position", "Tube ID", "StudyCode",
                             "StudySubject", "SpecimenType",
                             "PlateName", "FreezerName",
                             "ShelfName", "BasketName",
                             "PlateBarcode", "Comment")
    joined_df_forlayout <- select(joined_df, Position, Comment, StudySubject, `Tube ID`)
    colnames(joined_df_forlayout) = c("Position","LabID","FieldID","Tube ID")
    
    # Initialize an empty layout with row names A-H and column names 1-12
    layout_mat <- matrix(NA, nrow=8, ncol=12, dimnames = list(LETTERS[1:8], as.character(1:12)))
    layout_mat= populate_layout(joined_df_forlayout, layout_mat)
    
    label_col <- rep("LabID<br/>FieldID<br/>MicronicID", nrow(layout_mat))
    layout_mat_with_labels <- cbind(layout_mat, label_col)
    
    # Convert the layout matrix to a data frame for rendering
    layout_mat_df <- as.data.frame(layout_mat_with_labels)
    colnames(layout_mat_df)[ncol(layout_mat_df)] <- " "
    
    joined_df_filtered <- joined_df %>%
      filter(`Tube ID` != "No Code" & StudySubject != "NA" & Comment != "NA")
    
    
    malex_micronic_joined_data(joined_df_filtered)
    
    existing_ids <- database_data[["DNAStorage"]]$MicronicID
    new_ids <- joined_df_filtered$`Tube ID`
    any_ids_exist_already <- any(new_ids %in% existing_ids)
    
    if (any_ids_exist_already) {
      shinyalert::shinyalert(
        title = "Warning!", 
        text = "There are Micronic IDs that are already in the database. This needs to be solved to submit data and create the storage file.", 
        type = "warning"
      )
    }
    
    missing_data = any(c(input$dna_extraction_plate_name_input=="",input$dna_extraction_freezer_input=="",
                         input$dna_extraction_shelf_input==0,input$dna_extraction_basket_input==0))
    
    if (missing_data) {
      shinyalert::shinyalert(
        title = "Warning!", 
        text = "Missing Micronic Plate name, freezer, shelf or basket. Enter this information and re-join the data before proceeding", 
        type = "warning"
      )
    }
    
    
    output$joined_layout_output <- renderUI({
      tagList(
        if(!any_ids_exist_already & !missing_data){actionButton("dnastorage_submitdata","Submit data")},
        renderDT({
          datatable(layout_mat_df, escape = FALSE, options = list(
            columnDefs = list(
              list(targets = "_all", orderable = FALSE, className = "dt-center"),
              list(targets = "_all", className = "dt-head-center")
            ),
            pageLength = -1,
            dom = 't',
            autoWidth = TRUE
          )) %>%
            formatStyle(0, fontWeight = 'bold', fontSize = '10px', padding = '1px 1px') %>%
            formatStyle(1:(ncol(layout_mat_df) - 1), borderRight = '1px solid black', borderBottom = '1px solid black', fontSize = '10px', padding = '1px 1px') %>%
            formatStyle(ncol(layout_mat_df), borderRight = 'none', borderBottom = 'none', fontSize = '10px', padding = '1px 1px')
        })
      )
    })
    
    
  })
  
  
  observeEvent(input$dnastorage_submitdata, {
    database_data=database_data_reactive()
    
    joined_df_filtered = malex_micronic_joined_data()
    
    malex_file_input_custom_header <- malex_file_input_header_data()
    
    name = malex_file_input_custom_header$name
    malex = malex_file_input_custom_header$malex
    
    filename=paste(prefix_files,"DNA_storage_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
    
    write.csv(joined_df_filtered,  paste0(path_for_files,"/DNAStorage/",filename) , row.names = FALSE)
    shinyalert::shinyalert(title = "Success!", text = paste0("CSV file saved successfully \n Location: ",
                                                             path_for_files,"/DNAStorage/",filename), type = "success")
    
    tryCatch({
      drive_folder <- drive_get(as_id("1wEgG74WOaOWd1j0dR1xjmw7x0iyrqD2Z"))
      drive_upload(paste0(path_for_files,"/DNAStorage/",filename), path = drive_folder, name = filename)
      shinyalert::shinyalert(title = "Success!", text = "CSV backed up in Google Drive", type = "success")
    }, error = function(e) {
      shinyalert::shinyalert(title = "Warning!", text = "Could not back up CSV in Google Drive", type = "warning")
    })
    
    #update the database
    joined_df_filtered_database = joined_df_filtered %>% 
      select(StudyCode,StudySubject,Comment,`Tube ID`) %>% 
      mutate(User = name, MALEX = malex, Date = format(Sys.Date(), "%d%b%Y"))
    colnames(joined_df_filtered_database) = c("Study","FieldID","LabID","MicronicID","User","MALEX","Date")
    
    local_database_updated = database_data_reactive()
    local_database_updated[["DNAStorage"]] = rbind(database_data_reactive()[["DNAStorage"]], joined_df_filtered_database)
    database_data = update_database(local_database_updated, local_database_path, "DNAStorage", google_sheet_url)
    database_data_reactive(database_data)
  })
  
}