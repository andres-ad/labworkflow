library(lubridate)
read_custom_format <- function(file_path) {
  # Read the custom header
  custom_header <- readLines(file_path, n = 5)
  
  # Extract relevant info if needed (can be expanded upon)
  name <- gsub("Name:", "", custom_header[1])
  malex <- gsub("MALEX:", "", custom_header[2])
  country <- gsub("Country:", "", custom_header[3])
  province <- gsub("Province:", "", custom_header[4])
  date <- gsub("Date:", "", custom_header[5])
  
  # Read the actual data, skipping the first 6 lines
  data <- read.csv(file_path, skip = 6, header = TRUE)
  
  list(
    name = str_trim(name),
    malex = str_trim(malex),
    country = str_trim(country),
    province = str_trim(province),
    date = str_trim(date),
    data = data
  )
}





DNAStorageServer <- function(input, output, session) {
  
  joinButton = reactiveVal(FALSE)
  downloadActive <- reactiveVal(FALSE)
  joined_data_output <- reactiveValues(joinedData=NULL)
  joinedfileUploaded <- reactiveVal(FALSE)
  joinedDataDB_react <-reactiveVal(FALSE)
  joinedfileUploaded_forinputs1<- reactiveVal(FALSE)
  joinedfileUploaded_forinputs2<- reactiveVal(FALSE)
  joinedfileUploaded_forinputs3<- reactiveVal(FALSE)
  
  dna_data_noheader <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Read the entire file as lines
    all_lines <- readLines(inFile$datapath)
    # Find the index of the header
    header_row_index <- which(grepl("Position.*LabID.*FieldID", all_lines))
    
    # Check if the header was found
    if (length(header_row_index) > 0) {
      # Construct a text connection to read from the specific line onwards
      data_txt <- paste(all_lines[header_row_index:length(all_lines)], collapse = "\n")
      actual_data <- read.csv(textConnection(data_txt), header = TRUE)
      return(actual_data)
    } else {
      return(NULL)
    }
  })
  
  
  
  
  
  data <- reactive({
    # If no file uploaded, return NULL
    if (is.null(input$file1)) {
      return(NULL)
    }
    # Read the uploaded file with your custom function
    read_custom_format(input$file1$datapath)
  })
  
  output$fileCustomHeaderDisplay <- renderUI({
    # If no data is present (no file uploaded), return a message
    if(is.null(data())) {
      return(HTML("Please upload a CSV file."))
    }
    
    # Otherwise, display the custom header in a formatted way
    custom_header <- data()
    
    HTML(paste0(
      "Name: ", custom_header$name,"<br/>",
      "MALEX: ", custom_header$malex,"<br/>",
      "Country: ", custom_header$country,"<br/>",
      "Province: ", custom_header$province,"<br/>",
      "Date: ", custom_header$date,"<br/>"
    ))
  })
  
  
  
  micronic_data <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
    }
    # Read the Micronic CSV file
    data <- read.csv(inFile$datapath, header = TRUE,check.names = FALSE)
    return(data)
  })
  
  observe({
    inFile <- input$file2
    if (!is.null(inFile)) {
      
      file2name <- inFile$name
      
      # Get the path to the uploaded file
      filePath <- inFile$datapath
      
      # Define the Google Drive folder where you want to upload the file
    
      drive_folder <- drive_get(as_id("1QwHB7ZUpWyNimYXWIUJ8MGUWHJIQ8zBa"))
      
      # Upload the file to Google Drive
      drive_upload(filePath, path = drive_folder, name = file2name)
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
  
  
  observeEvent(input$joined_file_button, {
    output$upload_built_file <- renderUI({
      tagList(
        h4("This file needs to have the conventional naming, eg: DNA_storage_Jaishree_MALEX156_19Aug2023"),
        fileInput("file_upload_built_file", 
                  "Choose Joined file", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        )
      )
    })
  })
  
  joined_data_upload <- reactive({
    inFile <- input$file_upload_built_file
    if (is.null(inFile)) {
      return(NULL)
    }
    # Read the Micronic CSV file
    data <- read.csv(inFile$datapath, header = TRUE,check.names = FALSE)
    return(data)
  })
  
  observeEvent(joined_data_upload(), {
    
    if(!is.null(joined_data_upload())) {
      joined_data_output$joinedData <- joined_data_upload()
      joinedfileUploaded(TRUE)
    }
  })
  
  
  
  output$join_button <- renderUI({
    if (!is.null(micronic_data()) & !is.null(dna_data_noheader())) {
      actionButton("join_button", "Join Data")
    }
  })
  observeEvent(input$join_button, {
    if( !joinedfileUploaded()){
      joinButton(TRUE)
      downloadActive(TRUE)
      joined_data <- reactive({
        if (is.null(dna_data_noheader()) || is.null(micronic_data())) {
          return(NULL)
        }
        dna_subset <- dna_data_noheader()[c("Position", "LabID", "FieldID","Study_Code","Specimen_Type")]
        micronic_subset <- micronic_data()[, c("Tube Position", "Tube ID")]
        colnames(micronic_subset) = c("TubePosition","TubeID")
        right_join(dna_subset, micronic_subset, by = c("Position" = "TubePosition"))
      })
      
      
      
      # Fetch the current dataframe
      mic = micronic_data() 
      rack_barcode=mic$`Rack ID`[1]
      
      joined_df <- joined_data()
      
      
      
      joined_data_output$joinedData <- joined_data() %>% 
        mutate(PlateName = input$dna_extraction_plate_name_input,
               Freezer = input$dna_extraction_freezer_input,
               Shelf = input$dna_extraction_shelf_input,
               Basket = input$dna_extraction_basket_input,
               PlateBarcode = rack_barcode) %>% 
        select(Position,TubeID,Study_Code,
               FieldID,Specimen_Type,
               PlateName,Freezer,
               Shelf,Basket,
               PlateBarcode,LabID) %>% 
        filter(TubeID!="No Code" & FieldID !="NA" & LabID!="NA" )
      colnames(joined_data_output$joinedData) = c("Position","Tube ID","StudyCode",
                                                  "StudySubject","SpecimenType",
                                                  "PlateName","FreezerName",
                                                  "ShelfName","BasketName",
                                                  "PlateBarcode","Comment")
      
    }
  })
  
  layout_react = reactiveVal(NULL)
  
  
  observeEvent(reactiveValuesToList(input)[c("join_button","file_upload_built_file")],{
    if(joinedfileUploaded()| joinButton()){
      joinedDataDB = joined_data_output$joinedData %>% 
        mutate(User = ifelse(joinButton(), data()$name,
                             ifelse(joinedfileUploaded(),
                                    str_extract(input$file_upload_built_file, "(?<=DNA_storage_)[^_]+"),
                                    "Placeholder")),
               MALEX = ifelse(joinButton(), paste0("MALEX", data()$malex),
                              ifelse(joinedfileUploaded(), 
                                     paste0("MALEX",as.numeric(str_extract(input$file_upload_built_file, "(?<=_MALEX)\\d+"))),
                                     "Placeholder")),
               Date = ifelse(joinButton(), format(Sys.Date(), "%d%b%Y"),
                             ifelse(joinedfileUploaded(), 
                                    str_remove_all(sapply(strsplit(input$file_upload_built_file$name,"_"),tail,1),".csv"),
                                    "Placeholder"))
        )%>% 
        select('StudyCode','StudySubject','Comment','Tube ID','User','MALEX','Date')
      colnames(joinedDataDB) = c("Study","FieldID","LabID","MicronicID","User","MALEX","Date")
      joinedDataDB_react(joinedDataDB)
      joined_df <- joined_data_output$joinedData %>% 
        select(Position,Comment,'StudySubject','Tube ID')
      colnames(joined_df) <- c("Position","LabID","FieldID","TubeID")
      
      # Initialize an empty layout with row names A-H and column names 1-12
      joined_layout <- matrix(NA, nrow=8, ncol=12)
      rownames(joined_layout) <- LETTERS[1:8]
      colnames(joined_layout) <- as.character(1:12)
      
      
      # Loop through each row of the dataframe
      for(i in 1:nrow(joined_df)){
        # Extract row and column info from the Position column (e.g., "E02")
        row_index <- which(LETTERS == substr(joined_df$Position[i], 1, 1))
        col_index <- as.integer(substr(joined_df$Position[i], 2, 3))
        
        # Assign LabID and FieldID to the correct position in the layout matrix
        combined_joined_info <- paste(joined_df$LabID[i], joined_df$FieldID[i], joined_df$TubeID[i], sep = "<br/>")  # Use <br/> for line break
        joined_layout[row_index, col_index] <- combined_joined_info
      }
      layout_react(joined_layout)
      joinedfileUploaded(FALSE)
      joinButton(FALSE)
    }
  })
  
  ###### NOTE THAT THERE IS SOMETHING WERID HERE. THIS BIT WENT ON A LOOP UNLESS I MADE THOSE LAST 2 FALSE, I COULDNT FIGURE OUT WHAT WAS BEING UPDATED
  
  observeEvent(layout_react(),{
    if (!is.null(layout_react())){
      
      output$joined_layout_container <- renderUI({
        DTOutput("joined_layout_output")
      })
      
      # Render this table to the output
      output$joined_layout_output <- renderDT({
        datatable(layout_react(),
                  escape = FALSE,  # to allow HTML content in cells
                  options = list(
                    columnDefs = list(
                      list(targets = "_all", orderable = FALSE, className = "dt-center"),  # Disable sorting and center content
                      list(targets = "_all", className = "dt-head-center")  # Center headers
                    ),
                    pageLength = -1,  # Display all rows
                    dom = 't',  # Just the table (no other controls)
                    autoWidth = TRUE,  # Auto adjust column width,
                    redraw=TRUE
                  ),
                  rownames = TRUE
        ) %>%
          formatStyle(columns = 0,
                      fontWeight = 'bold',
                      fontSize = '10px',
                      padding = '1px 1px'
          ) %>%  # Make row names bold and set font size to 10
          formatStyle(columns = 1:ncol(layout_react()),
                      borderRight = '1px solid black',
                      borderBottom = '1px solid black',
                      fontSize = '10px',
                      padding = '1px 1px'  # Set font size to 10 for cell contents
          )
      })
    }
  })
  
  
  output$download_button_ui <- renderUI({
    if (downloadActive()) {
      tags$div(
        downloadButton("download_joined_data", "Download CSV"),
        style = "text-align: center;"  # CSS to center the content within the div
      )
    }
  })
  
  output$download_joined_data <- downloadHandler(
    
    filename = function() {
      name = str_remove_all(data()$name, " ")
      malex = str_remove_all(data()$malex, " ")
      
      paste("DNA_storage_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(joined_data_output$joinedData, file, row.names = FALSE)
      
      
      name = str_remove_all(data()$name, " ")
      malex = str_remove_all(data()$malex, " ")
      
      fileupload_name = paste("DNA_storage_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
      
      drive_folder <- drive_get(as_id("1wEgG74WOaOWd1j0dR1xjmw7x0iyrqD2Z"))
      drive_upload(file, path = drive_folder, name = fileupload_name)
      
    },
    contentType = "text/csv"
  )
  
  
  
  output$update_button_ui <- renderUI({
    if (!is.null(layout_react())) {
      tags$div(
        actionButton("update_database_button", "Update Database"),
        style = "text-align: center;"  # CSS to center the content within the div
      )
    }
  })
  
  
  observeEvent(input$update_database_button, {
    ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
    ss <- googlesheets4::gs4_get(ss_url)
    sheet_data <- googlesheets4::read_sheet(ss,"DNAStorage")
    
    # Filter out rows where Study is "Controls"
    filtered_sheet_data <- sheet_data %>% filter(Study != "Controls")%>% 
      mutate_all(as.character)
    
    data2join =joinedDataDB_react() %>% 
      mutate_all(as.character)
    
    
    # Find matching rows
    matching_rows <- data2join %>% 
      semi_join(filtered_sheet_data, by = c("FieldID", "LabID", "MicronicID"))
    
    # If there are matching rows, display a warning
    if (nrow(matching_rows) > 0) {
      
      # Extract the duplicate IDs
      dup_field_ids <- unique(matching_rows$FieldID)
      dup_lab_ids <- unique(matching_rows$LabID)
      dup_Micronic_ids <- unique(matching_rows$MicronicID)
      
      # Extract rows of FieldID, LabID, MicronicID for the duplicate entries
      dup_rows <- matching_rows %>%
        select(FieldID, LabID, MicronicID)
      
      # Convert the data frame to groups of three values
      rows_as_text <- lapply(1:nrow(dup_rows), function(i) {
        paste("(", paste(dup_rows[i,], collapse = ", "), ")", sep = "")
      })
      
      # Create a tag list for the warning message
      warning_content <- tagList(
        "The following samples are already in the Online Database:",
        tags$br(), # Line break
        "You must select Cancel or Proceed at the bottom of this window",
        tags$br(), # Line break
        "(Field ID, Lab ID, Micronic ID)",
        tags$br(),
        do.call(tagList, lapply(rows_as_text, function(item) {
          list(tags$span(item), tags$br())
        }))
      )
      
      # Show the warning in a modal dialog
      showModal(modalDialog(
        title = "Warning",
        warning_content,
        footer = tagList(
          actionButton("proceed_update", "Proceed with Update"),
          actionButton("cancel_update", "Cancel")
        )
      ))
      
      
      observeEvent(input$proceed_update, {
        removeModal()
        
        # The rest of the update code here...
        last_row <- nrow(sheet_data) + 2
        target_range <- paste0("A", last_row, ":G", last_row + nrow(data2join))
        googlesheets4::range_write(ss, data2join, range = target_range, col_names = FALSE)
        shinyalert::shinyalert(title = "Success!", text = "Upload successful", type = "success")
      })
      
      observeEvent(input$cancel_update, {
        removeModal()
        return()
      })
    } else {
      # If there are no matching rows, just proceed with the update
      last_row <- nrow(sheet_data) + 2
      target_range <- paste0("A", last_row, ":G", last_row + nrow(data2join))
      googlesheets4::range_write(ss, data2join, range = target_range, col_names = FALSE,sheet="DNAStorage")
      
      shinyalert::shinyalert(title = "Success!", text = "Upload successful", type = "success")
    }
  })
  
  
  
  
}