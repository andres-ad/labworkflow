qFalidSetupServer <- function(input, output, session) {
  
  summary_table_standards = reactiveVal(NULL)
  
  warning_message <- reactiveVal(NULL) 
  
  
  standard_values <- data.frame(Position = c("A01","B01","C01","D01","E01","F01","G01","H01",
                                             "A02","B02","C02","D02","E02","F02","G02","H02",
                                             "A03","B03","C03","E12"),
                                LabID = c("10000","10000","10000","1000","1000","1000","100","100",
                                          "100","10","10","10", "1", "1", "1", "0.1",
                                          "0.1", "0.1", "0", "NTC"))
  standard_values$TubeID = standard_values$LabID
  standard_values$FieldID = standard_values$LabID
  
  colnames(standard_values) = c("Tube Position", "LabID","Tube ID","FieldID")
  
  # Reactive expression for reading the uploaded CSV file
  qfalid_scan <- reactive({
    inFile <- input$qFalid_scan_file
    if (is.null(inFile)) {
      return(NULL)
    }
    data <- read.csv(inFile$datapath, header = TRUE, check.names = FALSE)
    return(data)
  })
  
  
  observe({
    inFile <- input$qFalid_scan_file
    if (!is.null(inFile)) {
      # Generate the name you want to use for the uploaded file
      file2name <- paste0("YourPrefix_", inFile$name)
      
      # Get the path to the uploaded file
      filePath <- inFile$datapath
      # Define the Google Drive folder where you want to upload the file
      drive_folder <- drive_get(as_id("1ruFm-1qYVLIWwaJhvTjkMffqOuVOdGag"))
      
      # Upload the file to Google Drive
      drive_upload(filePath, path = drive_folder, name = file2name)
    }
  })
  
  # Reactive expression to check if the required columns are present
  has_required_columns <- reactive({
    inFile <- input$qFalid_scan_file
    required_columns <- c("Tube Position", "Tube ID", "Rack ID", "Date", "Time", "Free Text", "Status")
    if (!is.null(inFile) && tools::file_ext(inFile$name) == "csv") {
      data <- read.csv(inFile$datapath, header = TRUE, check.names = FALSE)
      return(all(required_columns %in% names(data)))
    }
    return(FALSE)
  })
  
  output$qFalidScanDisplay_or_warning <- renderUI({
    inFile <- input$qFalid_scan_file
    if (!is.null(inFile)) {
      if (has_required_columns()) {
        data <- qfalid_scan()
        # Extract the values from the first row
        rack_id <- data$'Rack ID'[1]
        date <- lubridate::ymd(data$Date[1])
        formatted_date <- format(date, "%d %b %Y")
        
        # Format the time
        time_num <- as.character(data$Time[1])
        time_num <- stringr::str_pad(time_num, 6, pad = "0")
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
      } else {
        tags$div(class = "alert alert-warning", "The uploaded file doesn't meet the criteria. Please upload a valid CSV file with the required columns.")
      }
    }
  })
  
  output$get_data_button <- renderUI({
    if (has_required_columns()) {
      actionButton("get_data_btn", "Get data from database")
    }
  })
  
  conflicts <- reactiveVal(NULL)
  
  observeEvent(input$get_data_btn, {
    if (has_required_columns()) {
      # Read the uploaded file
      uploaded_data <- qfalid_scan()
      # Filter rows where "Tube ID" is not "No Code"
      uploaded_data_filter <- uploaded_data[uploaded_data$`Tube ID` != "No Code",]
      
      # Read the Google Sheet
      ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
      sheet_data <- googlesheets4::read_sheet(ss_url,sheet="DNAStorage") %>% 
        mutate_all(as.character)
      
      # Match "Tube ID" from uploaded_data with "MicronicID" from sheet_data
      matched_data <- uploaded_data_filter %>%
        dplyr::left_join(sheet_data, by = c("Tube ID" = "MicronicID"))
      # Create a summary table containing the relevant columns
      summary_table <- matched_data %>%
        dplyr::select(`Tube Position`, `Tube ID`, LabID, FieldID)
      
      
      # Check for unmatched "Tube ID" and set the warning message
      unmatched_rows <- is.na(summary_table$FieldID)
      if (any(unmatched_rows)) {
        unmatched_positions <- summary_table$`Tube Position`[unmatched_rows]
        warning_message(paste("The following Tube Positions did not have a match in the Google Sheet:", paste(unmatched_positions, collapse = ", ")))
      } else {
        warning_message(NULL) # Reset if no unmatched positions
      }
      
      summary_table_standards(summary_table)
      # Update the value of conflicts based on the uploaded data
      conflict_positions <- which(summary_table$`Tube Position` %in% standard_values$'Tube Position')
      conflicts(conflict_positions)
      observe({
        conflict_values <- conflicts()
        # If there are conflicts, display a warning and present options
        if (length(conflict_values) > 0) {
          
          showModal(modalDialog(
            title = "Warning",
            paste("The following Standard positions are not empty:", paste(summary_table$`Tube Position`[conflict_values], collapse = ", ")),
            footer = tagList(
              actionButton("keep_scanned", "Keep scanned tubes"),
              actionButton("keep_standards", "Keep standards"),
              modalButton("Cancel")
            )
          ))
          summary_table_standards(summary_table)
        }else{
          summary_table_standards(rbind(standard_values,summary_table))
        }
      })
      
      
      
      
      observeEvent(input$keep_scanned, {
        print("Keeping scanned")
        # Identify the rows where there is a conflict
        summary_table_standards(rbind(standard_values[!standard_values$`Tube Position` %in% summary_table$`Tube Position`,
        ],
        summary_table))
        # You can close the modal dialog after the action is taken
        removeModal()
      })
      
      observeEvent(input$keep_standards, {
        print("Keeping standards")
        
        summary_table_standards(rbind(standard_values,summary_table[-conflicts(),]))
        
        # Close the modal dialog
        removeModal()
      })
      
      
      
      ## create layout
      output$layout_table <- renderDT({
        new_summary_table = summary_table_standards()
        # Extract the 'Tube Position' column
        positions <- new_summary_table$`Tube Position`
        
        # Convert the tube positions into rows (letters) and columns (numbers)
        rows <- sapply(positions, function(pos) substr(pos, 1, 1))
        columns <- sapply(positions, function(pos) as.numeric(substr(pos, 2, 3)))
        
        # Create an empty layout matrix with 8 rows (A-H) and 12 columns (1-12)
        layout_matrix <- matrix("", nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12))
        
        # Fill the layout matrix with the combined 'LabID', 'FieldID', and 'Tube ID' values
        for (i in seq_along(positions)) {
          row_idx <- match(rows[i], LETTERS[1:8])
          col_idx <- columns[i]
          layout_matrix[row_idx, col_idx] <- paste(
            new_summary_table$LabID[i],
            new_summary_table$FieldID[i],
            new_summary_table$`Tube ID`[i],
            sep = "<br/>"
          )
        }
        
        # Convert the layout matrix to a data frame for rendering
        layout_df <- as.data.frame(layout_matrix)
        
        # Render the layout as a table
        datatable(layout_df,
                  escape = FALSE, # to allow HTML content in cells
                  options = list(
                    columnDefs = list(
                      list(targets = "_all", orderable = FALSE, className = "dt-center"),
                      list(targets = "_all", className = "dt-head-center")
                    ),
                    pageLength = -1,
                    dom = 't',
                    autoWidth = TRUE
                  ),
                  rownames = TRUE
        ) %>%
          formatStyle(columns = 0,
                      fontWeight = 'bold',
                      fontSize = '10px',
                      padding = '1px 1px'
          ) %>%
          formatStyle(columns = 1:ncol(layout_df),
                      borderRight = '1px solid black',
                      borderBottom = '1px solid black',
                      fontSize = '10px',
                      padding = '1px 1px'
          )
      })
    }
  })
  
  
  
  
  # Assuming that summary_table_standards is a reactiveVal
  observe({
    # Check if summary_table_standards is not NULL
    if (!is.null(summary_table_standards())) {
      table_output <- summary_table_standards()
      
      # Create the required data frame
      export_data <- data.frame(
        Row = substr(table_output$`Tube Position`, 1, 1),
        Column = as.numeric(substr(table_output$`Tube Position`, 2, 3)),
        `*Target Name` = "varATS",
        `*Sample Name` = table_output$`Tube ID`,
        `*Biological Group` = ifelse(
          table_output$`Tube Position` %in% c("A01","B01","C01","D01","E01","F01","G01","H01","A02","B02","C02","D02","E02","F02","G02","H02","A03","B03","C03"),
          "Standard",
          table_output$LabID
        )
      )
      colnames(export_data) <- c("Row", "Column", "*Target Name", "*Sample Name", "*Biological Group")
      
      output$qfalid_export_button_ui <- renderUI({
        # You can check a condition here to determine whether to show the button
        # For example, you might want to check if new_summary_table() is not NULL
        if (!is.null(summary_table_standards())) {
          tags$div(
            downloadButton("download_file_qPCR", "Download CSV"),
            style = "text-align: center;"  # CSS to center the content within the div
          )
        }
      })
      
      # Setup the download handler
      output$download_file_qPCR <- downloadHandler(
        filename = function() {
          paste("qFALIDSetup_",input$qFalid_name_input,"_",
                "qFALID",input$qfalid_input,"_",
                format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
        },
        content = function(file) {
          
          write.csv(export_data,file, row.names = FALSE,quote = FALSE)
          
    
          filename_upload = paste("qFALIDSetup_",input$qFalid_name_input,"_",
                                  "qFALID",input$qfalid_input,"_",
                                  format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
          
          drive_folder <- drive_get(as_id("1LgL1yaU4YMz-x9brEhg16fYUzvlcospx"))
          drive_upload(file, path = drive_folder, name = filename_upload)
          
          
        },
        contentType = "text/csv"
      )
    }
  })
  
  
  
  
  output$warning_message <- renderUI({
    msg <- warning_message()
    if (!is.null(msg)) {
      tags$div(class = "alert alert-warning", msg)
    }
  })
}
