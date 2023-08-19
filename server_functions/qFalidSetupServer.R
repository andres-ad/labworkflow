qFalidSetupServer <- function(input, output, session) {
  
  
  
  warning_message <- reactiveVal(NULL) 
  
  
  standard_values <- list(
    A01 = "10,000", B01 = "10,000", C01 = "10,000",
    D01 = "1,000", E01 = "1,000", F01 = "1,000",
    G01 = "100", H01 = "100", A02 = "100",
    B02 = "10", C02 = "10", D02 = "10",
    E02 = "1", F02 = "1", G02 = "1",
    H02 = "0.1", A03 = "0.1", B03 = "0.1",
    C03 = "0", E12 = "NTC"
  )
  
  
  
  
  # Reactive expression for reading the uploaded CSV file
  qfalid_scan <- reactive({
    inFile <- input$qFalid_scan_file
    if (is.null(inFile)) {
      return(NULL)
    }
    data <- read.csv(inFile$datapath, header = TRUE, check.names = FALSE)
    return(data)
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
  
  
  observeEvent(input$get_data_btn, {
    if (has_required_columns()) {
      # Read the uploaded file
      uploaded_data <- qfalid_scan()
      # Filter rows where "Tube ID" is not "No Code"
      uploaded_data <- uploaded_data[uploaded_data$`Tube ID` != "No Code",]
      
      # Read the Google Sheet
      ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
      sheet_data <- googlesheets4::read_sheet(ss_url)
      
      # Match "Tube ID" from uploaded_data with "GenomicID" from sheet_data
      matched_data <- uploaded_data %>%
        dplyr::left_join(sheet_data, by = c("Tube ID" = "GenomicID"))
      # Create a summary table containing the relevant columns
      summary_table <- matched_data %>%
        dplyr::select(`Tube Position`, `Tube ID`, LabID, FieldID)
      
      # Display the summary table in the Shiny UI
      output$summary_table <- DT::renderDataTable({
        DT::datatable(summary_table)
      })
      
      # Check for unmatched "Tube ID" and set the warning message
      unmatched_rows <- is.na(summary_table$FieldID)
      if (any(unmatched_rows)) {
        unmatched_positions <- summary_table$`Tube Position`[unmatched_rows]
        warning_message(paste("The following Tube Positions did not have a match in the Google Sheet:", paste(unmatched_positions, collapse = ", ")))
      } else {
        warning_message(NULL) # Reset if no unmatched positions
      }
    
      conflicts <- list()
      
      # Check and apply standard values
      # Check and apply standard values
      for (position in names(standard_values)) {
        if (position %in% summary_table$`Tube Position` && summary_table$`Tube ID`[summary_table$`Tube Position` == position] != "No Code") {
          conflicts <- append(conflicts, position)
        } else {
          # Create a new row with the standard values for the given position, aligned with the columns in summary_table
          new_row <- data.frame(
            `Tube Position` = position,
            `Tube ID` = "", # Set to "No Code" or other appropriate value
            LabID = standard_values[[position]],
            FieldID = "",
            # Add other columns as needed to match the structure of matched_data
            stringsAsFactors = FALSE
          )
          colnames(new_row) = c("Tube Position", "Tube ID", "LabID", "FieldID")
          # Combine the new row with summary_table
          summary_table <- rbind(summary_table, new_row)
        }
      }
      
      
      # If there are conflicts, display a warning and present options
      if (length(conflicts) > 0) {
        showModal(modalDialog(
          title = "Warning",
          paste("The following Standard positions are not empty:", paste(conflicts, collapse = ", ")),
          footer = tagList(
            actionButton("keep_scanned", "Keep scanned tubes"),
            actionButton("keep_standards", "Keep standards"),
            modalButton("Cancel")
          )
        ))
      }
      
      
      ## create layout
      
      # Extract the 'Tube Position' column
      positions <- summary_table$`Tube Position`
      
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
          summary_table$LabID[i],
          summary_table$FieldID[i],
          summary_table$`Tube ID`[i],
          sep = "<br/>"
        )
      }
      
      # Convert the layout matrix to a data frame for rendering
      layout_df <- as.data.frame(layout_matrix)
      
      # Render the layout as a table
      output$layout_table <- renderDT({
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
  
  
  
  output$warning_message <- renderUI({
    msg <- warning_message()
    if (!is.null(msg)) {
      tags$div(class = "alert alert-warning", msg)
    }
  })
  
  
}
