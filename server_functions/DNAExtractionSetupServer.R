DNAExtractionSetupServer <- function(input,output,session){
  
  #server for Matching Samples
  table_generated <- reactiveVal(FALSE)
  
  samples_data <- reactiveVal(data.frame(Position = character(), LabID = character(), FieldID = character()))
  
  layoutGenerated <- reactiveVal(FALSE)
  
  
  numRows <- reactiveVal(0)
  
  observeEvent(input$add_sample_group, {
    
    # Increment the reactive value
    numRows(numRows() + 1)
    
    newRowID <- as.character(numRows())
    
    # Create unique IDs for the new inputs
    rowID <- paste0("sample_row_", newRowID)
    
    newUI <- div(id = rowID, class="row",
                 column(4, 
                        textInput(paste0("prefix_input_", newRowID), label = "Prefix:", 
                                  placeholder = "Enter prefix")
                 ),
                 column(4,
                        numericInput(paste0("starting_number_input_", newRowID), label = "Starting Number:", 
                                     value = 0, min = 0)
                 ),
                 column(4,
                        numericInput(paste0("number_of_samples_input_", newRowID), label = "Number of Samples:", 
                                     value = 1, min = 1)
                 )
    )
    
    # Insert new UI elements above the button
    insertUI(
      selector = "#sample_entry_area",
      where = "beforeEnd",
      ui = newUI
    )
  })
  
  
  observeEvent(input$remove_sample_group, {
    
    # Only proceed if there are added rows to remove and at least one row is left
    if (numRows() > 0) {
      
      # Extract the ID of the last row
      lastRowID <- paste0("sample_row_", as.character(numRows()))
      
      # Remove the associated UI elements
      removeUI(selector = paste0("#", lastRowID))
      
      # Decrement the reactive value
      numRows(numRows() - 1)
    }
  })
  
  # Function to generate a sequence for a given group
  generate_samples_seq <- function(prefix, start, num) {
    # If starting number is 0, just return the prefix
    if (start == 0) {
      return(rep(prefix, num))
    }
    
    # Original logic for generating LabID
    end <- start + num - 1
    sapply(seq(start, end), function(x) paste0(prefix, sprintf("%03d", x)))
  }
  
  submit_samples_process <- function(){
    # List to store each group's sequence
    sequences_list <- list()
    
    # First group (always present)
    sequences_list[[1]] <- generate_samples_seq(input$prefix_input, 
                                                input$starting_number_input, 
                                                input$number_of_samples_input)
    
    # Additional groups (if any)
    if(numRows() > 0) {  # <-- Check if additional rows exist
      for (i in 1:numRows()) {
        prefix <- input[[paste0("prefix_input_", i)]]
        start <- input[[paste0("starting_number_input_", i)]]
        num <- input[[paste0("number_of_samples_input_", i)]]
        
        # Make sure all fields have valid values before generating sequences
        if(!is.null(prefix) && !is.null(start) && !is.null(num)) {
          sequences_list[[i + 1]] <- generate_samples_seq(prefix, start, num)
        }
      }
    }
    
    # Before converting list to dataframe, check the total number of samples
    total_samples <- sum(sapply(sequences_list, length))
    
    # If total_samples exceeds 96, display a warning
    if (total_samples > 96) {
      showModal(modalDialog(
        title = "Warning",
        "Only 96 samples allowed.",
        easyClose = TRUE
      ))
      return()  # exit the observer here without generating the dataframe
    }
    
    # Convert list to dataframe
    samples_df <- data.frame(LabID = unlist(sequences_list))
    
    # Get the number of rows in samples_df to help in generating Well and Column columns
    num_rows <- nrow(samples_df)
    
    # Create Well column based on the number of rows
    wells <- rep(LETTERS[1:8], times = 12)[seq_len(num_rows)]
    
    # Create Column column based on the number of rows
    columns <- sprintf('%02d', rep(1:12, each = 8))[seq_len(num_rows)]
    
    # Add the Well and Column columns to samples_df
    samples_df$Well <- wells
    samples_df$Column <- columns
    samples_df$Position <- paste0(samples_df$Well,samples_df$Column)
    samples_df$FieldID <- ""
    # Reorder the columns
    samples_df <- samples_df[, c('Position', 'LabID', 'FieldID')]
    
    # Existing samples_data
    previous_samples <- samples_data()
    
    # Match the rows based on Position and LabID
    matching_rows <- samples_df$Position %in% previous_samples$Position 
    
    # For rows that match, use FieldID from previous_samples
    samples_df$FieldID[matching_rows] <- previous_samples$FieldID[match(samples_df$Position[matching_rows], previous_samples$Position)]
    
    # For non-matching rows, FieldID is already empty
    
    # Update the reactive value with the new data
    samples_data(samples_df)
    
    # Display the data frame
    output$samples_output <- renderRHandsontable({
      df <- samples_data()
      if (!is.null(df)) {
        rhandsontable(df, rowHeaders = FALSE)
      }
    })
    table_generated(TRUE)
    output$generate_layout_button <- renderUI({
      if (table_generated()) {
        actionButton("generate_layout", "Generate layout")
      }
    })
    
  }
  
  observeEvent(input$submit_samples, {
    submit_samples_process()
  })
  
  output$table_generated <- reactive({ table_generated() })
  
  observe({
    if (!is.null(input$samples_output)) {
      # Capture the current state of the table after user edits
      new_data <- hot_to_r(input$samples_output)
      # Update the reactive value
      samples_data(new_data)
    }
  })
  
  
  observeEvent(input$generate_layout, {
    layoutGenerated(TRUE)
    
    # Initialize an empty layout with row names A-H and column names 1-12
    layout <- matrix(NA, nrow=8, ncol=12)
    rownames(layout) <- LETTERS[1:8]
    colnames(layout) <- as.character(1:12)
    
    # Fetch the current dataframe
    df <- samples_data()
    
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)){
      # Extract row and column info from the Position column (e.g., "E02")
      row_index <- which(LETTERS == substr(df$Position[i], 1, 1))
      col_index <- as.integer(substr(df$Position[i], 2, 3))
      
      # Assign LabID and FieldID to the correct position in the layout matrix
      combined_info <- paste(df$LabID[i], df$FieldID[i], sep = "<br/>")  # Use <br/> for line break
      layout[row_index, col_index] <- combined_info
    }
    
    # Render this table to the output
    output$layout_output <- DT::renderDT({
      datatable(layout, 
                escape = FALSE,  # to allow HTML content in cells
                options = list(
                  columnDefs = list(
                    list(targets = "_all", orderable = FALSE, className = "dt-center"),  # Disable sorting and center content
                    list(targets = "_all", className = "dt-head-center")  # Center headers
                  ),
                  pageLength = -1,  # Display all rows
                  dom = 't',  # Just the table (no other controls)
                  autoWidth = TRUE  # Auto adjust column width
                ),
                rownames = TRUE
      ) %>% 
        formatStyle(columns = 0, 
                    fontWeight = 'bold',
                    fontSize = '10px',
                    padding = '1px 1px'
        ) %>%  # Make row names bold and set font size to 10
        formatStyle(columns = 1:ncol(layout), 
                    borderRight = '1px solid black', 
                    borderBottom = '1px solid black',
                    fontSize = '10px',
                    padding = '1px 1px'  # Set font size to 10 for cell contents
        )
    }, container = htmltools::div(style = "font-size: 50%; width: 100%;")
    )
  })
  
  
  
  
  
  
  observeEvent(input$reset_malex_setup, {
    updateTextInput(session, "dna_extraction_name_input", value = "")
    updateTextInput(session, "malex_input", value = "")
    updateTextInput(session, "dna_extraction_country_input", value = "")
    updateTextInput(session, "dna_extraction_province_input", value = "")
    updateTextInput(session, "prefix_input", value = "")
    updateNumericInput(session, "starting_number_input", value = 0)
    updateNumericInput(session, "number_of_samples_input", value = 1)
    output$layout_output <- renderDT({
      # Return an empty or default data table. 
      # Replace data.frame() with any default data if needed.
      data.frame()
    })
    submit_samples_process()
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      name = str_remove_all(input$dna_extraction_name_input, " ")
      malex = str_remove_all(input$malex_input, " ")
      
      paste("DNA_ext_setup_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
    },
    
    content = function(file) {
      # Update empty FieldID to be the same as LabID
      updated_samples_data <- samples_data()
      empty_field_ids <- updated_samples_data$FieldID == ""
      updated_samples_data$FieldID[empty_field_ids] <- updated_samples_data$LabID[empty_field_ids]
      
      # Write custom header rows
      header_info <- c(
        paste("Name:", input$dna_extraction_name_input),
        paste("MALEX:", input$malex_input),
        paste("Country:", input$dna_extraction_country_input),
        paste("Province:", input$dna_extraction_province_input),
        paste("Date:", format(Sys.Date(), "%d%b%Y")),
        "",
        paste(colnames(updated_samples_data), collapse = ",")
      )
      
      writeLines(header_info, file)
      
      # Append the updated_samples_data to the same file without column names since the headers are custom
      write.table(updated_samples_data, file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",", quote = TRUE)
    }
  )
  
  
}