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
    
    newUI <- div(id = rowID, #class="row",
                 p(paste("Sample Group ",as.numeric(newRowID)+1)),
                 fluidRow(
                   column(3,tags$label(NULL),
                          selectInput(paste0("study_input_DNAExt_", newRowID), label = "Study:", 
                                      choices = c(global_study_codes,"Other"),
                                      selected = "GenE8"),
                          selectInput(paste0("content_input_", newRowID), label = "Content", 
                                      choices = c("DNA(DBS)","Empty","DNA(RDT)","Other"),
                                      selected = "DNA(DBS)")),
                   column(2,tags$label(NULL),
                          conditionalPanel(
                            condition = paste0('input.study_input_DNAExt_', newRowID, ' == "Other"'),
                            textInput(paste0("other_study_input_DNAExt_", newRowID), label = "Other Study", 
                                      placeholder = "Enter Study"
                            )
                          ),
                          conditionalPanel(
                            condition = paste0('input.', "content_input_", newRowID, ' == "Other"'),
                            textInput(paste0("other_content_input_", newRowID), label = "Other Content:", 
                                      placeholder = "Enter Content"
                            )
                          )),
                   column(3,tags$label(NULL),
                          textInput(paste0("prefix_input_", newRowID), label = "LabID Prefix:",  
                                    placeholder = "Enter prefix"),
                          numericInput(paste0("starting_number_input_", newRowID),
                                       label = "Start LabID#:", 
                                       value = 0, 
                                       min = 0)),
                   column(3,tags$label(NULL),
                          numericInput(paste0("number_of_samples_input_", newRowID), label = "# Samples", 
                                       value = 0, 
                                       min = 0),
                          fileInput(paste0('received_barcodes_', newRowID),'Barcodes file if available'))
                 ),
                 tags$hr()
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
    warnings <- character(0)
    
    for (i in 0:numRows()) {
      
      prefix <- input[[paste0("prefix_input_", i)]]
      start <- input[[paste0("starting_number_input_", i)]]
      num <- input[[paste0("number_of_samples_input_", i)]]
      
      # Make sure all fields have valid values before generating sequences
      if(!is.null(prefix) && !is.null(start) && !is.null(num)) {
        study_code_selected <- input[[paste0("study_input_DNAExt_", i)]]
        if (study_code_selected == "Other") {
          study_code <- input[[paste0("other_study_input_DNAExt_", i)]]
        } else {
          study_code <- study_code_selected
        }
        
        content_type_selected <- input[[paste0("content_input_", i)]]
        if (content_type_selected == "Other") {
          content_type <- input[[paste0("other_content_input_", i)]]
        } else {
          content_type <- content_type_selected
        }
        
        # Handle barcodes for this group
        barcode_file_input <- input[[paste0("received_barcodes_", i)]]
        
        if (!is.null(barcode_file_input)) {
          uploaded_file <- barcode_file_input$datapath
          lines <- readLines(uploaded_file)
          barcodes_line <- grep("Barcodes:", lines)
          barcodes <- lines[(barcodes_line + 1):length(lines)]
          num_samples <- length(barcodes)
          
          # Check if num_samples doesn't match the entered number (num)
          if (num_samples != num) {
            # Add warning to the warnings vector
            warnings <- c(warnings, paste0("Number of samples in uploaded file is not what you entered for Group ", i+1, ", it has been overwritten."))
            # Override num with num_samples
            num <- num_samples
          }
        } else {
          barcodes <- rep("", num) # Assuming 'num' is the number of samples in this group
        }
        
        sequences_list[[i + 1]] <- list(
          LabID = generate_samples_seq(prefix, start, num),
          Study_Code = rep(study_code, num),
          Specimen_Type = rep(content_type, num),
          FieldID = barcodes # Adding FieldID directly
        )
      }
      
    }
    
    
    # Before converting list to dataframe, check the total number of samples
    total_samples <- sum(sapply(sequences_list,
                                function(x) length(x$LabID)))
    
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
    samples_df <- do.call(rbind,
                          lapply(sequences_list,
                                 function(x) data.frame(
                                   LabID = x$LabID,
                                   Study_Code = x$Study_Code,
                                   Specimen_Type = x$Specimen_Type,
                                   FieldID = x$FieldID)))
    
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
    
    # Reorder the columns
    samples_df <- samples_df[, c('Position','Study_Code','Specimen_Type','LabID', 'FieldID')]
    
    
    repeated_lab_ids <- samples_df$LabID[duplicated(samples_df$LabID)]
    repeated_field_ids <- samples_df$FieldID[duplicated(samples_df$FieldID) & samples_df$FieldID != ""]
    
    
    if (length(repeated_lab_ids) > 0) {
      warnings <- c(warnings, paste("Repeated LabID detected: ", paste(repeated_lab_ids, collapse = ", ")))
    }
    
    if (length(repeated_field_ids) > 0) {
      warnings <- c(warnings, paste("Repeated FieldID detected: ", paste(repeated_field_ids, collapse = ", ")))
    }
    
    skip_block <- FALSE
    
    # Check for any non-null barcode inputs
    for (i in 0:numRows()) {
      if (!is.null(input[[paste0("received_barcodes_", i)]])) {
        skip_block <- TRUE
        break
      }
    }
    
    # Only execute the following code block if skip_block is FALSE
    if (!skip_block) {
      # Existing samples_data
      previous_samples <- samples_data()
      
      # Match the rows based on Position and LabID
      matching_rows <- ((samples_df$Position %in% previous_samples$Position) & previous_samples$FieldID != "")
      
      # For rows that match, use FieldID from previous_samples
      samples_df$FieldID[matching_rows] <- previous_samples$FieldID[match(samples_df$Position[matching_rows], previous_samples$Position)]
    }
    
    # For non-matching rows, FieldID is already empty
    
    # Update the reactive value with the new data
    samples_data(samples_df)
    
    if (length(warnings) > 0) {
      showModal(modalDialog(
        title = "Warnings",
        HTML(paste(warnings, collapse = "<br>")),
        easyClose = TRUE
      ))
    }
    
    # Display the data frame
    output$samples_output <- renderRHandsontable({
      df <- samples_data() %>% 
        filter(Specimen_Type!="Empty")
        
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
  
  layout_df_react = reactiveVal()
  observeEvent(input$generate_layout, {
    layoutGenerated(TRUE)
    
    # Fetch the current dataframe
    df <- samples_data()%>% 
      filter(Specimen_Type!="Empty")
    
    # Create an empty layout matrix with 8 rows (A-H) and 12 columns (1-12)
    layout <- matrix("", nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12))
    
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Extract row and column info from the Position column (e.g., "E02")
      row_index <- which(LETTERS == substr(df$Position[i], 1, 1))
      col_index <- as.integer(substr(df$Position[i], 2, 3))
      
      # Assign LabID and FieldID, concatenated with '<br/>' to the correct position in the layout matrix
      layout[row_index, col_index] <- paste(df$LabID[i], df$FieldID[i], sep = "<br/>")
    }
    
    
    # Create a new column filled with 'LabID<br/>FieldID'
    label_column <- matrix(rep("LabID<br/>FieldID", nrow(layout)), ncol = 1)
    
    # Combine the existing layout matrix with the label_column
    layout_with_labels <- cbind(layout, label_column)
    
    
    # Convert the layout matrix to a data frame for rendering
    layout_df <- as.data.frame(layout_with_labels)
    
    colnames(layout_df)[ncol(layout_df)] <- " "
    
    layout_df_react(layout_df)
    # Render the layout as a table (similar to the code in output$layout_table)
    output$layout_output_ui <- renderUI({
      tagList(
        downloadButton("download_malex_layout", "Download Table Image"),
        downloadButton('downloadData', 'Dowload/Upload data'),
        
        renderDT({
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
            formatStyle(columns = 1:(ncol(layout_df)-1),
                        borderRight = '1px solid black',
                        borderBottom = '1px solid black',
                        fontSize = '10px',
                        padding = '1px 1px'
            )%>%
            formatStyle(columns = ncol(layout_df),
                        borderRight = 'none',
                        borderBottom = 'none',
                        fontSize = '10px',
                        padding = '1px 1px'
            )
        })
      )
    })
    
    
  })
  

  
  # Replace the HTML line breaks with actual newline characters
  
  save_table_as_image <- function(layout_df, filename) {
    # Define the grid table
    grid_table <- tableGrob(layout_df,
                            rows = NULL, # Hide row names
                            theme = ttheme_default(
                              core = list(fg_params = list(hjust = 0.5, x = 0.5)), # Center text
                              colhead = list(fg_params = list(hjust = 0.5, x = 0.5)) # Center headers
                            ))
    
    # Define the height and width of the image (you can adjust these)
    height_in_inches <- 8
    width_in_inches <- 12
    
    # Save as a PNG
    png(filename, width = width_in_inches * 100, height = height_in_inches * 100)
    grid.draw(grid_table)
    dev.off()
  }
  
  layout_df2_react <- reactive({
    # Get the dataframe from the existing reactive expression
    layout_df <- layout_df_react()
    
    # Apply the gsub function to each element of the dataframe
    layout_df2 <- as.data.frame(lapply(layout_df, function(x) gsub("<br/>", "\n", x)))
    
    
    colnames(layout_df2) = c(1:12," ")
    layout_df2 <- cbind(Row = LETTERS[1:8], layout_df2)
    # Return the modified dataframe
    layout_df2
  })# Call the save_table_as_image function
  
  output$download_malex_layout <- downloadHandler(
    filename = function() {
      # Extract the user input values
      name <- paste0(input$malex_name_input, input$malex_surname_input)
      id <- input$malex_id_input
      # Remove any spaces from these values
      name <- gsub(" ", "", name)
      id <- gsub(" ", "", id)
      
      # Construct the filename
      paste0("MALEXSetup_", name, "_", format(Sys.Date(), "%d%b%Y"), "_MALEX", id, ".png")
    },
    content = function(file) {
      # Call the save_table_as_image function with the reactive layout data
      save_table_as_image(layout_df2_react(), file)
    }
  )
  
  
  
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      # Extract the user input values
      name <- paste0(input$malex_name_input, input$malex_surname_input)

      malex <- input$malex_id_input
      # Remove any spaces from these values
      name <- gsub(" ", "", name)
      malex <- gsub(" ", "", malex)
      
      paste("MALEXSetup_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
    },
    
    content = function(file) {
      # Update empty FieldID to be the same as LabID
      updated_samples_data <- samples_data()%>% 
        filter(Specimen_Type!="Empty")

      empty_field_ids <- updated_samples_data$FieldID == ""
      updated_samples_data$FieldID[empty_field_ids] <- updated_samples_data$LabID[empty_field_ids]
      
      # Write custom header rows
      header_info <- c(
        paste("Name:", input$dna_extraction_name_input),
        paste("MALEX:", input$malex_input),
        paste("Date:", format(Sys.Date(), "%d%b%Y")),
        "",
        paste(colnames(updated_samples_data), collapse = ",")
      )
      
      writeLines(header_info, file)
      
      # Append the updated_samples_data to the same file without column names since the headers are custom
      write.table(updated_samples_data, file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",", quote = TRUE)
      
      # add file to drive_folder
      # Extract the user input values
      name <- paste0(input$malex_name_input, input$malex_surname_input)
      
      malex <- input$malex_id_input
      # Remove any spaces from these values
      name <- gsub(" ", "", name)
      malex <- gsub(" ", "", malex)
      
      
      filename_upload = paste("MALEXSetup_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
      
      
      drive_folder <- drive_get(as_id("1muTascwjSUoB6Vip5IoDQoESMKVg-4pj"))
      drive_upload(file, path = drive_folder, name = filename_upload)
      
      
      shinyalert::shinyalert(title = "Success!", text = "Upload successful", type = "success")
    }
  )
  
  
}