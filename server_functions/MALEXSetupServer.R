MALEXSetupServer <- function(input,output,session){
  
  malex_canDownload = reactiveVal(FALSE)
  
  malex_table_generated <- reactiveVal(FALSE)
  malex_layout_generated <- reactiveVal(FALSE)
  malex_numRows <- reactiveVal(0)
  malex_samples_data <- reactiveVal(data.frame(Position = character(), LabID = character(), FieldID = character()))
  
  
  observeEvent(input$malex_add_sample_group, {
    malex_numRows(malex_numRows() + 1)
    newRowID <- as.character(malex_numRows())
    rowID <- paste0("sample_row_", newRowID)
    
    newUI <- div(id = rowID,
                 p(paste("Sample Group ",as.numeric(newRowID)+1)),
                 malex_generateSampleGroupRowUI(newRowID),
                 tags$hr()
    )
    insertUI(
      selector = "#sample_entry_area",
      where = "beforeEnd",
      ui = newUI
    )
  })
  
  
  observeEvent(input$malex_remove_sample_group, {
    if (malex_numRows() > 0) {
      lastRowID <- paste0("sample_row_", as.character(malex_numRows()))
      removeUI(selector = paste0("#", lastRowID))
      malex_numRows(malex_numRows() - 1)
    }
  })
  
  
  malex_generate_table_function <- function() {
    sequences_list <- list()
    warnings <- character(0)
    
    get_input <- function(prefix, i) input[[paste0(prefix, i)]]
    
    for (i in 0:malex_numRows()) {
      inputs <- lapply(c("prefix_input_", "starting_number_input_", "number_of_samples_input_"), get_input, i)
      
      if(all(!sapply(inputs, is.null))) {
        list_inputs <- lapply(c("study_input_malex_", "content_input_", "received_barcodes_"), get_input, i)
        study_code <- ifelse(list_inputs[[1]] == "Other", get_input("other_study_input_malex_", i), list_inputs[[1]])
        content_type <- ifelse(list_inputs[[2]] == "Other", get_input("other_content_input_", i), list_inputs[[2]])
        barcodes <- if (!is.null(list_inputs[[3]])) readLines(list_inputs[[3]]$datapath)[-1] else rep("", inputs[[3]])
        
        sequences_list[[i + 1]] <- list(LabID = malex_generate_samples_seq(inputs[[1]], inputs[[2]], inputs[[3]]), Study_Code = rep(study_code, inputs[[3]]), Specimen_Type = rep(content_type, inputs[[3]]), FieldID = barcodes)
      }
    }
    
    total_samples <- sum(sapply(sequences_list, function(x) length(x$LabID)))
    if (total_samples > 96) return(showModal(modalDialog(title = "Warning", "Only 96 samples allowed.", easyClose = TRUE)))
    
    samples_df <- do.call(rbind, lapply(sequences_list, as.data.frame)) %>%
      mutate(
        Well = rep(LETTERS[1:8], times = 12)[1:n()],
        Column = sprintf('%02d', rep(1:12, each = 8))[1:n()],
        Position = paste0(Well, Column)
      ) %>% 
      select(Position,Study_Code,Specimen_Type,LabID,FieldID)
    
    repeated_ids <- function(column_name) samples_df[[column_name]][duplicated(samples_df[[column_name]]) & samples_df[[column_name]]!="" & samples_df[["Specimen_Type"]]!="Empty"]
    
    for (field in c("LabID", "FieldID")) {
      repeated_field_values = repeated_ids(field)
      if(length(repeated_field_values) > 0) {
        warnings <- c(warnings, paste("Repeated ", field, " detected:", paste(repeated_field_values, collapse = ", ")))
      }
    }
    
    if(length(warnings) > 0) showModal(modalDialog(title = "Warnings", HTML(paste(na.omit(warnings), collapse = "<br>")), easyClose = TRUE))
    
    skip_block <- any(sapply(0:malex_numRows(), function(i) !is.null(get_input("received_barcodes_", i))))
    
    if (!skip_block) {
      previous_samples <- malex_samples_data()
      matching_rows <- (samples_df$Position %in% previous_samples$Position[previous_samples$FieldID != ""]) 
      samples_df$FieldID[matching_rows] <- previous_samples$FieldID[match(samples_df$Position[matching_rows], previous_samples$Position)]
    }
    
    samples_df <- transform(samples_df, FieldID = toupper(FieldID), LabID = toupper(LabID))
    malex_samples_data(samples_df)
    
    output$malex_samples_table <- renderRHandsontable({ if (!is.null(df <- malex_samples_data() %>% filter(Specimen_Type != "Empty"))) rhandsontable(df, rowHeaders = FALSE) })
    malex_table_generated(TRUE)
    output$generate_layout_button <- renderUI({ if (malex_table_generated()) actionButton("generate_layout", "Generate layout") })
  }
  
  
  observeEvent(input$malex_generate_table, {
    malex_generate_table_function()
    output$malex_table_input_ui<- renderUI({
      tagList(
        p("Use this table to manually add or scan FieldIDs"),
        rHandsontableOutput("malex_samples_table",height = "200px"),
        tags$hr()
      )
    })
  })
  
  output$malex_table_generated <- reactive({ malex_table_generated() })
  
  observe({
    if (!is.null(input$malex_samples_table)) {
      # Capture the current state of the table after user edits
      new_data <- hot_to_r(input$malex_samples_table)
      new_data$FieldID = toupper(new_data$FieldID)
      # Update the reactive value
      malex_samples_data(new_data)
    }
  })
  
  layout_df_react = reactiveVal()
  
  observeEvent(input$generate_layout, {
    
    # Update empty FieldID to be the same as LabID
    updated_samples_data <- malex_samples_data()%>% 
      filter(Specimen_Type!="Empty")
    
    missing_field_ids <- setdiff(updated_samples_data$FieldID, database_data[["Receiving"]]$FieldID)
    
    if (length(missing_field_ids) != 0) {
      # If the condition is not met, show a warning and prevent the download
      shinyalert::shinyalert("Warning", paste0("Field IDs not found \n",paste(missing_field_ids, collapse = ", "),"\n Please quickly receive samples before proceeding."), type = "warning")
      malex_canDownload(FALSE)
    } else {
      malex_canDownload(TRUE)
    }
    
    
    
    repeated_ids <- function(column_name) updated_samples_data[[column_name]][duplicated(updated_samples_data[[column_name]]) & updated_samples_data[[column_name]]!="" & updated_samples_data[["Specimen_Type"]]!="Empty"]
    
    for (field in c("LabID", "FieldID")) {
      repeated_field_values = repeated_ids(field)
      if(length(repeated_field_values) > 0) {
        shinyalert::shinyalert("Warning", paste0(field," repeated: \n",paste(unique(repeated_field_values), collapse = ", ")), type = "warning")
        malex_canDownload(FALSE)
      }else {
        malex_canDownload(TRUE)
      }
    }
    
    
    if(malex_canDownload()){
      
      
      malex_layout_generated(TRUE)
      df <- malex_samples_data() %>% filter(Specimen_Type!="Empty") %>% 
        left_join(database_data[["Receiving"]],by = "FieldID")
      
      # Create an empty layout matrix with 8 rows (A-H) and 12 columns (1-12)
      layout <- matrix("", nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12))
      
      row_idx <- match(substr(df$Position, 1, 1), LETTERS)
      col_idx <- as.integer(substr(df$Position, 2, 3))
      layout[cbind(row_idx, col_idx)] <- paste(df$LabID, df$FieldID, df$Province, df$Country, sep = "<br/>")
      
      label_col <- rep("LabID<br/>FieldID<br/>Province<br/>Country", nrow(layout))
      layout_with_labels <- cbind(layout, label_col)
      
      # Convert the layout matrix to a data frame for rendering
      layout_df <- as.data.frame(layout_with_labels)
      colnames(layout_df)[ncol(layout_df)] <- " "
      
      layout_df_react(layout_df)
      
      
      output$malex_layout_output_ui <- renderUI({
        malex_createTableUI(layout_df) # This function could encapsulate all the repetitive rendering and styling logic
        
      })
    }
  })
  
  
  
  
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
  
  
  observeEvent(input$download_malex_layout_image, {
    name <- paste0(input$malex_name_input, input$malex_surname_input)
    id <- input$malex_id_input
    name <- gsub(" ", "", name)
    id <- gsub(" ", "", id)
    filename = paste0(prefix_files,"MALEXSetup_", name, "_", format(Sys.Date(), "%d%b%Y"), "_MALEX", id, ".png")
    save_table_as_image(layout_df2_react(), filename, path_for_files,input)
  }
  )
  
  
  
  observeEvent(input$submit_malex_report, {
    
    if (malex_canDownload()) {
      
      
      name <- paste0(input$malex_name_input, input$malex_surname_input)
      
      malex <- input$malex_id_input
      # Remove any spaces from these values
      name <- gsub(" ", "", name)
      malex <- gsub(" ", "", malex)
      
      malex_report_filename = paste(prefix_files,"MALEXSetup_", name, "_MALEX", malex, "_", format(Sys.Date(), "%d%b%Y"), ".csv", sep = "")
      
      malex_report_filename_with_path = paste0(path_for_files,"/MALEXSetup/",malex_report_filename)
      
      updated_samples_data <- malex_samples_data()%>% 
        filter(Specimen_Type!="Empty")
      empty_field_ids <- updated_samples_data$FieldID == ""
      updated_samples_data$FieldID[empty_field_ids] <- updated_samples_data$LabID[empty_field_ids]
      
      header_info <- c(
        paste("Name:", paste0(input$malex_name_input, input$malex_surname_input)),
        paste("MALEX:", input$malex_id_input),
        paste("Date:", format(Sys.Date(), "%d%b%Y")),
        "",
        paste(colnames(updated_samples_data), collapse = ",")
      )
      
      
      writeLines(header_info, malex_report_filename_with_path)
      write.table(updated_samples_data, malex_report_filename_with_path, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",", quote = TRUE)
      
      shinyalert::shinyalert(title = "Success!", text = paste0("Image saved successfully \n Location: ",malex_report_filename_with_path), type = "success")
      
      tryCatch({
        drive_folder <- drive_get(as_id("1jxB1o_usTMXV-Bv-auSKEBfd8CeX6PSY"))
        drive_upload(malex_report_filename_with_path, path = drive_folder, name = malex_report_filename)
        shinyalert::shinyalert(title = "Success!", text = "CSV backed up in Google Drive", type = "success")
      }, error = function(e) {
        shinyalert::shinyalert(title = "Warning!", text = "Could not back up CSV in Google Drive", type = "warning")
      })
    }  
  })
  
}