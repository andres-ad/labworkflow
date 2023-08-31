sampleReceivingServer <- function(input,output,session){
  
  output$sample_receiving_province_ui <- renderUI({
    province_input <- selectInput("sample_receiving_province_input", label = "Province:",
                                  choices = c(global_provinces_names[[input$sample_receiving_country_input]],
                                              "Other"))
    
    other_province_input <- conditionalPanel(
      condition = 'input.sample_receiving_province_input == "Other"',
      textInput("sample_receiving_other_province_input", label = "Other Province:", placeholder = "Enter province")
    )
    
    tagList(province_input, other_province_input)
  })
  
  

  # Make a reactive variable to store the number of barcodes when scanned
  
  sample_receiving_barcodes_reactive <- reactiveVal(NULL)
  
  
  sample_receiving_input_barcodes = reactive({
    toupper(input$sample_receiving_barcode_input)
  })
  
  observe({
    barcodes <- input$sample_receiving_barcode_input
    if (!is.null(barcodes)) {
      barcodes <- unlist(strsplit(barcodes, "\n"))
      duplicates <- barcodes[duplicated(barcodes)]
      if (length(duplicates) > 0) {
        shinyalert::shinyalert(
          title = "Warning!",
          text = paste("These barcodes are repeated:", paste(unique(duplicates), collapse = ", "), ". Please ensure each barcode is unique."),
          type = "warning"
        )
      }
    }
  })
  
  
  # Upon clicking on "Get a count of scanned barcodes" display count
  observeEvent(input$sample_receiving_process_barcodes, {
    if (!is.null(sample_receiving_input_barcodes())) {
      barcodes <- unlist(strsplit(sample_receiving_input_barcodes(), "\n"))
      num_non_empty_samples <- sum(nchar(trimws(barcodes)) > 0)
      sample_receiving_barcodes_reactive(num_non_empty_samples)
    }
  })
  
  
  
  # Output the count of barcodes into the text box
  
  output$sample_receiving_total_samples_text <- renderText({
    paste("Total samples scanned:", sample_receiving_barcodes_reactive())
  })
  
  
  # Create a function to make filenames
  sample_receiving_generate_filename <- function() {
    name_cleaned <- paste0(gsub("[^A-Za-z0-9]", "", input$sample_receiving_name_input),
                           gsub("[^A-Za-z0-9]", "", input$sample_receiving_surname_input))
    date_cleaned <- gsub("[^A-Za-z0-9]", "", format(input$sample_receiving_date_input, "%Y%m%d"))
    time_cleaned <- gsub("[^0-9]", "", format(Sys.time(), "%H%M"))
    country <- get_input_or_other(input$sample_receiving_country_input, input$sample_receiving_other_country_input)
    province <- get_input_or_other(input$sample_receiving_province_input, input$sample_receiving_other_province_input)
    country_cleaned = gsub("[^A-Za-z0-9]", "",country)
    province_cleaned = gsub("[^A-Za-z0-9]", "",province)
    rec_cleaned = paste0("REV",input$sample_receiving_REV_input)
    
    generated_filename <- paste0("receiving_", name_cleaned, "_",country_cleaned,"_",province_cleaned,"_",rec_cleaned,"_", date_cleaned, "_", time_cleaned, ".txt")
    return(generated_filename)
  }
  
  process_barcode_report <- function(){
    barcodes <- unlist(strsplit(sample_receiving_input_barcodes(), "\n"))
    # Check for duplicates
    duplicates <- barcodes[duplicated(barcodes)]
    
    
    rec = paste0("REV", input$sample_receiving_REV_input)
    study <- get_input_or_other(input$sample_receiving_study_input, input$sample_receiving_other_study_input)
    country <- get_input_or_other(input$sample_receiving_country_input, input$sample_receiving_other_country_input)
    province <- get_input_or_other(input$sample_receiving_province_input, input$sample_receiving_other_province_input)
    name <- paste0(input$sample_receiving_name_input, input$sample_receiving_surname_input)
    datereport <- format(input$sample_receiving_date_input, "%d%b%Y")
    
    # 1. Write the txt file
    report <- paste(
      "REV:", rec,
      "Name:", name,
      "Date:", datereport,
      "Study:", study,
      "Country:", country,
      "Province:", province,
      "FieldIDs:", sample_receiving_input_barcodes(),
      sep = "\n"
    )
    filename <- sample_receiving_generate_filename()
    
    write(report, file = paste0(path_for_files,"/Receiving/",filename))
    
    # 2. Try to upload to Google Drive
    tryCatch({
      drive_folder <- drive_get(as_id("1WnrshcBiA_ZOeNGCjOI4RSZSpbKDZHNP"))
      drive_upload(paste0(path_for_files,"/Receiving/",filename), path = drive_folder, name = filename)
    }, error = function(e) {
      warning("Could not connect to internet, txt report was not uploaded to Google Drive, only a local copy was made, which you should have saved")
    })
    
    # 3. Make a dataframe and update the database
    report_df <- data.frame(REV = rec, Study = study, Country = country, Province = province, FieldID = barcodes, Name = name, Date = datereport)
    local_database_updated = database_data
    local_database_updated[["Receiving"]] = rbind(database_data[["Receiving"]], report_df)
    database_data = update_database(local_database_updated, local_database_path, "Receiving", google_sheet_url)
  }
  
  
  observeEvent(input$sample_receiving_generate_report_button, {
    barcodes <- unlist(strsplit(sample_receiving_input_barcodes(), "\n"))
    database_barcodes = database_data[["Receiving"]]$FieldID
    # Check for duplicates
    duplicates <- barcodes[barcodes %in% database_barcodes]
    # If there are duplicates, show a confirmation dialog
    if (length(duplicates) > 0) {
      shinyalert::shinyalert(
        title = "Warning!",
        text = paste("These barcodes already exist in the database:", paste(unique(duplicates), collapse = ", "), ". Do you want to proceed? Note that only the last entry will be used to link to receiving information"),
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Proceed",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x) { # If the user clicks "Proceed"
            process_barcode_report()
          }
        }
      )
    } else {
      process_barcode_report()
    }
  })
  
}
  
  