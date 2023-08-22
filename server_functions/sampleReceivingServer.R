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
  
  sample_receiving_barcodes_reactive <- reactiveVal(0)
  
  
  input_barcodes = reactive({
    toupper(input$sample_receiving_barcode_input)
  })
  
  # Upon clicking on "Get a count of scanned barcodes" display count
  observeEvent(input$sample_receiving_process_barcodes, {
    if (!is.null(input_barcodes())) {
      barcodes <- unlist(strsplit(input_barcodes(), "\n"))
      num_non_empty_samples <- sum(nchar(trimws(barcodes)) > 0)
      sample_receiving_barcodes_reactive(num_non_empty_samples)
    }
  })
  
  
  
  # Output the count of barcodes into the text box
  
  output$sample_receiving_total_samples_text <- renderText({
    paste("Total samples scanned:", sample_receiving_barcodes_reactive())
  })
  
  
  report_data <- reactiveVal()
  
  # Create a function to make filenames
  sample_receiving_generate_filename <- function() {
    name_cleaned <- paste0(gsub("[^A-Za-z0-9]", "", input$sample_receiving_name_input),
                           gsub("[^A-Za-z0-9]", "", input$sample_receiving_surname_input))
    date_cleaned <- gsub("[^A-Za-z0-9]", "", format(input$sample_receiving_date_input, "%Y%m%d"))
    time_cleaned <- gsub("[^0-9]", "", format(Sys.time(), "%H%M"))
    generated_filename <- paste0("receiving_", name_cleaned, "_", date_cleaned, "_", time_cleaned, ".txt")
    return(generated_filename)
  }
  
  
  # Event to generate the report and download it
  output$sample_receiving_generate_report_button <- downloadHandler(
    filename = sample_receiving_generate_filename,
    content = function(file) {
      # If Other, get other function
      get_input_or_other <- function(input_val, other_input_val) {
        ifelse(input_val == "Other", other_input_val, input_val)
      }
      
      # extract inputs
      
      study <- get_input_or_other(input$sample_receiving_study_input, input$sample_receiving_other_study_input)
      country <- get_input_or_other(input$sample_receiving_country_input, input$sample_receiving_other_country_input)
      province <- get_input_or_other(input$sample_receiving_province_input, input$sample_receiving_other_province_input)
      name <- paste0(input$sample_receiving_name_input, input$sample_receiving_surname_input)
      datereport <- format(input$sample_receiving_date_input, "%d%b%Y")
      
      report <- paste(
        "Name:", name,
        "Date:", datereport,
        "Study:", study,
        "Country:", country,
        "Province:", province,
        "Barcodes:", input_barcodes(),
        sep = "\n"
      )
      report_data(list(report = report, file = file, name = sample_receiving_generate_filename()))
      
      report_df <- data.frame(Study = study, Country = country, Province = province, Barcode = unlist(strsplit(input_barcodes(), "\n")), Name = name, Date = datereport)
      
      
      # Append to the Google sheet: 
      
      worksheet_name <- "Receiving"
      ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
      ss <- googlesheets4::gs4_get(ss_url)
      sheet_data <- googlesheets4::read_sheet(ss,sheet = worksheet_name)
      
      existing_barcodes <- sheet_data$Barcode # Adjust column name as needed
      duplicate_barcodes <- which(report_df$Barcode %in% existing_barcodes)
      
      # Add warning if there are codes that already exist in the database
      if (length(duplicate_barcodes)>0) {
        duplicate_rows <- sheet_data[sheet_data$Barcode %in% report_df$Barcode[duplicate_barcodes],]
        duplicates_message <- "Some barcodes already exist. Here are the details:\n\n"
        for (i in 1:nrow(duplicate_rows)) {
          duplicates_message <- paste(
            duplicates_message,
            paste(names(duplicate_rows), duplicate_rows[i, ], sep = ": ", collapse = " | "),
            sep = "\n"
          )
        }
        duplicates_message <- paste(duplicates_message, "Proceed?", sep = "\n\n")
        
        # If there are duplicate barcodes, show a warning
        shinyalert::shinyalert(
          title = "Warning!",
          text =  duplicates_message,
          type = "warning",
          showCancelButton = TRUE,
          callbackJS = "function(isConfirm) { if(isConfirm) Shiny.setInputValue('proceedWithDuplicates', true); }"
        )
        # Observe when the user chooses to proceed and call the function
        observeEvent(input$proceedWithDuplicates, {
          process_report(report_df,worksheet_name,ss)
        })
      } else {
        process_report(report_df,worksheet_name,ss)  }
      
    }
    
  )
  
  # Define a function to process the report data
  process_report <- function(report_df,worksheet_name,ss) {
    data <- report_data()
    
    sheet_data <- googlesheets4::read_sheet(ss,sheet = worksheet_name)
    last_row <- nrow(sheet_data) + 2
    target_range <- paste0("A", last_row, ":F", last_row + nrow(report_df))
    
    writeLines(data$report, con = data$file)
    drive_folder <- drive_get(as_id("1WnrshcBiA_ZOeNGCjOI4RSZSpbKDZHNP"))
    drive_upload(data$file, path = drive_folder, name = data$name)
    googlesheets4::range_write(ss, report_df, range = target_range, col_names = FALSE, sheet = worksheet_name)
    
    shinyalert::shinyalert(title = "Success!", text = "Upload to database Successful", type = "success")
  }
  
  
}