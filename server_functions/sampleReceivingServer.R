sampleReceivingServer <- function(input,output,session){
  
  barcodes_reactive <- reactiveVal(NULL)
  
  observeEvent(input$process_barcodes, {
    barcode_text <- input$barcode_input
    if (!is.null(barcode_text)) {
      barcodes <- unlist(strsplit(barcode_text, "\n"))
      num_non_empty_samples <- sum(nchar(trimws(barcodes)) > 0)
      barcodes_reactive(num_non_empty_samples)
    }
  })
  
  output$total_samples_text <- renderText({
    paste("Total samples scanned:", barcodes_reactive())
  })
  
  
  report_data <- reactiveVal()
  
  # Event to generate the report and download it
  output$generate_report_button <- downloadHandler(
    filename = function() {
      name_cleaned <- gsub("[^A-Za-z0-9]", "", input$name_input)
      date_cleaned <- gsub("[^A-Za-z0-9]", "", format(input$date_input, "%Y%m%d"))
      time_cleaned <- gsub("[^0-9]", "", format(Sys.time(), "%H%M"))
      generated_filename <- paste0("receiving_", name_cleaned, "_", date_cleaned, "_", time_cleaned, ".txt")
      return(generated_filename)
    },
    content = function(file) {
      name_cleaned <- gsub("[^A-Za-z0-9]", "", input$name_input)
      date_cleaned <- gsub("[^A-Za-z0-9]", "", format(input$date_input, "%Y%m%d"))
      time_cleaned <- gsub("[^0-9]", "", format(Sys.time(), "%H%M"))
      generated_filename <- paste0("receiving_", name_cleaned, "_", date_cleaned, "_", time_cleaned, ".txt")
      report <- paste(
        "Name:", input$name_input,
        "Date:", format(input$date_input, "%d%b%Y"),
        "Study:", ifelse(input$study_input == "Other", input$other_study_input, input$study_input),
        "Country:", ifelse(input$country_input == "Other", input$other_country_input, input$country_input),
        "Province(s):", input$province_input,
        "Barcodes:", input$barcode_input,
        sep = "\n"
      )
      report_data(list(report = report, file = file, name = generated_filename))
      
      report_df = data.frame(Study = ifelse(input$study_input == "Other", input$other_study_input, input$study_input),
                             Country = ifelse(input$country_input == "Other", input$other_country_input, input$country_input),
                             Barcode = strsplit(input$barcode_input,split = "\n")[[1]],
                             Name = input$name_input,
                             Date = format(input$date_input, "%d%b%Y"))
      
      
      #append to the Google sheet: 
      worksheet_name <- "Receiving"
      ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
      ss <- googlesheets4::gs4_get(ss_url)
      sheet_data <- googlesheets4::read_sheet(ss,sheet = worksheet_name)
      last_row <- nrow(sheet_data) + 2
      target_range <- paste0("A", last_row, ":E", last_row + nrow(report_df))
      
      
      existing_barcodes <- sheet_data$Barcode # Adjust column name as needed
      duplicate_barcodes <- which(report_df$Barcode %in% existing_barcodes)
      
      
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
          process_report(report_df)
        })
      } else {
        process_report(report_df)  }
      
    }
    
  )
  
  # Define a function to process the report data
  process_report <- function(report_df) {
    data <- report_data()
    
    worksheet_name <- "Receiving"
    ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
    ss <- googlesheets4::gs4_get(ss_url)
    sheet_data <- googlesheets4::read_sheet(ss,sheet = worksheet_name)
    last_row <- nrow(sheet_data) + 2
    target_range <- paste0("A", last_row, ":E", last_row + nrow(report_df))
    
    
    writeLines(data$report, con = data$file)
    drive_folder <- drive_get(as_id("1WnrshcBiA_ZOeNGCjOI4RSZSpbKDZHNP"))
    drive_upload(data$file, path = drive_folder, name = data$name)
    googlesheets4::range_write(ss, report_df, range = target_range, col_names = FALSE, sheet = worksheet_name)
  }
  
  
}