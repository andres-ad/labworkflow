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
  
  
  # Event to generate the report and download it
  output$generate_report_button <- downloadHandler(
    filename = function() {
      name_cleaned <- gsub("[^A-Za-z0-9]", "", input$name_input)
      date_cleaned <- gsub("[^A-Za-z0-9]", "", format(input$date_input, "%Y%m%d"))
      time_cleaned <- gsub("[^0-9]", "", format(Sys.time(), "%H%M"))
      filename <- paste0("receiving_", name_cleaned, "_", date_cleaned, "_", time_cleaned, ".txt")
      return(filename)
    },
    content = function(file) {
      report <- paste(
        "Name:", input$name_input,
        "Date:", format(input$date_input, "%Y-%m-%d"),
        "Country:", ifelse(input$country_input == "Other", input$other_country_input, input$country_input),
        "Province(s):", input$province_input,
        "Barcodes:", input$barcode_input,
        sep = "\n"
      )
      writeLines(report, con = file)
    }
  )
}