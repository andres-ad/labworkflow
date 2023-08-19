qFalidSetupServer <- function(input,output,session){
  
  qfalid_scan <- reactive({
    inFile <- input$qFalid_scan_file
    if (is.null(inFile)) {
      return(NULL)
    }
    # Read the Micronic CSV file
    data <- read.csv(inFile$datapath, header = TRUE,check.names = FALSE)
    return(data)
  })
  
  output$qFalidScanDisplay <- renderUI({
    data <- qfalid_scan()
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
  
  
  
}