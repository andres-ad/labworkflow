library(lubridate)
read_custom_format <- function(file_path) {
  # Read the custom header
  custom_header <- readLines(file_path, n = 5)
  
  # Extract relevant info if needed (can be expanded upon)
  name <- gsub("Name:", "", custom_header[1])
  malex <- gsub("MALEX:", "", custom_header[2])
  country <- gsub("Country:", "", custom_header[3])
  province <- gsub("Province:", "", custom_header[4])
  date <- gsub("Date:", "", custom_header[5])
  
  # Read the actual data, skipping the first 6 lines
  data <- read.csv(file_path, skip = 6, header = TRUE)
  
  list(
    name = str_trim(name),
    malex = str_trim(malex),
    country = str_trim(country),
    province = str_trim(province),
    date = str_trim(date),
    data = data
  )
}





DNAStorageServer <- function(input, output, session) {
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    # Use the custom reader to parse the file
    parsed_data <- read_custom_format(inFile$datapath)
    dna_data <- parsed_data$data
  })
  
  data <- reactive({
    # If no file uploaded, return NULL
    if (is.null(input$file1)) {
      return(NULL)
    }
    # Read the uploaded file with your custom function
    read_custom_format(input$file1$datapath)
  })
  
  output$fileCustomHeaderDisplay <- renderUI({
    # If no data is present (no file uploaded), return a message
    if(is.null(data())) {
      return(HTML("Please upload a CSV file."))
    }
    
    # Otherwise, display the custom header in a formatted way
    custom_header <- data()
    
    HTML(paste0(
      "Name: ", custom_header$name,"<br/>",
      "MALEX: ", custom_header$malex,"<br/>",
      "Country: ", custom_header$country,"<br/>",
      "Province: ", custom_header$province,"<br/>",
      "Date: ", custom_header$date,"<br/>"
    ))
  })
  
  
  
  micronic_data <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
    }
    # Read the Micronic CSV file
    data <- read.csv(inFile$datapath, header = TRUE)
    return(data)
  })
  
  output$micronicDetailsDisplay <- renderUI({
    data <- micronic_data()
    if (is.null(data) || nrow(data) < 1) {
      return(NULL)
    }
    # Extract the values from the first row
    rack_id <- data$Rack.ID[1]
    
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