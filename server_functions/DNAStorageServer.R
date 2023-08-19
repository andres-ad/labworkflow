DNAStorageServer <- function(input,output,session){
observe({
  inFile <- input$file1
  
  if (is.null(inFile)) {
    return(NULL)
  }
  
  # Use the custom reader to parse the file
  parsed_data <- read_custom_format(inFile$datapath)
  
  # Access the parsed content using parsed_data$name, parsed_data$malex, etc.
  # For example, if you want to use the actual data:
  dna_data <- parsed_data$data
  print(parsed_data)
  
  # Further processing or display of the dna_data can be added here
})

data <- reactive({
  # If no file uploaded, return NULL
  if(is.null(input$dataFile)) {
    return(NULL)
  }
  
  # Read the uploaded file with your custom function
  read_custom_format(input$dataFile$datapath)
})

output$fileCustomHeader <- renderPrint({
  # If no data is present (no file uploaded), display a message
  if(is.null(data())) {
    return("Please upload a CSV file.")
  }
  
  # Otherwise, display the custom header
  data()$custom_header
})
}