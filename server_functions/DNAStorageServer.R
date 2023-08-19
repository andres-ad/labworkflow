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
  
  joinButton = reactiveVal(FALSE)
  downloadActive <- reactiveVal(FALSE)
  joined_data_output <- reactiveValues(joinedData=NULL)
  
  dna_data_noheader <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Read the entire file as lines
    all_lines <- readLines(inFile$datapath)
    # Find the index of the header
    header_row_index <- which(grepl("Position.*LabID.*FieldID", all_lines))
    
    # Check if the header was found
    if (length(header_row_index) > 0) {
      # Construct a text connection to read from the specific line onwards
      data_txt <- paste(all_lines[header_row_index:length(all_lines)], collapse = "\n")
      actual_data <- read.csv(textConnection(data_txt), header = TRUE)
      return(actual_data)
    } else {
      return(NULL)
    }
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
    data <- read.csv(inFile$datapath, header = TRUE,check.names = FALSE)
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
  
  
  
  output$join_button <- renderUI({
    if (!is.null(micronic_data()) & !is.null(dna_data_noheader())) {
      actionButton("join_button", "Join Data")
    }
  })

  observeEvent(input$join_button, {
    joinButton(TRUE)
    downloadActive(TRUE)

    joined_data <- reactive({
      if (is.null(dna_data_noheader()) || is.null(micronic_data())) {
        return(NULL)
      }
      dna_subset <- dna_data_noheader()[c("Position", "LabID", "FieldID")]
      micronic_subset <- micronic_data()[, c("Tube Position", "Tube ID")]
      colnames(micronic_subset) = c("TubePosition","TubeID")
      right_join(dna_subset, micronic_subset, by = c("Position" = "TubePosition"))
    })

    # Initialize an empty layout with row names A-H and column names 1-12
    joined_layout <- matrix(NA, nrow=8, ncol=12)
    rownames(joined_layout) <- LETTERS[1:8]
    colnames(joined_layout) <- as.character(1:12)

    # Fetch the current dataframe
    joined_df <- joined_data()
    joined_data_output$joinedData <- joined_data()


    # Loop through each row of the dataframe
    for(i in 1:nrow(joined_df)){
      # Extract row and column info from the Position column (e.g., "E02")
      row_index <- which(LETTERS == substr(joined_df$Position[i], 1, 1))
      col_index <- as.integer(substr(joined_df$Position[i], 2, 3))

      # Assign LabID and FieldID to the correct position in the layout matrix
      combined_joined_info <- paste(joined_df$LabID[i], joined_df$FieldID[i], joined_df$TubeID[i], sep = "<br/>")  # Use <br/> for line break
      joined_layout[row_index, col_index] <- combined_joined_info
    }

    output$joined_layout_container <- renderUI({
      if (joinButton()) {
        DTOutput("joined_layout_output")
      }
    })

    # Render this table to the output
    output$joined_layout_output <- renderDT({
        datatable(joined_layout,
                  escape = FALSE,  # to allow HTML content in cells
                  options = list(
                    columnDefs = list(
                      list(targets = "_all", orderable = FALSE, className = "dt-center"),  # Disable sorting and center content
                      list(targets = "_all", className = "dt-head-center")  # Center headers
                    ),
                    pageLength = -1,  # Display all rows
                    dom = 't',  # Just the table (no other controls)
                    autoWidth = TRUE,  # Auto adjust column width,
                    redraw=TRUE
                  ),
                  rownames = TRUE
        ) %>%
          formatStyle(columns = 0,
                      fontWeight = 'bold',
                      fontSize = '10px',
                      padding = '1px 1px'
          ) %>%  # Make row names bold and set font size to 10
          formatStyle(columns = 1:ncol(joined_layout),
                      borderRight = '1px solid black',
                      borderBottom = '1px solid black',
                      fontSize = '10px',
                      padding = '1px 1px'  # Set font size to 10 for cell contents
          )
    })
  })
  output$download_button_ui <- renderUI({
    if (downloadActive()) {
      tags$div(
        downloadButton("download_joined_data", "Download CSV"),
        style = "text-align: center;"  # CSS to center the content within the div
      )
    }
  })
  
  output$download_joined_data <- downloadHandler(
    filename = function() {
      paste("joined_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(joined_data_output$joinedData, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
}