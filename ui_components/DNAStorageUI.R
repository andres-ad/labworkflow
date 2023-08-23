DNAStorageUI <- function(){
  tabPanel("DNA Storage", 
           fluidRow(
             column(9, h4("Upload DNA extraction file")),
             column(3, 
                    actionButton("joined_file_button","I have the joined file!")
             )
           ),
           h6("This is the CSV file created in DNA extraction Setup tab that links LabID to FieldID"),
           fluidRow(
             column(6,fileInput('file1', 'Choose DNA Extraction Setup File',  
                                accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             ),
             column(6,
                    uiOutput("fileCustomHeaderDisplay")
             )
           ),
           tags$hr(),
           h4("Enter Micronic rack information:"),
           fluidRow(
             column(4, 
                    textInput("dna_extraction_plate_name_input", label = "Plate Name:", 
                              placeholder = "Enter rack name")),
             column(4,
                    selectInput("dna_extraction_freezer_input", label = "Freezer:", 
                                choices = c("Active Samples", "Archived Samples"),
                                selected = "Active Samples")),
             column(2, 
                    selectInput("dna_extraction_shelf_input", label = "Shelf:", choices = 1:5)),
             column(2,
                    selectInput("dna_extraction_basket_input", label = "Basket:", choices = 1:2))
           ),
           tags$hr(),
           h4("Enter Micronic scan output file:"),
           fluidRow(
             column(6,
                    fileInput('file2',  'Choose Micronic Scan File', 
                              accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             ),
             column(6,
                    uiOutput("micronicDetailsDisplay")
             )
           ),
           uiOutput("join_button"),
           uiOutput("upload_built_file"),
           tags$hr(),
           uiOutput("joined_layout_container"),
           tags$hr(),
           uiOutput("download_button_ui"),
           uiOutput("update_button_ui"),
           tags$hr()
  )
}