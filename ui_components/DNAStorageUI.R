DNAStorageUI <- function(){
  tabPanel("DNA Storage", 
           fluidRow(
             column(9, h4("Upload DNA extraction file"))#,
             # column(3, 
             #        actionButton("joined_file_button","I have the joined file!")
             # )
           ),
           h6("This is the CSV file created in DNA extraction Setup tab that links LabID to FieldID"),
           fluidRow(
             column(6,fileInput('malex_file_input', 'Choose DNA Extraction Setup File',  
                                accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             ),
             column(6,
                    uiOutput("malexfileCustomHeaderDisplay")
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
                                choices = c("","Active Samples", "Archived Samples"),
                                selected = "")),
             column(2, 
                    selectInput("dna_extraction_shelf_input", label = "Shelf:", choices = 0:5),selected=0),
             column(2,
                    selectInput("dna_extraction_basket_input", label = "Basket:", choices = 0:2),selected=0)
           ),
           tags$hr(),
           h4("Enter Micronic scan output file:"),
           fluidRow(
             column(6,
                    fileInput('micronic_file_input',  'Choose Micronic Scan File', 
                              accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             ),
             column(6,
                    uiOutput("micronicDetailsDisplay")
             )
           ),
           uiOutput("malex_micronic_join_button"),
           uiOutput("joined_layout_output")
           # tags$hr(),
           # uiOutput("download_button_ui"),
           # uiOutput("update_button_ui"),
           # tags$hr()
  )
}