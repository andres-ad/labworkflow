DNAStorageUI <- function(){
  tabPanel("DNA Storage", 
           h4("Matching Field and Lab ID's with Micronic barcodes"),
           # Add more UI elements as needed
           
           fileInput('file1', 'Choose DNA Extraction Setup File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
           #verbatimTextOutput('fileCustomHeader'),
           
           fluidRow(
             column(6, 
                    textInput("dna_extraction_plate_name_input", label = "Plate Name:", 
                              placeholder = "Enter plate name")),
             column(6,
                    selectInput("dna_extraction_freezer_input", label = "Freezer:", 
                                choices = c("Active Samples", "Archived Samples"),
                                selected = "Active Samples"))
           ),
           fluidRow(
             column(6, 
                    selectInput("dna_extraction_shelf_input", label = "Shelf:", choices = 1:4)),
             column(6,
                    selectInput("dna_extraction_basket_input", label = "Basket:", choices = 1:2))
           )
  )
}