DNAExtractionSetupUI <- function(){
  tabPanel("MALEX Setup", 
           h4("DNA Extraction Information:"),
           fluidRow(
             column(3, 
                    textInput("dna_extraction_name_input", label = "Name:", 
                              placeholder = "Enter name")),
             column(3,
                    textInput("malex_input", label = "MALEX code:", 
                              placeholder = "Enter number")),
             column(3, 
                    textInput("dna_extraction_country_input", label = "Country(s):", 
                              placeholder = "Enter countries")),
             column(3,
                    textInput("dna_extraction_province_input", label = "Province(s):", 
                              placeholder = "Enter provinces"))
           ),
           tags$hr(),
           h4("Sample Entry"),
           div(id = "sample_entry_area",
               fluidRow(
                 column(2,class = "print-input-row",selectInput("study_input_DNAExt", label = "Study:", 
                                                                choices = c("GenE8","Controls","Other"),
                                                                selected = "GenE8")),
                 column(2,class = "print-input-row", textInput("prefix_input", label = "Prefix:", placeholder = "Enter prefix")),
                 column(2,class = "print-input-row", numericInput("starting_number_input", label = "Start#:", value = 0, min = 0)),
                 column(2,class = "print-input-row", numericInput("number_of_samples_input", label = "#:", value = 1, min = 1)),
                 column(2,class = "print-input-row",selectInput("content_input", label = "Content:", 
                                                                choices = c("DNA(DBS)","DNA(RDT)","Other"),
                                                                selected = "DNA(DBS)")),
                 # Inside your fluidRow:
                 column(2,class = "print-input-row",conditionalPanel(
                   condition = 'input.study_input_DNAExt == "Other"',
                   textInput("other_study_input_DNAExt",
                             label = "Study:",
                             placeholder = "Enter content"
                   )
                 )
                 ),
                 column(2,class = "print-input-row",conditionalPanel(
                   condition = 'input.content_input == "Other"',
                   textInput("other_content_input",
                             label = "Content:",
                             placeholder = "Enter content"
                   )
                 )
                 )
               )
           ),
           br(),
           div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;", 
               actionButton("add_sample_group", "Add Sample Group"),
               actionButton("remove_sample_group", "Remove Sample Group"),
               actionButton("submit_samples", "Generate table"),
               uiOutput("generate_layout_button")
           ),
           fileInput('received_barcodes', 'I have my barcodes already! (upload before you click Generate table)',  
                     accept = c('text', '.txt')),
           rHandsontableOutput("samples_output",height = "200px"),
           tags$hr(),
           uiOutput("layout_output"),
           downloadButton('downloadData', 'Export table'),
           actionButton("reset_malex_setup", "Reset")
  )
}