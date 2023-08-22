DNAExtractionSetupUI <- function(){
  tabPanel("MALEX Setup", 
           fluidRow(
             column(3,
                    textInput("malex_name_input", label = "Name*:", placeholder = "Enter Name")
             ),
             column(3,
                    textInput("malex_suname_input", label = "Surname*:", placeholder = "Enter Surname")
             ),
             column(3,
                    dateInput("malex_date_input", label = "Date*:", value = Sys.Date())
             ),
             column(3,
                    numericInput("malex_id_input",label = "MALEX:", value=global_get_default_malex_value(),min=0)
             )
           ),
           tags$hr(),
           h4("Lab ID assignments"),
           br(),
            fluidRow(
             column(3,actionButton("add_sample_group", "Add Sample Group")),
             column(3,actionButton("remove_sample_group", "Remove Sample Group")),
             column(3,actionButton("submit_samples", "Generate table")),
             column(3,uiOutput("generate_layout_button"))
           ),
           br(),
           div(id = "sample_entry_area",
               p("Sample Group 1"),
               fluidRow(
                 column(3,class = "print-input-row",
                        selectInput("study_input_DNAExt", 
                                    label = "Study:", 
                                    choices = c(global_study_codes,"Other"),
                                    selected = "GenE8"),
                        selectInput("content_input", 
                                    label = "Content:", 
                                    choices = c("DNA(DBS)","DNA(RDT)","Other"),
                                    selected = "DNA(DBS)")),
                 column(2,class = "print-input-row",
                        conditionalPanel(
                          condition = 'input.study_input_DNAExt == "Other"',
                          textInput("other_study_input_DNAExt",
                                    label = "Other study",
                                    placeholder = "Enter Study"
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.content_input == "Other"',
                          textInput("other_content_input",
                                    label = "Other content",
                                    placeholder = "Enter Content"
                          )
                        )),
                 column(3,class = "print-input-row", 
                        textInput("prefix_input", 
                                  label = "LabID Prefix:", 
                                  placeholder = "Enter prefix"),
                        numericInput("starting_number_input", 
                                     label = "Start LabID#:", 
                                     value = 0, 
                                     min = 0)),
                 column(3,
                        numericInput("number_of_samples_input", 
                                     label = "# Samples:", 
                                     value = 0, 
                                     min = 0),
                        fileInput('received_barcodes','Barcodes file if available'))
               ),
               tags$hr()     
           ),
           p("Use this table to manually add or scan FieldIDs"),
           rHandsontableOutput("samples_output",height = "200px"),
           tags$hr(),
           uiOutput("layout_output"),
           downloadButton('downloadData', 'Export table'),
           actionButton("reset_malex_setup", "Reset")
  )
}