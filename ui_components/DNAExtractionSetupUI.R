DNAExtractionSetupUI <- function(){
  tabPanel("MALEX Setup", 
           fluidRow(
             column(3,
                    textInput("malex_name_input", label = "Name*:", placeholder = "Enter Name")
             ),
             column(3,
                    textInput("malex_surname_input", label = "Surname*:", placeholder = "Enter Surname")
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
           p("If you use receiving scans to fill in the table and you make a mistake you will have to refresh and start again"),
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
                        selectInput("study_input_DNAExt_0", 
                                    label = "Study:", 
                                    choices = c(global_study_codes,"Other"),
                                    selected = "GenE8"),
                        selectInput("content_input_0", 
                                    label = "Content:", 
                                    choices = c("DNA(DBS)","Empty","DNA(RDT)","Other"),
                                    selected = "DNA(DBS)")),
                 column(2,class = "print-input-row",
                        conditionalPanel(
                          condition = 'input.study_input_DNAExt_0 == "Other"',
                          textInput("other_study_input_DNAExt_0",
                                    label = "Other study",
                                    placeholder = "Enter Study"
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.content_input_0 == "Other"',
                          textInput("other_content_input_0",
                                    label = "Other content",
                                    placeholder = "Enter Content"
                          )
                        )),
                 column(3,class = "print-input-row", 
                        textInput("prefix_input_0", 
                                  label = "LabID Prefix:", 
                                  placeholder = "Enter prefix"),
                        numericInput("starting_number_input_0", 
                                     label = "Start LabID#:", 
                                     value = 0, 
                                     min = 0)),
                 column(3,
                        numericInput("number_of_samples_input_0", 
                                     label = "# Samples:", 
                                     value = 0, 
                                     min = 0),
                        fileInput('received_barcodes_0','Barcodes file if available')
                 )
               ),
               tags$hr()     
           ),
           p("Use this table to manually add or scan FieldIDs"),
           rHandsontableOutput("samples_output",height = "200px"),
           tags$hr(),
           uiOutput("layout_output_ui")
           
  )
}