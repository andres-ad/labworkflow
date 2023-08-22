sampleReceivingUI <- function(){
  tabPanel("Sample receiving", 
           h4("Sample Receiving"),
           h4("Please enter one province/country at a time"),
           br(),
           fluidRow(
             column(4,
                    textInput("sample_receiving_name_input", label = "Name*:", placeholder = "Enter Name")
             ),
             column(4,
                    textInput("sample_receiving_surname_input", label = "Surname*:", placeholder = "Enter Surname")
             ),
             column(4,
                    dateInput("sample_receiving_date_input", label = "Date*:", value = Sys.Date())
             )
           ),
           fluidRow(
             column(3,
                    numericInput("sample_receiving_REC_input", label = "REC*:", value=global_get_default_rec_value(),min=0)
                    ),
             column(3,
                    selectInput("sample_receiving_study_input", label = "Study*:", 
                                choices = c(global_study_codes, "Other"),
                                selected = "GenE8"),
                    conditionalPanel(
                      condition = 'input.sample_receiving_study_input == "Other"',
                      textInput("sample_receiving_other_study_input", label = "Other Study:", placeholder = "Enter study")
                    )
             ),
             column(3,
                    selectInput("sample_receiving_country_input", label = "Country*:", 
                                choices = c(global_country_names, "Other"),
                                selected = "South Africa"),
                    conditionalPanel(
                      # This condition checks if "Other" is in the selected options
                      condition = 'input.sample_receiving_country_input =="Other"',
                      textInput("sample_receiving_other_country_input", label = "Other Country:", placeholder = "Enter country")
                    )
             ),
             column(3,
                    uiOutput("sample_receiving_province_ui")
             )
           ),
           
           br(),
           p("Scan or manually enter all received barcodes here (one barcode per line, lowercase will be processed as uppercase)"),
           p("If you don't want to scan or enter these during DNA extraction, scan only a batch of samples that you will extract together"),
           tags$textarea(id = "sample_receiving_barcode_input", placeholder = "Enter barcodes here", rows = 5, cols = 40),
           br(),
           actionButton("sample_receiving_process_barcodes", "Get a count of scanned barcodes"),
           br(),
           verbatimTextOutput("sample_receiving_total_samples_text"),
           br(),
           downloadButton("sample_receiving_generate_report_button", "Generate Report")
  )
}