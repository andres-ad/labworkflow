sampleReceivingUI <- function(){
  tabPanel("Sample receiving", 
           h4("Sample Receiving"),
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
             column(4,
                    selectInput("sample_receiving_study_input", label = "Study*:", 
                                choices = c(study_codes, "Other"),
                                selected = "GenE8"),
                    conditionalPanel(
                      condition = 'input.sample_receiving_study_input == "Other"',
                      textInput("sample_receiving_other_study_input", label = "Other Study:", placeholder = "Enter study")
                    )
             ),
             column(4,
                    
                    selectInput("sample_receiving_country_input", label = "Country*:", 
                                choices = c(country_names, "Other"),
                                selected = "South Africa",
                                multiple=TRUE),
                    conditionalPanel(
                      # This condition checks if "Other" is in the selected options
                      condition = '$.inArray("Other", input.sample_receiving_country_input) > -1',
                      textInput("sample_receiving_other_country_input", label = "Other Country:", placeholder = "Enter country")
                    )
             ),
             column(4,
                    textInput("sample_receiving_province_input", label = "Province(s):", placeholder = "Enter province(s)")
             )
           ),
           
           br(),
           p("Scan all received barcodes here (one barcode per line, lowercase will be processed as uppercase):"),
           uiOutput("country_textareas"),
           br(),
           actionButton("sample_receiving_process_barcodes", "Get a count of scanned barcodes"),
           br(),
           verbatimTextOutput("sample_receiving_total_samples_text"),
           br(),
           downloadButton("sample_receiving_generate_report_button", "Generate Report")
  )
}