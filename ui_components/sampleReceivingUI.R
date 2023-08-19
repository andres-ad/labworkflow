sampleReceivingUI <- function(){
  tabPanel("Sample receiving", 
           h2("Sample Receiving Content"),
           br(),
           textInput("name_input", label = "Name:", placeholder = "Enter name"),
           dateInput("date_input", label = "Date:", value = Sys.Date()),
           selectInput("country_input", label = "Country:", 
                       choices = c("Angola", "Eswatini", "Namibia", "South Africa", "Zambia", "Other"),
                       selected = "Angola"),
           conditionalPanel(
             condition = 'input.country_input == "Other"',
             textInput("other_country_input", label = "Other Country:", placeholder = "Enter country")
           ),
           textInput("province_input", label = "Province(s):", placeholder = "Enter province(s)"),
           br(),
           p("Scan all received barcodes here:"),
           tags$textarea(id = "barcode_input", placeholder = "Enter barcodes here", rows = 5, cols = 40),
           br(),
           actionButton("process_barcodes", "Get a count of scanned barcodes"),
           br(),
           verbatimTextOutput("total_samples_text"),
           br(),
           downloadButton("generate_report_button", "Generate Report")
  )
}