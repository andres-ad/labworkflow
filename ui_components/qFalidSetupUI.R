qFalidSetupUI <- function(){
  tabPanel("qFALID Setup",
                    fluidRow(
                      column(3,
                             textInput("qfalid_name_input", label = "Name*:", placeholder = "Enter Name")
                      ),
                      column(3,
                             textInput("qfalid_surname_input", label = "Surname*:", placeholder = "Enter Surname")
                      ),
                      column(3,
                             dateInput("qfalid_date_input", label = "Date*:", value = Sys.Date())
                      ),
                      column(3,
                             numericInput("qfalid_id_input",label = "qFALID:", value=global_get_default_qfalid_value(),min=0)
                      )
                    ),
           br(),
           h4("Upload Micronic scan"),
           h6("This should be a scan of the Micronic plate containing the tubes in the same positions as they will go into the qPCR plate."),
           fluidRow(
             column(6, fileInput('qfalid_scan_file', 'Choose File',
                                 accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
             ),
             
             ),
             column(6,
                    uiOutput("qfalidScanDisplay_or_warning") # Combining scan display and warning
             )
           ),
           tags$hr(),
           uiOutput("get_data_button"),
           uiOutput("warning_message"),
           uiOutput("layout_button")
  )
}
