qFalidSetupUI <- function(){
  tabPanel("QFALID Setup",
           h4("QFALID Information:"),
           fluidRow(
             column(6, 
                    textInput("qFalid_name_input", label = "Name:", 
                              placeholder = "Enter name")),
             column(6,
                    textInput("qfalid_input", label = "qFALID code:", 
                              placeholder = "Enter number"))
           ),
           br(),
           h4("Upload Micronic scan"),
           h6("This should be a scan of the Micronic plate containing the tubes in the same positions as they will go into the qPCR plate."),
           fluidRow(
             column(6, fileInput('qFalid_scan_file', 'Choose QFALID Setup File',
                                 accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
             ),
             
             ),
             column(6,
                    uiOutput("qFalidScanDisplay_or_warning") # Combining scan display and warning
             )
           ),
           tags$hr(),
           uiOutput("get_data_button"),
           uiOutput("warning_message"),
           DTOutput("layout_table"),
           tags$hr(),
           uiOutput("qfalid_export_button_ui"),
           tags$hr()
  )
}
