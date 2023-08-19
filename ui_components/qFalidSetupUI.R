qFalidSetupUI <- function(){
  tabPanel("qFalid",
           h4("Upload Micronic scan"),
           h6("This should be a scan of the Micronic plate containing the tubes in the same positions as they will go into the qPCR plate."),
           fluidRow(
             column(6,fileInput('qFalid_scan_file', 'Choose qFalid Micronics Scan File',  
                                accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             ),
             column(6,
                    uiOutput("qFalidScanDisplay")
             )
           ),
           tags$hr()
  )
}
