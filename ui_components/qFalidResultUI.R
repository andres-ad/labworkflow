qFalidResultUI <- function(){
  tabPanel("qFALID Results",
           h4("qFALID Information:"),
           
           fluidRow(
             column(4, 
                    textInput("qfalid_results_name", label = "Name:", 
                              placeholder = "Enter name")),
             column(4,
                    numericInput("qfalid_results_qfalidid", label = "qFALID:", 
                                 value = 0,min=1))
           ),
           fluidRow(
             column(6,
                    fileInput("qfalid_results_file_upload", "Upload qFALID export", accept = ".csv")
             ),
             column(6,
                    uiOutput("qfalid_results_display_merge_button")
                    
             ),
           ),
           tags$hr(),
           fluidRow(
             column(6,
                    uiOutput("qfalid_results_display_upload_button")
             )
           ),
           actionButton("qfalid_results_display_reshape_button","Reshape database")
  )
}
