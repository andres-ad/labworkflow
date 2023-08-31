MALEXSetupUI <- function(){
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
             column(3,actionButton("malex_add_sample_group", "Add Sample Group")),
             column(3,actionButton("malex_remove_sample_group", "Remove Sample Group")),
             column(3,actionButton("malex_generate_table", "Generate table")),
             column(3,uiOutput("generate_layout_button"))
           ),
           br(),
           div(id = "sample_entry_area",
               p("Sample Group 1"),
               malex_generateSampleGroupRowUI(0),
               tags$hr()     
           ),
           uiOutput("malex_table_input_ui"),
           uiOutput("malex_layout_output_ui")
           
  )
}