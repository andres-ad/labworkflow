# load needed libraries

libraries <- c("shiny", "shinyjs", "rhandsontable", 
               "DT", "writexl", "tidyverse",
               "googlesheets4","gargle",
               "googledrive","shinyalert")
lapply(libraries, library, character.only = TRUE)

# source functions for UI and server in their respective folders
lapply(list.files("ui_components",pattern="\\.R$",full.names = TRUE),
       source)
lapply(list.files("server_functions",pattern="\\.R$",full.names = TRUE),
       source)

# source the global variablesvariables 
source("global_variables.R")


# Define the UI as a set of tabs
ui <- fluidPage(
  tags$style(HTML("
    /* Adjust hr height and margin */
    hr {
      height: 1px; /* Adjust height as needed */
      margin: 5px 0; /* Adjust margin as needed */
      border: none;
      background-color: #ccc; /* Adjust color as needed */
    }
  ")),
  titlePanel("ARMMOR Lab Workflow"),
  useShinyjs(),
  tabsetPanel(
    sampleReceivingUI(),
    DNAExtractionSetupUI(),
    DNAStorageUI(),
    qFalidSetupUI(),
    qFalidResultUI(),
    madhatSetupUI(),
    poolSetupUI(),
    superpoolSetupUI(),
    madhatResultsUI()
  )
)

# Define the server that is broken into each of the tabs
server <- function(input, output, session) {
  sampleReceivingServer(input,output,session)
  DNAExtractionSetupServer(input,output,session)
  DNAStorageServer(input,output,session)
  qFalidSetupServer(input,output,session)
  qFalidResultServer(input,output,session)
}

shinyApp(ui = ui, server = server)
