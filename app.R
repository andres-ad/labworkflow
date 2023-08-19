libraries <- c("shiny", "shinyjs", "rhandsontable", 
               "DT", "writexl", "tidyverse",
               "googlesheets4","gargle")
lapply(libraries, library, character.only = TRUE)


lapply(list.files("ui_components",pattern="\\.R$",full.names = TRUE),
       source)
lapply(list.files("server_functions",pattern="\\.R$",full.names = TRUE),
       source)

##### UI
ui <- fluidPage(
  titlePanel("ARMMOR Lab Workflow"),
  useShinyjs(),
  tabsetPanel(
    sampleReceivingUI(),
    DNAExtractionSetupUI(),
    DNAStorageUI(),
    qFalidSetupUI()
  )
)

##### SERVER
server <- function(input, output, session) {
  sampleReceivingServer(input,output,session)
  DNAExtractionSetupServer(input,output,session)
  DNAStorageServer(input,output,session)
  qFalidSetupServer(input,output,session)
}

shinyApp(ui = ui, server = server)
