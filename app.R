libraries <- c("shiny", "shinyjs", "rhandsontable", "DT", "writexl", "tidyverse")
lapply(libraries, library, character.only = TRUE)


lapply(list.files("ui_components",pattern="\\.R$",full.names = TRUE),
       source)
lapply(list.files("server_functions",pattern="\\.R$",full.names = TRUE),
       source)
source("custom_functions.R")

##### UI
ui <- fluidPage(
  titlePanel("ARMMOR Lab Sample Tracking"),
  useShinyjs(),
  tabsetPanel(
    sampleReceivingUI(),
    DNAExtractionSetupUI(),
    DNAStorageUI()
  )
)

##### SERVER
server <- function(input, output, session) {
  sampleReceivingServer(input,output,session)
  DNAExtractionSetupServer(input,output,session)
  DNAStorageServer(input,output,session)
}

shinyApp(ui = ui, server = server)
