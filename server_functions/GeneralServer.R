GeneralServer <- function(input, output, session) {
  observeEvent(input$auth, {
    # Capture the console output
    console_output <- capture.output({
      # Replace this with your googlesheets4 authentication code
      cat("The googlesheets4 package is requesting access to your Google account.\n")
    })
    
    # Check if the authentication message is present
    if (grepl("The googlesheets4 package is requesting access", console_output)) {
      # Show a warning to the user
      showModal(modalDialog(
        title = "Authentication Required",
        "Please check the R console for authentication instructions.",
        easyClose = TRUE
      ))
    }
  })
}