malex_generateSampleGroupRowUI <- function(rowID) {
  fluidRow(
    column(3, tags$label(NULL),
           selectInput(paste0("study_input_malex_", rowID), label = "Study:", choices = c(global_study_codes,"Other"), selected = "GenE8"),
           selectInput(paste0("content_input_", rowID), label = "Content", choices = c("DNA(DBS)","Empty","DNA(RDT)","Other"), selected = "DNA(DBS)")),
    column(2, tags$label(NULL),
           conditionalPanel(condition = paste0('input.study_input_malex_', rowID, ' == "Other"'),
                            textInput(paste0("other_study_input_malex_", rowID), label = "Other Study", placeholder = "Enter Study")
           ),
           conditionalPanel(condition = paste0('input.content_input_', rowID, ' == "Other"'),
                            textInput(paste0("other_content_input_", rowID), label = "Other Content:", placeholder = "Enter Content")
           )),
    column(3, tags$label(NULL),
           textInput(paste0("prefix_input_", rowID), label = "LabID Prefix:",  placeholder = "Enter prefix"),
           numericInput(paste0("starting_number_input_", rowID), label = "Start LabID#:", value = 0, min = 0)),
    column(3, tags$label(NULL),
           numericInput(paste0("number_of_samples_input_", rowID), label = "# Samples", value = 0, min = 0),
           fileInput(paste0('received_barcodes_', rowID),'Barcodes file if available'))
  )
}


malex_generate_samples_seq <- function(prefix, start, num) {
  # If starting number is 0, just return the prefix
  if (start == 0) {
    return(rep(prefix, num))
  }
  
  # Original logic for generating LabID
  end <- start + num - 1
  sapply(seq(start, end), function(x) paste0(prefix, sprintf("%03d", x)))
}




malex_createTableUI <- function(layout_df) {
  tagList(
    actionButton("download_malex_layout_image", "Download Layout Image"),
    actionButton('submit_malex_report', 'Submit data'),
    renderDT({
      datatable(layout_df, escape = FALSE, options = list(
        columnDefs = list(
          list(targets = "_all", orderable = FALSE, className = "dt-center"),
          list(targets = "_all", className = "dt-head-center")
        ),
        pageLength = -1,
        dom = 't',
        autoWidth = TRUE
      )) %>%
        formatStyle(0, fontWeight = 'bold', fontSize = '10px', padding = '1px 1px') %>%
        formatStyle(1:(ncol(layout_df) - 1), borderRight = '1px solid black', borderBottom = '1px solid black', fontSize = '10px', padding = '1px 1px') %>%
        formatStyle(ncol(layout_df), borderRight = 'none', borderBottom = 'none', fontSize = '10px', padding = '1px 1px')
    })
  )
}




save_table_as_image <- function(layout_df, filename, path_for_files,input) {
  # Define the grid table
  grid_table <- tableGrob(layout_df,
                          rows = NULL, # Hide row names
                          theme = ttheme_default(
                            core = list(fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10)), # Center text and adjust font size
                            colhead = list(fg_params = list(hjust = 0.5, x = 0.5, fontsize = 12)) # Center headers and adjust font size
                          ))
  
  header <- textGrob(paste0("Name:", input$malex_name_input," ",input$malex_surname_input,"\n",
                            "MALEX:", input$malex_id_input,"\n",
                            "Date:",input$malex_date_input),
                     gp = gpar(fontsize = 16))
  
  
  height_in_inches <- max(10, nrow(layout_df) * 0.7*1.2) # Adjusted values
  width_in_inches <- max(14, ncol(layout_df) * 1.2*1.2)   #
  
  
  # Save as a PNG
  png(paste0(path_for_files,"/MALEXSetup/LayoutImages/",filename), width = width_in_inches * 100, height = height_in_inches * 100, res = 100)
  
  grid.arrange(header, grid_table, ncol = 1, heights = c(1,4))
  
  dev.off()
  shinyalert::shinyalert(title = "Success!", text = paste0("Image saved successfully \n Location: ", path_for_files,"/MALEXSetup/LayoutImages/",filename), type = "success")
  
  
  tryCatch({
    drive_folder <- drive_get(as_id("1w8BUJgWUa-O2LuflgsTstOqXDVXo4mcB"))
    drive_upload(paste0(path_for_files,"/MALEXSetup/LayoutImages/",filename), path = drive_folder, name = filename)
    shinyalert::shinyalert(title = "Success!", text = "Image backed up in Google Drive", type = "success")
  }, error = function(e) {
    shinyalert::shinyalert(title = "Warning!", text = "Could not back up image in Google Drive", type = "warning")
  })
  
  
}


