qFalidResultServer <- function(input, output, session) {
  qfalid_results_showMergeButton <- reactiveVal(FALSE)
  
  qPCRdatainfile = reactiveVal(NULL)
  qpcr_data_toupload_react = reactiveVal(NULL)
  qfalid_results_showUploadButton <- reactiveVal(FALSE)
  
  observeEvent(input$qfalid_results_file_upload, {
    req(input$qfalid_results_file_upload)
    
    file_path <- input$qfalid_results_file_upload$datapath
    raw_data <- read.csv(file_path, skip = 0, header = FALSE)
    
    
    required_values <- c("Well", "Fluor", "Target", "Content", "Sample", "Cq", "Starting Quantity (SQ)")
    
    matching_row <- which(apply(raw_data, 1, function(row) all(row == required_values)))
    
    if (length(matching_row) == 0) {
      # No matching row found
    } else {
      matched_row_index <- matching_row[1]
      matched_row <- a[matched_row_index, ]
      # Process the matched row as needed
    }
    
    # Check if the Well row is found
    if (length(matching_row) == 0) {
      showModal(modalDialog(
        title = "Warning",
        "File is not correctly formatted. No row with required headers found.",
        easyClose = TRUE
      ))
      # Resetting the file input
      return()
    }
    
    # Show the merge and show data buttons
    qfalid_results_showMergeButton(TRUE)
    qPCRdatainfile(read.csv(file_path, skip = matched_row_index-1, header = TRUE))
  })
  
  output$qfalid_results_display_merge_button <- renderUI({
    if (qfalid_results_showMergeButton()) {
      actionButton("qfalid_results_merge_with_database", "Match with Database")
    }
  })
  
  observeEvent(input$qfalid_results_merge_with_database, {
    ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
    ss <- googlesheets4::gs4_get(ss_url)
    sheet_data <- googlesheets4::read_sheet(ss, "DNAStorage") %>%
      mutate_all(as.character)
    
    
    qpcr_data = qPCRdatainfile() %>% 
      select(Well,Sample,Content,Cq,'Starting.Quantity..SQ.') %>% 
      rename(Well=Well,GenomicID = Sample,LabIDqPCR = Content,Cq=Cq,Parasitemia='Starting.Quantity..SQ.') %>% 
      mutate_all(as.character) %>%
      left_join(sheet_data %>% select(Study,FieldID,LabID,GenomicID),by="GenomicID") %>% 
      mutate(Parasitemia=round(as.numeric(Parasitemia),digits=2),
             Cq=round(as.numeric(Cq),digits=2)) 
    
    nomatches = qpcr_data %>% 
      filter(LabIDqPCR!="Std")%>% 
      filter(!GenomicID %in% sheet_data$GenomicID)
    
    # display a warning that these ID don't exist and they won't be uploaded
    
    mismatches =qpcr_data %>% 
      filter(LabIDqPCR!="Std")%>% 
      filter(LabIDqPCR != LabID)
    
    # display warning that LabIDs don't match and that the one in the database will be kept
    
    qpcr_data_toupload = qpcr_data %>% 
      filter(LabIDqPCR!="Std",
             Study!="Controls",
             GenomicID %in% sheet_data$GenomicID)
    
    qpcr_data_toupload_react(qpcr_data_toupload)
    qfalid_results_showUploadButton(TRUE)
  })
  
  output$qfalid_results_display_upload_button <- renderUI({
    if (qfalid_results_showUploadButton()) {
      actionButton("qfalid_results_upload_to_database", "Upload data to database")
    }
  })
  
  qfalid_results_upload_to_database_triggered <- reactiveVal(FALSE)
  
  observeEvent(input$qfalid_results_upload_to_database, {
    ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
    ss <- googlesheets4::gs4_get(ss_url)
    sheet_data <- googlesheets4::read_sheet(ss, "qFALIDResults") %>%
      mutate_all(as.character)
    
    print(sheet_data)
    qpcr_data_toupload = qpcr_data_toupload_react()
    
    existing_ids = qpcr_data_toupload %>% 
      filter(GenomicID %in% sheet_data$GenomicID)
    new_ids =   qpcr_data_toupload %>% 
      filter(!GenomicID %in% sheet_data$GenomicID)
    
    # add warning
    
    qpcr_data_toupload_toadd = qpcr_data_toupload %>% 
      mutate(User = input$qfalid_results_name,
             qFALID = input$qfalid_results_qfalidid,
             Date = format(Sys.Date(),"%d%b%Y")) %>% 
      select(FieldID,LabID,GenomicID,
             User,qFALID,Date,Cq,Parasitemia)
    
    last_row <- nrow(sheet_data) + 2
    target_range <- paste0("A", last_row, ":H", last_row + nrow(qpcr_data_toupload_toadd))
    googlesheets4::range_write(ss, qpcr_data_toupload_toadd, range = target_range, col_names = FALSE,sheet="qFALIDResults")
    qfalid_results_upload_to_database_triggered(TRUE)
  })
  

  observeEvent(input$qfalid_results_display_reshape_button,{
    ss_url <- "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
    ss <- googlesheets4::gs4_get(ss_url)
    sheet_data <- googlesheets4::read_sheet(ss, "qFALIDResults") %>%
      group_by(GenomicID) %>%
      mutate(Repeat = row_number(),n=n()) 
    
    data_with_morethan2 = sheet_data %>% 
      filter(n>2)
    data_with_1or2 = sheet_data %>% 
      filter(n<=2)
    
    data_with_morethan2 <- data_with_morethan2 %>%
      group_by(GenomicID) %>%
      arrange(GenomicID, Parasitemia) %>%
      mutate(diff_with_next = abs(lead(Parasitemia) - Parasitemia),
             diff_with_prev = abs(lag(Parasitemia) - Parasitemia),
             mi = min(abs(diff_with_next),na.rm = T)) %>% 
      filter(diff_with_next==mi | diff_with_prev == mi) %>% 
      group_by(GenomicID,LabID,FieldID) %>% 
      summarize("Average Parasitemia" = mean(Parasitemia))
    
    data_with_1or2 <- data_with_1or2 %>%
      group_by(GenomicID,LabID,FieldID) %>% 
      summarize("Average Parasitemia" = mean(Parasitemia))
    
    data_av = rbind(data_with_morethan2,data_with_1or2)
    
   
    sheet_data = sheet_data %>% 
      ungroup() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(cols = c(User,qFALID,Date,Cq,Parasitemia),
                   names_to = "Field",
                   values_to = "Value") %>% 
      mutate(FieldRepeat = paste0(Field,"_",Repeat)) %>% 
      select(-Field,-Repeat)%>% 
      pivot_wider(
        id_cols = c(GenomicID, LabID, FieldID),
        values_from = Value,
        names_from = FieldRepeat) %>% 
      left_join(data_av,by=c("GenomicID","LabID","FieldID"))
      
    sheet_data = sheet_data[,c(1:3,ncol(sheet_data),4:ncol(sheet_data)-1)]
    
    googlesheets4::sheet_write(sheet_data,ss,sheet = "qFALIDResultsWide")
    
    
  })
  
}
