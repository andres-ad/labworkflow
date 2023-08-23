global_study_codes = c("GenE8","BF_SMC","Test")

global_provinces_names = list("South Africa" =c("KwaZuluNatal","Mpumalanga","Limpopo"),
                       "Angola" = "Placeholder",
                       "Burkina Faso" = "None",
                       "Eswatini" = "Placeholder",
                       "Namibia" = "Placeholder",
                       "Zambia" = "Placeholder",
                       "Lab" = "Controls")

global_country_names = names(global_provinces_names)





global_get_default_malex_value <- function() {
  tryCatch(
    {
      DNAStorage_sheet_data <- read_sheet(
        gs4_get("https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"),
        sheet = "DNAStorage")
      
      1 + suppressWarnings(max(as.numeric(gsub("MALEX", "", DNAStorage_sheet_data$MALEX)), na.rm = TRUE))
    },
    error = function(e) {
      warning("Could not connect to the Internet")
      0
    }
  )
}


global_get_default_rec_value <- function() {
  tryCatch(
    {
      DNAStorage_sheet_data <- read_sheet(
        gs4_get("https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"),
        sheet = "Receiving")
      
      1 + suppressWarnings(max(as.numeric(gsub("REV", "", DNAStorage_sheet_data$REV)), na.rm = TRUE))
    },
    error = function(e) {
      warning("Could not connect to the Internet")
      0
    }
  )
}
