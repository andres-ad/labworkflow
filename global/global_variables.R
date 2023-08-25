global_study_codes = c("GenE8","BF_SMC","Test")

global_provinces_names = list("South Africa" =c("KwaZuluNatal","Mpumalanga","Limpopo"),
                       "Angola" = "Placeholder",
                       "Burkina Faso" = "None",
                       "Eswatini" = "Placeholder",
                       "Namibia" = "Placeholder",
                       "Zambia" = "Placeholder",
                       "Lab" = "Controls")

global_country_names = names(global_provinces_names)



global_get_default_value <- function(sheet_name, pattern) {
  tryCatch(
    {
      sheet_data <- read_sheet(
        gs4_get("https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"),
        sheet = sheet_name)
      
      1 + suppressWarnings(max(as.numeric(gsub(pattern, "", sheet_data[[pattern]])), na.rm = TRUE))
    },
    error = function(e) {
      warning("Could not connect to the Internet")
      0
    }
  )
}

global_get_default_malex_value <- function() {
  global_get_default_value("DNAStorage", "MALEX")
}

global_get_default_qfalid_value <- function() {
  global_get_default_value("qFALIDResults", "qFALID")
}

global_get_default_rec_value <- function() {
  global_get_default_value("Receiving", "REV")
}

