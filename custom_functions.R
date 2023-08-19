
read_custom_format <- function(file_path) {
  # Read the custom header
  custom_header <- readLines(file_path, n = 5)
  
  # Extract relevant info if needed (can be expanded upon)
  name <- gsub("Name:", "", custom_header[1])
  malex <- gsub("MALEX:", "", custom_header[2])
  country <- gsub("Country:", "", custom_header[3])
  province <- gsub("Province:", "", custom_header[4])
  date <- gsub("Date:", "", custom_header[5])
  
  # Read the actual data, skipping the first 6 lines
  data <- read.csv(file_path, skip = 6, header = TRUE)
  
  list(
    name = str_trim(name),
    malex = str_trim(malex),
    country = str_trim(country),
    province = str_trim(province),
    date = str_trim(date),
    data = data
  )
}

