
google_sheet_urls <- list(
  testing = "https://docs.google.com/spreadsheets/d/1A0d3CuMQrZqMVknMuVqmx2uhYr2mJIzuP1k1cawxGqs",
  production = "https://docs.google.com/spreadsheets/d/143S5AmwM1OZ-1vbUSNmj8jRUcLQS8LQvDbjvgFauc4s"
)

google_sheet_url <- google_sheet_urls[[environment]]


initialize_app <- function(google_sheet_url,local_cache_path) {

  # Check for Internet connection by attempting to access Google Sheet
  online <- FALSE
  tryCatch({
    gs4_get(google_sheet_url)
    online <- TRUE
  }, error = function(e) {
    warning("Could not connect to the Internet. Working offline.")
  })
  
  # If online, synchronize local cache with Google Sheet
  if (online) {
    # Logic to sync the Google Sheet with the local cache
    # ...
  }
  
  # Load data from the local cache into the app
  local_data <- read_xlsx(local_cache_path) # 
  
  return(local_data)
}