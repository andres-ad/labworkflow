

Sys.setenv(GARGLE_OAUTH_APP = "Lab Database")
Sys.setenv(GARGLE_OAUTH_ID = "2870958191-0ifiv5rs06v9iiggamdso58vvls8793h.apps.googleusercontent.com")
Sys.setenv(GARGLE_OAUTH_SECRET =  "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")


library(googlesheets4)
googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

