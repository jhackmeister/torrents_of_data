get_pwhl_game_data <- function(game_id) {
  # Build URL
  url <- paste0("https://lscluster.hockeytech.com/feed/index.php?feed=gc&tab=pxpverbose&game_id=", 
                game_id, 
                "&key=446521baf8c38984&client_code=pwhl")
  
  # Make the GET request
  response <- GET(url)
  
  # Get the raw text
  data_text <- content(response, "text", encoding = "UTF-8")
  
  # Check if it's JSONP (wrapped in parentheses or a function call)
  if (grepl("^\\w+\\(", data_text)) {
    data_text <- sub("^\\w+\\(", "", data_text)
    data_text <- sub("\\);?$", "", data_text)
  } else if (grepl("^\\(", data_text)) {
    data_text <- sub("^\\(", "", data_text)
    data_text <- sub("\\)$", "", data_text)
  }
  
  # Parse JSON and extract Pxpverbose
  data_list <- fromJSON(data_text, flatten = TRUE)
  df <- data_list$GC$Pxpverbose
  
  return(df)
}

# Use the function
df_game333 <- get_pwhl_game_data(333)
