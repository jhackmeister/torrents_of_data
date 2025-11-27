# Build URL
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=statviewtype&stat=conference&type=standings&season_id=7&key=446521baf8c38984&client_code=pwhl"


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
data_list

season_teams <- data_list$SiteKit$Statviewtype