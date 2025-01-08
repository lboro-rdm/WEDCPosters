library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)


# Read collection IDs and titles
collection_data <- read.csv("collection_ids.csv", stringsAsFactors = FALSE)
collection_ids <- collection_data$collection_id
collection_titles <- collection_data$collection_title

# Initialize a data frame to store results
article_details <- data.frame(
  collection_title = character(),
  article_id = character(),
  title = character(),
  stringsAsFactors = FALSE
)

# Base URL for the API
base_url <- "https://api.figshare.com/v2"

# Function to fetch articles from a collection
fetch_articles_from_collection <- function(collection_id, collection_title) {
  articles_url <- paste0(base_url, "/collections/", collection_id, "/articles?page_size=1000")
  articles_response <- GET(articles_url)
  
  if (status_code(articles_response) != 200) {
    message("Failed to fetch articles for collection: ", collection_title)
    return(NULL)
  }
  
  articles <- fromJSON(content(articles_response, as = "text"))
  
  if (length(articles) == 0) {
    message("No articles found for collection: ", collection_title)
    return(NULL)
  }
  
  # Extract article IDs and titles
  data.frame(
    collection_title = collection_title,
    article_id = as.character(articles$id),  # Ensure article_id is a character
    title = articles$title,
    stringsAsFactors = FALSE
  )
}

# Iterate through each collection and fetch articles
for (i in seq_along(collection_ids)) {
  collection_id <- collection_ids[i]
  collection_title <- collection_titles[i]
  message("Fetching articles for collection: ", collection_title)
  
  # Fetch articles for the current collection
  collection_articles <- fetch_articles_from_collection(collection_id, collection_title)
  
  # If articles were fetched successfully, bind them to the main data frame
  if (!is.null(collection_articles)) {
    article_details <- bind_rows(article_details, collection_articles)
  }
}

# Set the Figshare API request URL for articles
endpoint2 <- "https://api.figshare.com/v2/articles/"

# Initialize a data frame to store citation data
combined_df <- data.frame(
  collection_title = character(),
  article_id = character(),
  title = character(),
  Author = character(),
  Year = character(),
  hdl = character(),
  doi = character(),
  sort = character(),  # Renamed column
  stringsAsFactors = FALSE
)

# Iterate through article IDs to get article citation data
for (i in 1:nrow(article_details)) {
  print(i)
  article_id <- article_details$article_id[i]
  full_url_citation <- paste0(endpoint2, article_id)
  
  # Get the article citation data
  response <- GET(full_url_citation)
  if (http_status(response)$category != "Success") {
    warning("Failed to fetch data for article ID: ", article_id)
    next
  }
  
  citation_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  
  # Extract authors, year, handle, DOI, and tags with appropriate checks
  Author <- if (!is.null(citation_data$authors) && nrow(citation_data$authors) > 0) {
    paste(citation_data$authors$full_name, collapse = ", ")
  } else {
    NA
  }
  
  year <- if (!is.null(citation_data$published_date)) {
    year(as.Date(citation_data$published_date))
  } else {
    NA
  }
  
  hdl <- if (!is.null(citation_data$handle)) {
    citation_data$handle
  } else {
    NA
  }
  
  doi <- if (!is.null(citation_data$doi)) {
    citation_data$doi
  } else {
    NA
  }
  
  sort <- if (!is.null(citation_data$tags)) {
    # Filter tags to keep only those starting with "WEDC"
    filtered_tags <- citation_data$tags[grepl("^WEDC", citation_data$tags)]
    if (length(filtered_tags) > 0) {
      # Extract the portion of the tag after "WEDC"
      sub("^WEDC", "", filtered_tags[1])  # Take the first matching tag and remove "WEDC"
    } else {
      ""  # Blank if no "WEDC" tags exist
    }
  } else {
    ""
  }
  
  # Append the citation data to the combined data frame
  combined_df <- rbind(combined_df, data.frame(
    collection_title = article_details$collection_title[i],
    article_id = article_id,
    title = article_details$title[i],
    Author = Author,
    Year = year,
    hdl = hdl,
    doi = doi,
    sort = sort,
    stringsAsFactors = FALSE
  ))
}

# Save the final dataset to a CSV file
output_file <- "combined_data.csv"
write.csv(combined_df, file = output_file, row.names = FALSE)
