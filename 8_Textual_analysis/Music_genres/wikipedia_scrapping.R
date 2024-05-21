library(rvest)
library(dplyr)

original_dataset <- read.csv("~/Universitat/4rt Quatri/PMAAD_git/PMAAD_GIA/8_Textual_analysis/Music_genres/spotify-top-200-dataset.csv", sep=";")


genre_list <- strsplit(as.character(original_dataset$artist_genres), ",\\s*")

# Unlisting the list into a single vector
all_genres <- unlist(genre_list)

# Removing empty strings (in case there are any)
all_genres <- all_genres[all_genres != ""]

freq_table <- table(all_genres)

freq_df <- as.data.frame(freq_table)

# Sort the data frame by frequency
sorted_freq_df <- freq_df[order(-freq_df$Freq), ]

# Print the sorted frequency table
print(sorted_freq_df)

# Getting the unique genres
unique_genres <- unique(all_genres)

# Sorting the unique genres (optional)
unique_genres <- sort(unique_genres)
unique_genres

url <- "https://en.wikipedia.org/wiki/List_of_music_genres_and_styles"

# Read the HTML content of the page
page <- read_html(url)

# Extract the links to individual music genres
genre_links <- page %>%
  html_nodes("div.div-col li a") %>%
  html_attr("href") %>%
  na.omit() %>%
  unique()

print(genre_links)


extract_genre_name <- function(url) {
  # Remove the "/wiki/" prefix
  genre <- sub("/wiki/", "", url)
  # Replace underscores with spaces
  genre <- gsub("_", " ", genre)
  genre <- tolower(genre)
  
  # Handle specific exceptions
  genre <- switch(genre,
                  "dance-pop" = "dance pop",
                  "pop music" = "pop",
                  "hip hop music" = "hip hop",
                  "latin trap" = "trap latino",
                  "trap_music" = "trap",
                  "electronic dance music" = "edm",
                  "teen pop" = "post-teen pop",
                  "atlanta hip hop" = "atl hip hop",
                  "rhythm and blues" = "r&b",
                  genre) # Default case
  
  # Add "rap" to "hip hop" genres
  if (genre == "hip hop music") {
    genre <- c("hip hop", "rap")
  }
  
  return(genre)
}

# Extract genre names from URLs
extracted_genres <- sapply(genre_links, extract_genre_name)

# Check if the extracted genre names are in the unique genres list
filtered_urls <- genre_links[extracted_genres %in% unique_genres]


# Print the filtered URLs
print(sort(filtered_urls))

filtered_df <- as.data.frame(sort(filtered_urls))

# Prepend the base URL to get full links
base_url <- "https://en.wikipedia.org"
full_links <- paste0(base_url, filtered_urls)

scrape_genre_page <- function(link) {
  # Read the HTML content of the page
  page <- read_html(link)
  
  # Extract the title of the page
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  
  # Extract the main content while excluding irrelevant sections
  content <- page %>% html_node("div.mw-parser-output") %>%
    html_nodes(xpath = './/p') %>% # Only paragraphs and lists
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n") # Combine the text with double line breaks
  
  # Print done for each page processed
  print("done")
  
  # Return a list with the title and content
  return(list(title = title, content = content))
}

genres_data <- lapply(full_links, scrape_genre_page)

# Convert the list to a data frame for easier handling
genres_df <- do.call(rbind, lapply(genres_data, as.data.frame))


missing_content_indices <- which(is.na(genres_df$content) | genres_df$content == "")

# Iterate over missing content elements and prompt user for input
for (index in missing_content_indices) {
  cat("Title:", genres_df$title[index], "\n")
  
  # Prompt user for content input until an empty line is entered
  cat("Please enter content for this element (press Enter twice to finish):\n")
  content_lines <- character()
  repeat {
    line <- readline(prompt = "")
    if (line == "") break  # Break the loop if empty line is entered
    content_lines <- c(content_lines, line)
  }
  
  # Combine the content lines into a single string
  content <- paste(content_lines, collapse = "\n")
  genres_df$content[index] <- content
}

save(genres_df, file="./8_Textual_analysis/Music_genres/music_genres_descriptions.RData")
