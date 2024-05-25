library(tidyverse)

top50_lyrics_trans <- read.csv("./1_Dataset_creation/top50_lyrics_trans.csv", comment.char="#")

top50_lyrics <- read.csv("./1_Dataset_creation/top50_lyrics.csv", comment.char="#")

top50_lyrics["original_language"] <- top50_lyrics_trans["original_language"]
top50_lyrics["translated_lyrics"] <- top50_lyrics_trans["translated_lyrics"]


top50_lyrics <- top50_lyrics %>%
  mutate(artists = str_split(artists, ",\\s*") %>% map_chr(1))

top50_lyrics[is.na(top50_lyrics["translated_lyrics"]), ] <- " "

top50_tracks_lyrics <- top50_lyrics %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup()

top50_tracks_lyrics <- top50_tracks_lyrics %>%
  mutate(artists = str_split(artists, ",\\s*") %>% map_chr(1))


colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "spotify_id"] <- "track_id"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "name"] <- "track_name"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "artists"] <- "artist_name"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "popularity"] <- "track_popularity"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "is_explicit"] <- "explicit"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "duration_ms"] <- "duration"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "mode"] <- "major_mode"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "album_release_date"] <- "year_release"
colnames(top50_tracks_lyrics)[colnames(top50_tracks_lyrics) == "original_language"] <- "lyrics_language"


# Convert date to year
top50_tracks_lyrics$year_release <- as.numeric(format(as.Date(top50_tracks_lyrics$year_release), "%Y"))


load("8_Textual_analysis/unique_tracks_translated.RData")
# Add missing columns with FALSE
genre_columns <- c("pop", "hip _hop", "rock", "electro", "christmas", "cinema","latino")
top50_tracks_lyrics[genre_columns] <- FALSE

missing_columns <- setdiff(names(unique_translated), names(top50_tracks_lyrics))
top50_tracks_lyrics[missing_columns] <- NA

# Select and reorder columns to match unique_translated
top50_tracks_lyrics <- top50_tracks_lyrics %>%
  select(names(unique_translated))

save(top50_tracks_lyrics, file="./8_Textual_analysis/2024_global_tracks.RData")


top50_tracks_lyrics$"explicit" <- as.logical(top50_tracks_lyrics$"explicit")

top50_tracks_lyrics$key <- as.factor(top50_tracks_lyrics$key)
top50_tracks_lyrics$time_signature <- as.factor(top50_tracks_lyrics$time_signature)
top50_tracks_lyrics$year_release <- as.factor(top50_tracks_lyrics$year_release)


top50_tracks_lyrics[c("nationality", "gender")] <- "Unknown"

combined_df <- bind_rows(unique_translated, top50_tracks_lyrics)


save(combined_df, file="./8_Textual_analysis/unique_translated_2024.RData")
