import pandas as pd
import re
import csv

with open("just_lyrics.csv", "r",  encoding="utf-8") as infile:
    # Create a reader object
    reader = csv.DictReader(infile)

    # Iterate through the rows
    for row in reader:
        print(row)
        song_lyrics = row




dataset = pd.read_csv('updated_spotify.csv')
lyrics = []
for song in dataset["track_name"]:
    lyrics.append(song_lyrics[song])
dataset['Lyrics'] = lyrics

dataset.to_csv('./spotify_new.csv')
print(len(dataset["track_name"].unique()))

"""
song = genius.search_song(title="Rockabye", artist="Clean Bandit")
a = re.sub(r'\([^)]*\)', '', song.lyrics)
a = re.sub(r'\[[^\]]*\]', '', a)
a = re.sub(r'^(.*)Lyrics', '', a)
"""