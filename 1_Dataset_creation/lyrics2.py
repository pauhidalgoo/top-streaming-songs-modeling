import pandas as pd
import re
import csv

with open("./1_Dataset_creation/just_lyrics_corrected.csv", "r",  encoding="utf-8") as infile:
    # Create a reader object
    reader = csv.DictReader(infile)

    # Iterate through the rows
    for row in reader:
        #print(row)
        song_lyrics = row

dataset = pd.read_csv('./1_Dataset_creation/top50_global.csv')
lyrics = []
for song in dataset["name"]:
    lyrics.append(song_lyrics[song])
dataset['lyrics'] = lyrics

dataset.to_csv('./1_Dataset_creation/top50_lyrics.csv')
print(len(dataset["name"].unique()))

"""
song = genius.search_song(title="Rockabye", artist="Clean Bandit")
a = re.sub(r'\([^)]*\)', '', song.lyrics)
a = re.sub(r'\[[^\]]*\]', '', a)
a = re.sub(r'^(.*)Lyrics', '', a)
"""