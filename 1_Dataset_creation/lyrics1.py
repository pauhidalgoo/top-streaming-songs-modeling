import pandas as pd
import re
import csv
import os
from lyricsgenius import Genius
genius = Genius("C9k2o0iXz8nwhLHo5ihASBrwH6R36_XpIIRx9zPCPX5msSawlz-6gn-Mb9K5QIxl")
genius.response_format = 'plain'
genius.retries = 10

dataset = pd.read_csv('new_lyrics_spotify.csv')
old_dataset = pd.read_csv('updated_lyrics_spotify.csv')
song_lyrics = {}

for song in dataset["track_name"].unique():
    artist_name = dataset.loc[dataset["track_name"] == song, "artist_name"].iloc[0]
    lyrics = dataset.loc[dataset["track_name"] == song, "Lyrics"].iloc[0]
    lyrics_old = old_dataset.loc[old_dataset["track_name"] == song, "Lyrics"].iloc[0]
    if lyrics != lyrics_old:
        os.system('cls')
        print(song)
        print("========================")
        print(lyrics_old)
        print("-----------------")
        print(lyrics)
        a = input("Which one do you want")
        if a == '1':
            lyrics = lyrics_old
    song_lyrics[song] = lyrics
    
with open("just_lyrics.csv", "w", newline="", encoding="utf-8") as fp:
    writer = csv.DictWriter(fp, fieldnames=song_lyrics.keys())
    writer.writeheader()
    writer.writerow(song_lyrics)
    print('Done writing dict to a csv file')
"""
song = genius.search_song(title="Rockabye", artist="Clean Bandit")
a = re.sub(r'\([^)]*\)', '', song.lyrics)
a = re.sub(r'\[[^\]]*\]', '', a)
a = re.sub(r'^(.*)Lyrics', '', a)
"""