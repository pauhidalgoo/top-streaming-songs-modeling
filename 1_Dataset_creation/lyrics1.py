import pandas as pd
import re
import csv
import os
from lyricsgenius import Genius
genius = Genius("API-KEY")
genius.response_format = 'plain'
genius.retries = 10

dataset = pd.read_csv('./1_Dataset_creation/top50_global_lyrics2.csv')
old_dataset = pd.read_csv('./1_Dataset_creation/top50_global_lyrics.csv')
song_lyrics = {}

for song in dataset["name"].unique():
    lyrics = dataset.loc[dataset["name"] == song, "lyrics"].iloc[0]
    lyrics_old = old_dataset.loc[old_dataset["name"] == song, "lyrics"].iloc[0]
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