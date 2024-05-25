import re
import csv
from lyricsgenius import Genius

genius = Genius("API-KEY")
genius.response_format = 'plain'
genius.retries = 10

"""
song = genius.search_song(title="Rockabye", artist="Clean Bandit")
a = re.sub(r'\([^)]*\)', '', song.lyrics)
a = re.sub(r'\[[^\]]*\]', '', a)
a = re.sub(r'^(.*)Lyrics', '', a)
"""

in_path = './1_Dataset_creation/top50_global.csv'
out_path = './1_Dataset_creation/top50_global_lyrics2.csv'

lyrics_cache = {}

with open(in_path, 'r', newline='', encoding="utf-8") as inputFile, open(out_path, 'a', newline='', encoding="utf-8") as writerFile:
    read_file = csv.reader(inputFile, delimiter=',')
    write_file = csv.writer(writerFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    line = 0
    for row in read_file:
        try:
            #stitle = re.sub(r'\([^)]*\)', '', row[1])
            stitle = row[1].split(" - From ")[0]
            # Check if lyrics are already cached
            if stitle in lyrics_cache:
                a = lyrics_cache[stitle]
            else:
                song = genius.search_song(title=stitle, artist=row[2])
                a = song.lyrics
                a = re.sub(r'\([^)]*\)', '', a)
                a = re.sub(r'\[[^\]]*\]', '', a)
                a = re.sub(r'^(.*)Lyrics', '', a)
                a = re.sub(r'"', '', a)
                a = re.sub(r'\d+Embed$', '', a)
                # Cache the lyrics
                lyrics_cache[stitle] = a
        except:
            a = 'NA'
        write_file.writerow(row+[a])
        line += 1
        print(line)