from googletrans import Translator
import pandas as pd
import time
import json


def translate_to_english(sentence):
    traductor = Translator()
    try:
        result = traductor.translate(sentence, dest='en')
        return result.src, result.text
    except json.JSONDecodeError:
        print("Error in the decodification.")
        return 'en', sentence
    except Exception as e:
        print(f"Error happened: {str(e)}")
        return 'en', sentence
    

# Example of use
if __name__ == "__main__":
    # Load the preprocessed dataframe


    dataset = pd.read_csv('8_Textual_analysis/final_d3_data.csv')
    translated_lyrics = {} # Initialize an empty dictionary to store the translations

    progress = 0
    n_songs = dataset['track_name'].unique().shape[0]


    # Iterate over each song in the dataset

    for song in dataset['lyrics'].unique():
        if song not in translated_lyrics:
            translated_lyrics[song] = translate_to_english(song)
            progress += 1
            print('\r', f'Progress: {round(progress*100/n_songs,2)}%', end='')
            time.sleep(0.01)  # Pause to avoid overwhelming the API


    # Translated dataset is a new dataset just with the variable lyrics
    translated_dataset = dataset[['lyrics']]
    
    translated_dataset['lyrics'] = translated_dataset['lyrics'].apply(lambda x: translated_lyrics[x][1])
    translated_dataset['song_language'] = translated_dataset['lyrics'].apply(lambda x: translated_lyrics[x][0])

    # Change the column name to 'translated_lyrics'

    translated_dataset.columns = ['translated_lyrics']

    # Save the translated dataset to a new CSV file

    translated_dataset.to_csv('8_Textual_analysis/translated_lyrics.csv', index=False)

