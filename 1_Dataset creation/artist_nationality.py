import requests
import pandas as pd
import time

def get_artist_details(artist_name):
    # URL of the MusicBrainz API to search for an artist by name
    url = f"https://musicbrainz.org/ws/2/artist/?query=artist:{artist_name}&fmt=json"
    
    # Make a request to the API
    response = requests.get(url)
    data = response.json()
    
    # Check if artist data was found
    if 'artists' not in data or not data['artists']:
        return "NA", "NA", "NA", "NA"  # Returns "NA" for country, city, gender, and if it's a group
    
    # Get the first artist in the list
    artist = data['artists'][0]
    
    # Get the full name of the country
    country = artist.get('area', {}).get('name', "NA")
    
    # Get the city of origin
    city = artist.get('begin-area', {}).get('name', "NA")
    
    # Attempt to obtain the gender, or mark as missing
    gender = artist.get('gender', "NA").lower()
    if gender not in ["male", "female", "non-binary"]:
        gender = "NA"  # Missing data if not one of the expected values
    
    # Determine if the artist is a group or an individual
    artist_type = artist.get('type', "NA")
    is_group = 1 if artist_type == "Group" else 0 if artist_type == "Person" else "NA"
    
    return country, city, gender, is_group

# Load the preprocessed dataframe

dataset = pd.read_csv('preprocessed_spotify.csv')

# Initialize a dictionary to store the information
artist_information = {}

progress = 0
n_artists = dataset['artist_name'].unique().shape[0]

# Iterate over each artist in the dataset
for artist_name in dataset['artist_name'].unique():
    if artist_name not in artist_information:
        artist_information[artist_name] = get_artist_details(artist_name)
        progress += 1
        print('\r', f'Progress: {round(progress*100/n_artists,2)}%', end='')
        time.sleep(0.7)  # Pause to avoid overwhelming the API

# Initialize lists to store nationalities, cities, genders, and if it's a group
nationalities = []
cities = []
genders = []
is_group = []

# Iterate over each artist in the dataset
for artist_name in dataset['artist_name']:
    info = artist_information[artist_name]
    nationalities.append(info[0])
    cities.append(info[1])
    genders.append(info[2])
    is_group.append(info[3])

# Add the new columns to the dataset
dataset['Nationality'] = nationalities
dataset['City'] = cities
dataset['Gender'] = genders  # Column to indicate gender, including non-binary
dataset['Is_Group'] = is_group  # Binary column to indicate if it's a group

# Save the updated dataset
dataset.to_csv("updated_spotify.csv", index=False)
