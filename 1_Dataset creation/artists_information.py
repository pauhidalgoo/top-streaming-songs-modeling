import requests
import pandas as pd
import time


# Function to get the gender of a group based on their members

def get_group_gender(artist_id, attempts=3):
    url = f"https://musicbrainz.org/ws/2/artist/{artist_id}?inc=artist-rels&fmt=json"
    
    genders = set()
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        
        for relation in data.get('relations', []):
            if relation['type'] == 'member of band' and relation.get('artist'):
                member_id = relation['artist']['id']
                for attempt in range(attempts):
                    member_response = requests.get(f"https://musicbrainz.org/ws/2/artist/{member_id}?fmt=json")
                    if member_response.status_code == 200:
                        member_data = member_response.json()
                        member_gender = member_data.get('gender', "NA")
                        if member_gender:
                            genders.add(member_gender.lower())
                            break  # Exit the retry loop if gender is found
                    if attempt == 0:
                        print(f"\nError {member_response.status_code} for member: {member_id}")
                    print(f"Retry {attempt + 1} for member: {member_id}")
                    time.sleep(5)  # Wait 5 seconds before retrying

    if "female" in genders and "male" in genders:
        return "mixed"
    elif "female" in genders:
        return "female"
    elif "male" in genders:
        return "male"
    else:
        return "NA"
    

# Function to get whether a city is in a country using the countriesnow API

def is_city_in_country(country, city, attempts=3):
    url = "https://countriesnow.space/api/v0.1/countries/cities"
    payload = {
        "country": country
    }
    for attempt in range(attempts):
        response = requests.post(url, json=payload) 

        if response.status_code == 200:
            data = response.json()
            if city in data['data']: 
                return True
            else:
                if attempt < attempts - 1: 
                    print(f"City not found in attempt {attempt + 1} for country: {country}")
                    time.sleep(5)  
                else:
                    return False  
        else:
            print(f"Error {response.status_code} on attempt {attempt + 1} for country: {country}")
            if attempt < attempts - 1:
                time.sleep(5) 
    return False


# Function to get the capital of a country using the restcountries API

def capital_of_country(country, attempts=3):
    url = f'https://restcountries.com/v3.1/name/{country}?fullText=true'
    for attempt in range(attempts):
        response = requests.get(url) 
        if response.status_code == 200:
            data = response.json()
            capital = data[0]['capital'][0]
            return capital

        else:
            print(f"Error {response.status_code} on attempt {attempt + 1} for country: {country}")
            if attempt < attempts - 1:
                time.sleep(5) 
    return None


# Function to get the details of an artist using the MusicBrainz API

def get_artist_details(artist_name, attempts=3):
    """
    Fetches artist details from MusicBrainz API with up to 'attempts' retries.
    Returns the artist's country, city, gender, and group status.
    If the artist is a group, attempts to determine the group's collective gender.
    """
    url = f"https://musicbrainz.org/ws/2/artist/?query=artist:{artist_name}&fmt=json"
    for attempt in range(attempts):
        response = requests.get(url)
        if response.status_code == 200:
            data = response.json()
            if 'artists' in data and data['artists']:
                artist = data['artists'][0]  # Assuming the first result is the correct artist
                country = artist.get('area', {}).get('name', "NA")
                city = artist.get('begin-area', {}).get('name', "NA")
                artist_type = artist.get('type', "NA")
                if artist_type == "Group":
                    is_group = 1
                    gender = get_group_gender(artist['id'])
                elif artist_type == "Person":
                    is_group = 0
                    gender = artist.get('gender', "NA").lower()
                else:
                    is_group = "NA"
                    gender = "NA"
                
                if city == "NA" or not is_city_in_country(country, city):
                    city = capital_of_country(country)
                    time.sleep(3)  # Pause to avoid overwhelming the API
                return country, city, gender, is_group
            else:
                print(f"Retry {attempt + 1} for artist: {artist_name}")
                time.sleep(5)  # Wait 5 second before retrying
        else:
            print(f"Error {response.status_code} on attempt {attempt + 1} for artist: {artist_name}")
            time.sleep(5)
    # If all attempts fail, return NA for all fields
    print(artist_name)
    return "NA", "NA", "NA", "NA"

# Load the preprocessed dataframe


dataset = pd.read_csv('data_knn_imputed.csv')

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
        time.sleep(3)  # Pause to avoid overwhelming the API

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
dataset['nationality'] = nationalities
dataset['city'] = cities
dataset['gender'] = genders  # Column to indicate gender, including non-binary
dataset['is_Group'] = is_group  # Binary column to indicate if it's a group

# Save the updated dataset
dataset.to_csv("ultra_updated_spotify.csv", index=False)



