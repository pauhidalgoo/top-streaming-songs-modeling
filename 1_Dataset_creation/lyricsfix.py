import pandas as pd
import csv

def get_multiline_input(prompt):
    print(prompt)
    lines = []
    while True:
        line = input()
        if line == "END":  # The user can type 'END' to finish input
            break
        lines.append(line)
    return "\n".join(lines)

with open("./1_Dataset_creation/just_lyrics_corrected.csv", "r", encoding="utf-8") as infile:
    # Create a reader object
    reader = csv.DictReader(infile)
    
    # Read the data into a list of dictionaries
    data = list(reader)
    
    # Get the fieldnames from the reader object
    fieldnames = reader.fieldnames


for row in data:
    for key, value in row.items():
        if value == "NA" or value == None or value == "nan":
            # Prompt user for input
            print(f"Lyrics for {key}")
            print("=============================================")
            row[key] = get_multiline_input(f"Please enter the value for {key} (type 'END' on a new line to finish): ")

# Write the corrected data to a new CSV file
with open("./1_Dataset_creation/just_lyrics_corrected.csv", "w", encoding="utf-8", newline='') as outfile:
    # Create a writer object
    writer = csv.DictWriter(outfile, fieldnames=fieldnames)
    
    # Write the header
    writer.writeheader()
    
    # Write the corrected data
    writer.writerows(data)
