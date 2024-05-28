# TOP 40 SONGS MODELING AND ANALYSIS

> This repo contains the work done in the PMAAD course of the Artificial Intelligence Degree


## Table of Contents

- [TOP 40 SONGS MODELING AND ANALYSIS](#top-40-songs-modeling-and-analysis)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Data sources](#data-sources)
  - [File structure](#file-structure)
    - [Description of folders](#description-of-folders)
  - [Requeriments](#requeriments)
  - [Use](#use)
    - [GUI](#gui)
    - [Dataset creation / textual / geospatial](#dataset-creation--textual--geospatial)
    - [Com fer commit, push, pull... des de RStudio?](#com-fer-commit-push-pull-des-de-rstudio)
    - [Problemes](#problemes)
  - [Results and examples](#results-and-examples)

## Overview

Music has always been a very important element of the day to day life of many people. In this work, we seek to analyze the consumer habits and the creation characteristics of the top 40 weekly most streamed songs. The data used comprises the months between the start of 2017 and april of 2021.

From this data, we're going unveal trends in the most popular music, which traits do they have in common the most successful songs, what genres... To do this, we're going to use different preprocessing, advanced clustering, textual analysis and geospatial tools to extract the maximum knowledge from the dataset. The programming language selected to do so is R.

---
Des de l'arribada de les plataformes de streaming, la música s'ha convertit en un element molt present en el dia a dia d'una gran quantitat de persones. En aquest treball, es buscarà analitzar els hàbits de consum a partir de les 50 cançons més escoltades cada setmana.
Les dades amb les que es treballarà provenen de Spotify, una de les empreses amb més usuaris en el sector, i van ser recollides entre el 2017 i el quart mes del 2021.

A partir d'aquesta informació, s'intentarà esbrinar quines són les tendències a les quals evoluciona la música més popular, quins trets tenen en comú les cançons més exitoses i quins estils trobem, entre d'altres.

Durant el transcurs d'aquest projecte s'usaran tècniques de preprocessament i de clústering avançades, així com eines d'anàlisi textual i geoespacial per tal d'extreure el màxim d'informació d'aquestes dades.


## Data sources

The original data used came from Kaggle, specifically [spotify-top-200-dataset](https://www.kaggle.com/datasets/younver/spotify-top-200-dataset).

Additionally, to get more information this data has been expanded using the Genius API, along with MusicBrainz one and googletrans library.

For geospatial analysis we also used information from [top-spotify-songs-in-73-countries-daily-updated](https://www.kaggle.com/datasets/asaniczka/top-spotify-songs-in-73-countries-daily-updated) which is made available under the ODC Attribution License.

## File structure
The file and folder structure is the following:

    ├── 1_Dataset_creation
    ├── 2_Descriptive_analysis
    ├── 3_Preprocessing
    ├── 4_Clustering
    ├── 5_Profiling
    ├── 6_Factorial_methods
    ├── 7_Geoespacial
    ├── 8_Textual_analysis
    │   ├── Data_creation
    │   ├── Descriptive_sentiment
    │   └── Music_genres
    ├── backup
    ├── GUI
    ├── Latex
    ├── Media
    │   ├── ACM
    │   ├── ACM2
    │   ├── ACP
    │   ├── Clustering
    │   ├── Descriptive
    │   ├── Geodescriptive
    │   ├── Geoespacial_new
    │   ├── Geotextual
    │   ├── Neural_Network
    │   ├── Preprocessing
    │   ├── Textual_Analysis
    │   └── TLP
    ├── data.RData
    ├── final_d3_data.RData
    ├── PMAAD_GIA.Rproj
    ├── Readme.md
    └── ultra_updated_data.RData

### Description of folders

- **Dataset creation**: Python and R scripts to initialy process data and to add lyrics
- **Descriptive analysis**: R scripts (and RMarkdowns) to perform univariate and bivariate analysis. PDFs with the results. Also, `unique_artists`and `unique_tracks` RData
- **Preprocessing**: Script to add missing data randomly, three different imputation methods and comparison.
- **Clustering**: Scripts in R containing advanced clustering techiques: CURE, dbscan, hierarchical, kmode, optics and Time Series. Also, some basic profiling.
- **Profiling**: Profiling of the clusters obtained by hierarchical, using Class Panel Graphs and TLP
- **Factorial methods**: ACM, PCA. We select the axes which explain 80% and train a neural network to predict explicit
- **Geoespacial**: Shapefile and RData for geospatial analysis. An R script for descriptive, and two for modeling (artist data and countries data)
- **Textual analysis**
  - **Data creation**: R scripts and notebook to create new data and translate lyrics
  - **Descriptive sentiment**:Basic descriptive analysis of text, and sentimental analysis.
  - **Music genres**: Using Wikipedia data and LDA to group subgenres into main genres of music
  The other files perform CA-GALT, LSA, LDA, and also include scripts to predict genre based on lyrics and predict songs based on a sentence (playlist creator), and some RDatas
- **GUI**: Implementation of a Shiny app, with wordcloud viewer, playlist creator, genre predictor, artist visualizer and song prediction based on coordinates.
- **Latex**: Latex files of the report
- **Media**: Saved plots

## Requeriments

To start, you need R (RStudio) and for some of the files Python (the data creation ones). To install the required libraries, we recommend to use [renv](https://rstudio.github.io/renv/articles/renv.html) with the packages listed in `renv.lock`. 

## Use
The use of the code is pretty straightforward, just execute the R files that you need. In some cases, you may need extra information.

### GUI
To use the GUI, you need to execute the `wordcloudapp.R`. You may notice that `api_configuration.R` is missing: it is used to connect with the Spotify API to automatically create the playlists. If you want to add this functionality, create the file with `Sys.setenv(SPOTIFY_CLIENT_SECRET = 'API_KEY')`and `Sys.setenv(SPOTIFY_CLIENT_ID = 'API_KEY')`

### Dataset creation / textual / geospatial
For some of the files that create data for this sections, the code asks for a csv. Some are the ones from kaggle, but others are versions created using the Python scripts. To avoid including too large files in Github, this csv files have been ignored, but you you probably have some RData with the same information.

This section also includes some information about how to work with Github and RStudio (used during the development of the project) in the following sections

## Results and examples
We can visualize some of the results obtained.

For example, with TimeSeries clustering we got 5 clusters with different types of streaming evolutions.

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Clustering/TimeSeriesClustering/streams_cada_mes_per_cluster_amb_mitjana.png?raw=true">
</p>

With the Neural network, we got this confusion matrix

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Neural_Network/cm_mlp.png?raw=true">
</p>

Using LDA, we got the following 6 topics, that separated songs talking about love, songs in Spanish, hip hop songs...

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Textual_Analysis/LDA/lda_topics6.png?raw=true">
</p>

In the geospatial modeling, we fitted some variograms and used kriging to obtain results like the following one for energy

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Geoespacial_new/energy_interpolation.png?raw=true">
</p>

We can also observe some examples of the GUI, like the playlist section

<p align="center">
  <img width="460" height="350" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/GUI_playlist.png?raw=true">
</p>

As you can see, there's a lot of different work done, so this Readme isn't enough to provide the complete description. For that, go to `D4_report.pdf`
