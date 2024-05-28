# SPOTIFY TOP 40 MODELING AND ANALYSIS

> This repo contains the work done in the PMAAD course of the Artificial Intelligence Degree


## Table of Contents

- [SPOTIFY TOP 40 MODELING AND ANALYSIS](#spotify-top-40-modeling-and-analysis)
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

### Com fer commit, push, pull... des de RStudio?

A dalt, hi ha un petit botó que posa git.

<p align="center">
  <img width="460" height="300" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/botogit.png?raw=true">
</p>

Si el cliqueu, veureu diverses opcions. Les que ens interessen més Commit (Ctrl-Alt-M), Pull Branches i Push Branches.

<p align="center">
  <img width="200" height="300" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/llistagit.png?raw=true">
</p>
Pull és quan voleu agafar el que hi ha al github, i Push per penjar. Abans de penjar, però, heu de fer commit. Per fer-ho, cliqueu (o feu la drecera del teclat). S'obrirà una finestra així:

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/commit.png?raw=true">
</p>
Si cliqueu als quadradets anireu afegint (ADD) els canvis, fent stage. Veureu que passen de groc a verd. Un cop fet tot el que voleu commitejar, ESCRIVIU UN MISSATGE (important, sinó pot ser que us doni error) i cliqueu el botó commit (s'obre una finestra de terminal que quan acaba es pot tancar i ja). Llavors, recordeu que per pujar-los heu de fer PUSH.

### Problemes

Si al anar a version control no surt la opció Git, tenir en compte que heu de tenir Git instal·lat a l'ordinador. En principi si al Visual el teniu configurat també hauria d'anar.
Si teniu problemes a la hora de fer commits, push i tal, comproveu que tingueu el Git ben enllaçat amb el Github.

Si al pujar us surt un error de que el fitxer és massa gran, busqueu-ho a internet jsjs. Us farà baixar com una extensió de Git per fitxers que pesen molt. No crec que passi perquè treballem amb RData.

Al gitignore afegiu els arxius que NO voleu que us pugi. Podeu especificar els que acabin amb .pdf, per exemple.

Si quan aneu a fer push us salta un error en el terminal de Git, sol dir quin és el motiu. El més típic és que no estiguin sincronitzats els vostres canvis amb els del github, és a dir, us falta fer un push abans. La majoria de cops, serà capaç de resoldre'l sol si no hi ha conflictes. Si n'hi hi ha és més liat, o sigui que vigileu. Una bona praxi és fer branques: les pots crear des de Github, i llavors des de Rstudio, en el lloc on fas commit, a dalt a l'esquerra pots canviar. Et poses a la teva branca, fas commit i tal normal sense por i quan acabes fas push. Des de la web de Github et sortirà segurament Your recently pushed branches, i podràs fer un pull request per posar els teus canvis a la main. Si hi ha conflictes, segurament els podràs resoldre des d'allà (més fàcil que des de R).

Per a més informació, podeu mirar a la web de Github <a href="https://docs.github.com/es/pull-requests">aquí</a>.

## Results and examples
We can visualize some of the results obtained.

For example, with TimeSeries clustering we got 5 clusters with different types of streaming evolutions.

<p align="center">
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Clustering/TieSeriesClustering/streams_cada_mes_per_cluster_amb_mitjana.png?raw=true">
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
  <img width="460" height="250" src="https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/GUI_playlist.png?raw=true">
</p>

As you can see, there's a lot of different work done, so this Readme isn't enough to provide the complete description. For that, go to `D4_report.pdf`
