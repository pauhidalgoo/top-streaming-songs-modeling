# SPOTIFY
## PMAAD-GIA

Aquest és el repositori de Github del projecte. Tingueu en compte diverses coses:

- Quan obriu el projecte de RStudio, feu PULL. Quan vulgueu pujar els vostres canvis, Commit (Ctrl-Alt-M) i PUSH.
- Si creieu que heu de fer canvis que modifiquin molt, podeu fer una nova branca i llavors ja l'ajuntarem.
- Quan obriu RStudio, per obrir la carpeta aneu a Open project.

## Com obrir-lo per primer cop des de RStudio?
Des de RStudio vas a File -> New Project
Allà escolliu: Version Control -> Git*
Us demana un URL, és el del repositori.
![alt text](https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/url.png?raw=true)
El nom no el canvieu.
La tercera opció és la carpeta del vostre ordinador on el voleu posar, escolliu on el vulgueu.

## Com fer commit, push, pull... des de RStudio?
A dalt, hi ha un petit botó que posa git.
![alt text](https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/botogit.png?raw=true)

Si el cliqueu, veureu diverses opcions. Les que ens interessen més Commit (Ctrl-Alt-M), Pull Branches i Push Branches.
![alt text](https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/llistagit.png?raw=true)
Pull és quan voleu agafar el que hi ha al github, i Push per penjar. Abans de penjar, però, heu de fer commit. Per fer-ho, cliqueu (o feu la drecera del teclat). S'obrirà una finestra així:
![alt text](https://github.com/pauhidalgoo/PMAAD_GIA/blob/main/Media/Tutorial/commit.png?raw=true)

Si cliqueu als quadradets anireu afegint (ADD) els canvis, fent stage. Veureu que passen de groc a verd. Un cop fet tot el que voleu commitejar, ESCRIVIU UN MISSATGE (important, sinó pot ser que us doni error) i cliqueu el botó commit (s'obre una finestra de terminal que quan acaba es pot tancar i ja). Llavors, recordeu que per pujar-los heu de fer PUSH.

## Problemes
Si al anar a version control no surt la opció Git, tenir en compte que heu de tenir Git instal·lat a l'ordinador. En principi si al Visual el teniu configurat també hauria d'anar.
Si teniu problemes a la hora de fer commits, push i tal, comproveu que tingueu el Git ben enllaçat amb el Github.

Si al pujar us surt un error de que el fitxer és massa gran, busqueu-ho a internet jsjs. Us farà baixar com una extensió de Git per fitxers que pesen molt. No crec que passi perquè treballem amb RData.

Al gitignore afegiu els arxius que NO voleu que us pugi. Podeu especificar els que acabin amb .pdf, per exemple.