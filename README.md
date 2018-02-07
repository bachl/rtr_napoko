# Material zum NapoKo-Workshop "Medienwirkungsforschung in Echtzeit: RTR-Messung in der Politischen Kommunikation"

## Von Marko Bachl, Universität Hohenheim

Hier findet ihr das Material zum NapoKo-Workshop. Alle Infos zum Workshop gibt es hier: http://napoko.de/?p=1369.

## Interaktive Apps zur Analyse der RTR-Messungen

Die Apps zur Analyse der RTR-Messungen zum TV-Duell 2013 könnt ihr lokal in RStudio ausführen - damit sollte die Performance besser sein als mit der Web-Variante. Wenn ihr dieses Repository geklont oder komplett heruntergeladen habt, findet ihr die Apps in den Verzeichnissen *R/rtr_dial* und *R/rtr_pb*.

Alternativ könnt ihr auch Folgendes in ein RStudio Skript kopieren und ausführen:
(Voraussetzung ist, dass die Pakete `lubridate` und `tidyverse` installiert sind.)
```R
library(shiny)
runGitHub("rtr_napoko", "bachl", subdir = "R/rtr_dial/") # Dial-Daten
runGitHub("rtr_napoko", "bachl", subdir = "R/rtr_pb/") # PB-Daten
```

Online finden sich die Apps hier:

* https://review.shinyapps.io/rtr_dial/
* https://review.shinyapps.io/rtr_pb/

