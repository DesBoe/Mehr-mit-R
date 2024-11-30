#### 01. 1_BeispielDaten_Jördis einlesen####

# Working directory
# Session --> Set Working directory --> to source file location
setwd("/Volumes/Dennis_OTG/Rworkshop/DataSheetsForWorkshop")

# Tip: aus der Konsole Kopieren und hier einfügen --> erneutes Ausführen geht schneller


#### 02. Pakete abrufen ####

#"Apps Laden, um damit was zu machen"

library(xlsx)
library(readxl)
library(ggplot2)


#### 03. Daten einlesen ####

Wachstum <- read_excel("1_BeispielDaten_Jördis.xlsx",# welche Datei?
                       sheet = "Growth curve ", # welches Tabellenblatt?
                       ) 

# Console: hat neue Namen angelegt!
# Wachstum anschauen

head(Wachstum)
# erste Zeile mit 3 4 5 usw
# dann ein NA --> Die Daten sind aber vollständig!

tail(Wachstum)

str(Wachstum)
# viele characters anstatt Nummern

Wachstum


#### 04. Theorie Teil: Table Long vs Table Short ####

#### 05. Table Long einlesen

Growth <- read_excel("3-2_WachstumTableLong_template.xlsx",
                        sheet = 1)

head(Growth)
# Daten sind jetzt in langer Liste
# perfekt zum Plotten, da R nach jedem Zeitpunkt, Experiment und Treament die Daten Filtern kann

str(Growth)
# Zeit und Wachstum sind Numerisch, das ist gut
# Experiment und Treatment sind Text, Faktor ist aber besser

Growth$experiment <- as.factor(Growth$experiment)
Growth$treatment <- as.factor(Growth$treatment)

str(Growth)
# R erkennt nun 2 Treatments und 4 Experimente (die pro Treatement durchgeführt wurden)


#### 06. Labortabelle manipulieren ####

library(data.table)
Messreihen <- read_excel("3-3_Labortabelle.xlsx")

Messreihe_Long <- melt(setDT(Messreihen[,1:ncol(Messreihen)]), id.vars = c("treatment", "time"), variable.name = "Experiment")
#SetDT konvertiert
#Wir wollen die Messreihen manipulieren, also alle Spalten. Dabei müssen wir das max. Angeben (ncol(Messreihen))
#id,vars sind Variablen, die uns die Identität eines jeden Datenpunktes bestimmen, 
#alle anderen Varaiblen werden bei jeder ID untereinander geschrieben und nach deren früheren Tabellen Kopf und Wert ind 2 Spalten Geschrieben
# blöd ist nun der Name value...
names(Messreihe_Long)[names(Messreihe_Long) == 'value'] <- 'growth'

# Damit lässt sichs doch arbeiten :)

head(Messreihe_Long)
tail(Messreihe_Long)
str(Messreihe_Long)

Messreihe_Long$treatment <- as.factor(Messreihe_Long$treatment)
str(Messreihe_Long)


# Pause? #


####07 Daten Analyse

