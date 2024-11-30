# Umgebung Aufräumen
rm(list = ls())
# - ich mache es nur selten, das ist aber nur so eine Macke ^^^

setwd("/Volumes/Dennis_OTG/Rworkshop/DataSheetsForWorkshop")

library(xlsx)
library(readxl)
library(ggplot2)
library(data.table)


Messreihen <- read_excel("3-3_Labortabelle.xlsx")

Messreihen <- melt(setDT(Messreihen[,1:ncol(Messreihen)]), id.vars = c("treatment", "time"), variable.name = "Experiment")
names(Messreihen)[names(Messreihen) == 'value'] <- 'growth'
Messreihen$treatment <- as.factor(Messreihen$treatment)

str(Messreihen)


####07 Daten Analyse####

plot(Messreihen$time, Messreihen$growth)

# die Basis Befehle sind ganz brauchbar, aber da geht noch mehr mit GGPLOT

## Basaler Plot
ggplot(data = Messreihen)+
  geom_point(aes(time,growth))

## schöneres theme
ggplot(data = Messreihen)+
  geom_point(aes(time,growth))+
  theme_classic()


## Achsenbeschriftungen
ggplot(data = Messreihen)+
  geom_point(aes(time,growth))+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  theme_classic()


## Abstand bei den Achsen
ggplot(data = Messreihen)+
  geom_point(aes(time,growth))+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,.6), expand = c(0, 0))+
  theme_classic()

## Punkte nach Experiment und Treatment 
ggplot(data = Messreihen)+
  geom_point(aes(time,growth, shape = Experiment, colour = treatment))+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,.6), expand = c(0, 0))+
  theme_classic()


#Mittelwerte für besser Visualisierung
#! Mittelwerte nur bei Normalverteilung Nehmen (shaprio Test + QQ plot)
# zur Einfachheit nehmen wir den hier


library(dplyr)
mean_values <- Messreihen %>% # wir wollen also Mittelwerte der Messreihen berechnen
  group_by(treatment, time) %>% # die Daten sollen Nach den Faktoren Treatment und Zeit ausgeschlüsselt werden
  summarise(Mean = mean(growth, na.rm = TRUE), .groups = 'drop')
# Fasse zusammen: erstelle die Spalte Mean
# diese soll berechnet werden als Mittelwert von growth, nicht beachtet werden NAs
# Gruppierungsstribute entfernen, um sauberes Datafram zu erhalten


ggplot(data = mean_values)+
  geom_point(aes(time,Mean, colour = treatment))+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,.6), expand = c(0, 0))+
  theme_classic()


# Plotten fertig - andere Farben???
#Farbvektor
my_cols <- c("C. albicans SC 5314" = "red", "control" = "blue")
my_cols <- c("C. albicans SC 5314" = "#E61A1A", "control" = "#0060FF")

ggplot(data = mean_values)+
  geom_point(aes(time,Mean, colour = treatment))+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,.6), expand = c(0, 0))+
  scale_colour_manual(values=my_cols) +
  ggtitle("Meine \nWachstumskurve")+
  theme_classic()  


## Modifizieren
Mein_Thema <-     theme(aspect.ratio = 1:1,
                        panel.background = element_blank(),
                        legend.position= "right", 
                        axis.line = element_line(colour = "black", size = 0.5),
                        legend.title = element_text(size=18, color= "black"),
                        plot.title = element_text(size=20, color= "purple"),
                        legend.text= element_text(size=16, color= "black"),
                        legend.key = element_blank(),
                        axis.text= element_text(size=16, color= "black"), 
                        axis.title = element_text(size=18, color= "black"),
                        axis.ticks = element_line(colour = "black", size = 0.5)
                        ) 


ggplot(data = mean_values)+
  geom_point(aes(time,Mean, colour = treatment), size = 3)+
  xlab("time(h)")+
  labs(y = expression(OD[600])) +
  scale_x_continuous(limits = c(0,50), expand = c(0, 0))+
  scale_colour_manual(values=my_cols) +
  scale_y_continuous(limits = c(0,.6), expand = c(0, 0))+
  ggtitle("Meine \nWachstumskurve")+
  Mein_Thema 

#Plot speichern
ggsave("04_Wachstumskurve.svg")


####08 weitere Manipulation: Inkspace, Illustrator...####
# Ich nutze Affinity (keine Schleichwerbung ;) )
