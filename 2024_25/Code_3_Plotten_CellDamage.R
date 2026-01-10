####09 Celldamage - base R####
setwd("/Volumes/Dennis_OTG/Rworkshop/DataSheetsForWorkshop")

library(xlsx)
library(readxl)
library(ggplot2)
library(data.table)
library(dplyr)


CD <- read_excel("4-1_CelldamageLabor.xlsx")
str(CD)
# Gutes Zeichen: alle Rs sind numerisch,
# Treatments sind Charakters - R erkennt hier keien Faktoren, das ist aber okay


CellDamage <- melt(setDT(CD[,1:ncol(CD)]), id.vars = c("Treatment"), variable.name = "Experiment")
names(CellDamage)[names(CellDamage) == 'value'] <- 'celldamage'

str(CellDamage)
# Experiment ist als Faktor erkannt - R weiß also, dass es 7 verschiedene Experimenet gabe
# Treatment auch zu einem Faktor

CellDamage$Treatment <- as.factor(CellDamage$Treatment)
str(CellDamage)

plot(CellDamage$Treatment, CellDamage$celldamage)
# schön, aber das wollten wir nicht.. wir wollen non-infected, infected und dann Ca zugabe.
# Lösung: Ordered Faktor:
# Wir geben R also die Hirachie der Level vor

CellDamage$Treatment <- factor(CellDamage$Treatment, levels = c("non-infected",
                                                                "Ca",
                                                                "25 mM + Ca",
                                                                "5 mm + Ca",
                                                                "1 mM + Ca",
                                                                "0.1 mM + Ca"))

str(CellDamage)
# sieht soweit unverändert aus - aber der Plot?
plot(CellDamage$Treatment, CellDamage$celldamage)
# R macht uns jetzt die reihenfolge so, wie wir es wollen.

####10 Celldamage GGPLOT####

# Das Thema ist aus Code Abschnit 07 noch gespeichert (siehe Environment)
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

ggplot(CellDamage)+
  geom_point(aes(x=Treatment, y=celldamage))
# Das Problem sind möglicherweise überplottete Punkte. Besser mit jitter Arbeiten:

ggplot(CellDamage)+
  geom_jitter(aes(x=Treatment, y=celldamage))
#Vorsicht: nicht bei numerischer X Achse Verwenden - Datenfälschung!
# auch bei Faktoren auf der X Achse auf die Breite des Jitters aufpassen

ggplot(CellDamage)+
  geom_jitter(aes(x=Treatment, y=celldamage), width = .2)

# bereitgestellte Aestetics anwenden

ggplot(CellDamage)+
  geom_jitter(aes(x=Treatment, y=celldamage), width = .2)+
  Mein_Thema
# Text auf der X Achse nicht lesbar... 
# Wie wäre es mit drehen?

Mein_Thema_2 <- Mein_Thema+theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))

ggplot(CellDamage)+
  geom_jitter(aes(x=Treatment, y=celldamage), width = .2)+
  Mein_Thema_2

# schon besser, aber wir sind noch nicht am Ziel:
# Die Gruppierung nach infected + uninfected fehlt noch :/

# Ideen, wie wir das machen könnten?

#1. In Excel - Labortabelle  - eine Neue Spalte Hinzufügen: Händisch oder Sverweis
#2. In R mit 2. Tabelle

Behandlung <- data.frame(matrix(ncol = 2, nrow= 6))
# wir haben 6 Behandlungen und weisen jeder Behandlung (1.Spalte) zu, ob infected oder nicht

Behandlung$X1 <- CD$Treatment
Behandlung
# 1. Spalte ist gefüllt
# 2. Spalte bekommt nun einen händisch erstellten Vektor

Infiziert <- c(rep("not infected",2), rep("infected",4))
Infiziert
# diesen Vektor nun in Behandlung einbinden

Behandlung$X2<- Infiziert
Behandlung

# ordentliche Namen
Behandlung <- setNames(Behandlung, c("Treatment", "Infection"))

#Vóila: ein eigenes Datenframe in R erstellt :D

# jetzt verschmelzen wir celldamage und Behandlung miteinander. Das geht, weil wir in beiden eine Spalte "Treatment" haben

CellDamage <- merge(CellDamage, Behandlung)
str(CellDamage)

CellDamage$Treatment <- as.factor(CellDamage$Treatment)
CellDamage$Infection <- as.factor(CellDamage$Infection)

CellDamage$Treatment <- factor(CellDamage$Treatment, levels = c("non-infected",
                                                                "Ca",
                                                                "25 mM + Ca",
                                                                "5 mm + Ca",
                                                                "1 mM + Ca",
                                                                "0.1 mM + Ca"))

str(CellDamage)

my_cols <- c("infected" = "#E61A1A", "not infected" = "#0060FF")
# plotten
ggplot(CellDamage)+
  geom_jitter(aes(x=Treatment, y=celldamage, colour = Infection), width = .2)+
  scale_color_manual(values=my_cols) +
  Mein_Thema_2

# Datenverteilungen Zeigen:

ggplot(CellDamage)+
  geom_boxplot(aes(x=Treatment, y=celldamage), outlier.colour = "green")+  
  geom_jitter(aes(x=Treatment, y=celldamage, colour = Infection), width = .2)+
  scale_color_manual(values=my_cols) +
  Mein_Thema_2

## und noch bisschen hübsch machen
ggplot(CellDamage)+
  geom_boxplot(aes(x=Treatment, y=celldamage), outlier.colour = "green")+  
  geom_jitter(aes(x=Treatment, y=celldamage, colour = Infection), width = .2)+
  scale_color_manual(values=my_cols) +
  scale_y_continuous(limits = c(0,200), expand = c(0, 0))+
  ggtitle("Cell Damage Essay")+  
  ylab("cell damage relative to\n uninfected cells [%]")+
  Mein_Thema_2


####11 Mediane je Treatment berechnen + plotten####

# Was wollen wir plotten --> was müssen wir berechnen?
# prinzipiell können viele Lagemaße angewendet werden: SD, SE, CI... 
# bei vergleichsweise geringen Dazenpunkten, lieber Median + IQR nehmen.
# Die Frage hier war aber nach Mittelwert + SE, also machen wir das:

# Formel für SE:
se <- function(x) sd(x) / sqrt(length(x))

Balken_daten <- CellDamage %>%
  group_by(Treatment) %>%
  summarise(
    Mittelwert = mean(celldamage),
    Standartfehler = se(celldamage)
  )

# erstelle die Balken_daten.
# dazu nutze die CellDamage, grupiert nach Treatment.
# berechne uns Mittelwert und Standartfehler mit den funktionen mean() und se()

Balken_daten <- merge(Balken_daten, Behandlung)
Balken_daten$Treatment <- factor(Balken_daten$Treatment, levels = c("non-infected",
                                                                 "Ca",
                                                                 "25 mM + Ca",
                                                                 "5 mm + Ca",
                                                                 "1 mM + Ca",
                                                                 "0.1 mM + Ca"))

Balken_daten$Infection <- as.factor(Balken_daten$Infection)


ggplot(Balken_daten) +
  geom_bar( aes(x=Treatment, y=Mittelwert), stat="identity", fill="skyblue", width=0.8) +
  geom_errorbar( aes(x=Treatment, ymin=Mittelwert-Standartfehler, ymax=Mittelwert+Standartfehler), width=0.4, colour="orange", size=1.1)+
  Mein_Thema_2





ggplot(Balken_daten) +
  geom_bar(aes(x=Treatment, y=Mittelwert,  fill = Infection), stat="identity", width=0.8) +
  geom_errorbar( aes(x=Treatment, ymin=Mittelwert-Standartfehler, ymax=Mittelwert+Standartfehler), width=0.6, colour="black", size=1.5)+
  geom_jitter(data= CellDamage, aes(x=Treatment, y=celldamage), width = .2, size = .5)+
  scale_color_manual(values=my_cols) +
  scale_y_continuous(limits = c(0,200), expand = c(0, 0))+
  ggtitle("Cell Damage Essay")+  
  ylab("cell damage relative to\n uninfected cells [%]")+
  Mein_Thema_2




ggplot(Balken_daten) +
  geom_bar(aes(x = Treatment, y = Mittelwert, fill = Infection), 
           stat = "identity", show.legend = TRUE, width = 0.8) +
  geom_errorbar(aes(x = Treatment, 
                    ymin = Mittelwert - Standartfehler, 
                    ymax = Mittelwert + Standartfehler), 
                width = 0.6, colour = "black", size = 1.5) +
  geom_jitter(data = CellDamage, aes(x = Treatment, y = celldamage), 
              width = 0.2, size = 0.5) +
  scale_fill_manual(values = c("infected" = "#0060FF", 
                               "not infected" = "#E61A1A"), 
                    name = "Infection") +
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) +
  ggtitle("Cell Damage Essay") +
  ylab("cell damage relative to\n uninfected cells [%]") +
  Mein_Thema_2
