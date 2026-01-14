setwd("/Volumes/Dennis_OTG/Bilder/Freunde_Städtedreieck/Jördis btS Rworkshop/2026-01-14")

#### 0 Data handling ####
#download https://pubmed.ncbi.nlm.nih.gov/39773774/
  
#explore appendices: which file to use? table long vs short?

#use S3 as table long --> we will generate the files required 


#### packages ####
library(readxl)
library(vegan)
library(ggplot2)
library(iNEXT)
library(indicspecies)
library(tidyr)
library(dplyr)

#### 1 Species accumulation ####
#Extra_Intra

curves <- read_xlsx("Böttger_2025_et_al__The moth fauna is more diverse in the understorey than in the canopy in a european forest_S3.xlsx",
                    "sheet"= "Extra_Intra")
str(curves)

curves <- as.list(curves)
curves <- curves[!is.na(curves)]

curves$Canopy
curves$Understorey

curves <- iNEXT(curves, q=0, datatype="abundance")

# basic plot
ggiNEXT(curves, type=1)

# expand
ggiNEXT(curves, type=1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))


# shapes 
ggiNEXT(curves, type=1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual(values = c(19,19))


# theme default
ggiNEXT(curves, type=1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual(values = c(19,19)) + 
  theme_bw()

# theme customized
ggiNEXT(curves, type=1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual(values = c(19,19,19)) +
  theme(aspect.ratio = 1:1,
        legend.position = "none", 
        legend.direction = "vertical",
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(size = 10))

# theme colors
ggiNEXT(curves, type=1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_shape_manual(values = c(19,19,19)) +
  scale_fill_manual(values = c("#0D28F2","#43F20D"))+
  scale_color_manual(values = c("#0D28F2","#43F20D"))+
  theme(aspect.ratio = 1:1,
        legend.position = "none", 
        legend.direction = "vertical",
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(size = 10))


#ggsave(filename = "species_accumulation_plot_1.svg", width = 16, height = 16, units = c ("cm"), dpi = 350)



#### 2 Indicator species ####
#vegan_long

Catch <- read_xlsx("Böttger_2025_et_al__The moth fauna is more diverse in the understorey than in the canopy in a european forest_S3.xlsx",
                "sheet"= "vegan_long")
    
IndexGesammt= multipatt(Catch[,4:ncol(Catch)], Catch$Stratum,
                   control = how(nperm=999))

str(Catch[,4:ncol(Catch)])

summary(IndexGesammt)


#### 3 Composition ####
#vegan_nmds

Catch <- read_xlsx("Böttger_2025_et_al__The moth fauna is more diverse in the understorey than in the canopy in a european forest_S3.xlsx",
                "sheet"= "vegan_nmds")



str(Catch)


#Catch$Date_start <- format(Catch$Date_start, "%m-%d")
str(Catch)

matrix.nmds_2 = data.matrix(Catch[ , 4:ncol(Catch)])
(all_NMDS_2 <- metaMDS(matrix.nmds_2, distance = "bray", k=2, autotransform = FALSE))
plot(all_NMDS_2)
  
stressplot(all_NMDS_2)
data.scores_2 = as.data.frame(scores(all_NMDS_2$points))
data.scores_2$Location = Catch$Site
data.scores_2$Stratum = Catch$Stratum
data.scores_2$date=Catch$Date_start

centroids <- data.scores_2 %>% group_by(Stratum) %>%
  summarise(NMDS1= mean(MDS1), NMDS2= mean(MDS2), .groups= "drop")

print(data.scores_2)

hull.c <- data.scores_2[data.scores_2$Stratum == "Canopy", ][chull(data.scores_2[data.scores_2$Stratum == 
    "Canopy", c("MDS1", "MDS2")]), ]

hull.u <- data.scores_2[data.scores_2$Stratum == "Understorey", ][chull(data.scores_2[data.scores_2$Stratum == 
    "Understorey", c("MDS1", "MDS2")]), ]

hull.data <- rbind(hull.c, hull.u)

print(hull.data)


(NMDS_2 <- ggplot(data = data.scores_2, aes(x=MDS1, y=MDS2)) +

    geom_polygon(data=hull.data, aes(x=MDS1, y=MDS2, fill=Stratum, group=Stratum), alpha=0.30, show.legend = T) +
    scale_fill_manual(values = c("Canopy" = "#0D28F2", "Understorey" = "#43F20D")) +
    geom_point(data = data.scores_2, size= 5, aes(color= Stratum, shape= Location),   show.legend = T)+
    scale_color_manual(values = c("Canopy" = "#0D28F2", "Understorey" = "#43F20D")) +
    geom_point(data= centroids, aes(x= NMDS1, y= NMDS2),shape= 18, size= 5, color= c("#0D28F2","#43F20D"),show.legend = F )+
    geom_text(data=data.scores_2,aes(x=MDS1,y=MDS2, label= date),
              hjust= -0.55, vjust= 0.1,
              colour = "black", size = 4) +
    theme(aspect.ratio = 1:1,
        panel.background = element_blank(),
        legend.position= "right", 
        panel.grid.major = element_line(colour = "black", linewidth = 0.3),
        panel.grid.minor = element_line(colour = "black", linewidth = 0.1),
        legend.text= element_text(size=10, color= "black"),
        legend.key = element_blank(),
        axis.text.x= element_text(size=12, color= "black"), 
        axis.text.y= element_text(size=12, color= "black")))


#ggsave(filename = "NMDS_plot_1.svg", plot = NMDS_2, width = 20, height = 16, units = c ("cm"), dpi = 350)






anoAllSites <- anosim(matrix.nmds_2, data.scores_2$Location, distance = "bray", permutations = 9999)
anoAllSites

# note: signifance is slightly different to paper as we do not correct for multiple testing here







#### 4 diversity metrics ####
#vegan_long

Index <- read_xlsx("Böttger_2025_et_al__The moth fauna is more diverse in the understorey than in the canopy in a european forest_S3.xlsx",
                    "sheet"= "vegan_fisher_alpha")


str(Index)
summary(Index)
(fish <- fisher.alpha(Index[,2:ncol(Index)]))
(shannon <- diversity(Index[,2:ncol(Index)], index = "shannon"))


Catch <- read_xlsx("Böttger_2025_et_al__The moth fauna is more diverse in the understorey than in the canopy in a european forest_S3.xlsx",
                "sheet"= "vegan_long")
str(Catch)
(metric_C <-specpool(Catch[Catch$Stratum == "Canopy", 4:ncol(Catch)], smallsample = TRUE))
(metric_U <-specpool(Catch[Catch$Stratum == "Understorey", 4:ncol(Catch)], smallsample = TRUE))

result <- rbind(metric_C, metric_U)
result <- cbind(result, fish, shannon)

result

write.csv(result, "Diversity_metrics_Canopy_Understorey.csv", row.names = TRUE)
