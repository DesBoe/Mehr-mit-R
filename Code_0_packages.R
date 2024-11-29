#### 00. benötigte Pakete herunterladen####

pakete <- c("ggplot2", 
            "dplyr", 
            "tidyr",
            "DBI")  # Liste der Pakete

for (paket in pakete) {
  if (!require(paket, character.only = TRUE)) {
    install.packages(paket)
    library(paket, character.only = TRUE)
  }
}
