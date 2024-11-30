#### 00. benötigte Pakete herunterladen####

pakete <- c("xlsx", 
            "readxl", 
            "ggplot2",
            "data.table",
            "dplyr")

for (paket in pakete) {
  if (!require(paket, character.only = TRUE)) {
    install.packages(paket)
    library(paket, character.only = TRUE)
  }
}
