# Codice R per analisi di immagini satellitari

# packages: raster, RStoolbox
install.packages("raster")
library(raster)
install.packages("RStoolbox")
library(RStoolbox)

# per entrare nella cartella di lavoro
setwd("C:/Lab")

# per caricare dati dall'esterno e associarli ad una certa immagine si utilizza la funzione brick
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# per plottare l'immagine 
plot(224r63_2011)
