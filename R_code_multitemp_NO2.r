# R code per un'analisi multitemporale dei dati NO2

setwd("C:/Lab") # windows

# richiamare la libreria raster
library(raster)

# importare e visualizzare l'immagine EN_0001.png
EN01 <- raster("EN_0001.png")
plot(EN01)

# Exercise: importare tutte le altre immagini EN 
# possibile utilizzare anche un "ciclo for" che importa automaticamente le immagini richieste
EN01 <- raster("EN_0001.png")
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

# per introdurre una palette di colori 
cl <- colorRampPalette(c('red','orange','yellow'))(100) 

# per plottare l'immagine iniziale (EN01) e l'immagine finale (EN13)
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

dev.off()

# differenza fra EN13 ed EN01
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)

# per plottare tutte le immagini
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

 
