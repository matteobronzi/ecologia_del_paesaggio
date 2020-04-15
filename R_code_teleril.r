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
plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# save .RData

# per ricaricare il file .RData utilizzare la funzione load
load("teleril.RData)
ls()
library(raster)
plot(p224r63_2011)

# per cambiare la palette di colori dell'immagine in scala di grigi
cl <- colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011, col=cl)

# plottare l'immagine con una palette differente
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 

# la funzione attach(dataframe) non è utilizzabile con il pacchetto raster
# simbolo che lega la colonna (la banda) al dataset (immagine satellitare):
plot(p224r63_2011$B1_sre, col=clb)

#Exercise: plottare la banda del NIR con colorRampPalette che varia dal rosso all'arancione al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# multiframe
par(mfrow=c(2,2))

 # blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_2011$B1_sre, col=clb)
# green
clg <- colorRampPalette(c('dark green','green','light green'))(100) # 
plot(p224r63_2011$B1_sre, col=clg)
# red
clr <- colorRampPalette(c('dark red','red','pink'))(100) # 
plot(p224r63_2011$B1_sre, col=clr)
# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

dev.off()

# natural colors
# tre componenti: R G B
# tre bande: R= banda del rosso, G= banda del verde, B= banda del blu
plotRGB(p224r63_2011, r=3, g=2, b=1)
# plotRGB: no!
# stretch dei colori con tipologia Lineare "Lin"
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# false colours (andiamo ad introdurre la banda nir, non visibile ad occhio nudo)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# salvataggio pdf immagine
pdf("primo_grafico")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#Exercise: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
#Exercise: nir nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# Day 2

# per entrare nella cartella di lavoro
setwd("C:/lab/") # windows
# setwd("~/lab/") # linux
# setwd("/Users/nome/Desktop/lab") # mac

# richiamare libreria raster
library(raster)

# richiamare l'RData con l'area di lavoro 
load("teleril.RData")

# list
ls()

# per importare l'immagine satellitare utilizzare la funzione brick
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# per plottare l'immagine satellitare
plot(p224r63_1988)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# multiframe
par(mfrow=c(2,2))

 # blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_1988$B1_sre, col=clb)
# green
clg <- colorRampPalette(c('dark green','green','light green'))(100) # 
plot(p224r63_1988$B1_sre, col=clg)
# red
clr <- colorRampPalette(c('dark red','red','pink'))(100) # 
plot(p224r63_1988$B1_sre, col=clr)
# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir)

# per chiudere la finestra
dev.off()

# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4

# natural colors
# tre componenti: R G B
# tre bande: R= banda del rosso, G= banda del verde, B= banda del blu
plotRGB(p224r63_1988, r=3, g=2, b=1)
# plotRGB da sola non funziana, va aggiuto l'argomento stretch
# stretch dei colori con tipologia Lineare "Lin"
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Exercise: plottare l'immagine usando il sensore nir al posto della componente r nello spazio RGB
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# plottare le immagini del 1988 e del 2011 per analizzare i cambiamenti
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# spectral indices
# dvi1988 = nir1988 - red1988
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

# plottare l'indice dvi1988
plot(dvi1988)

# Exercise: plottare l'indice dvi per il 2011
# dvi2011 = nir2011 - red2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

# plottare l'indice dvi2011
plot(dvi2011)

dev.off()

# cambiare la palette di colori
cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 
plot(dvi2011, col=cldvi)

# multitemporal analysis
difdvi <- dvi2011 - dvi1988
plot(difdvi)

#cambiare la palette di colori 
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)  
plot(difdvi, col=cldifdvi)

dev.off()

# visualizzare gli outputs
# multiframe 1988rgb, 2011rgb, difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# ricampionare l'immagine del 2011
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# plottare le due immagini a diversa risoluzione
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

dev.off()

# lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
# original 30m <- resampled 1500m 

# plottare le tre immagini con risoluzioni diverse
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dev.off()

# calcolare il dvi2011lr50
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
plot(dvi2011lr50)

dev.off()

# ricampionare l'immagine del 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)

# calcolare il dvi1988lr50
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
plot(dvi1988lr50)

# multitemporal analysis lr50 (low resolution)
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50)

# plottare la difdvilr50 con la color palette "cldifdvi"
plot(difdvilr50,col=cldifdvi)

dev.off()

# plottare la difdvi (ad alta definizione) con la difdvilr50 (a bassa risoluzione)
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

