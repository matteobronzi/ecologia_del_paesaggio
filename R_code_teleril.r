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
