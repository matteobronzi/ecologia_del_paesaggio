# R code analisi multitemporale di variazione della land cover
# Copertura del suolo viene da telerilevazioni

setwd("/Users/enricopriarone/lab")
install.packages("Rcmdr")
library(raster)
library(RStoolbox)
library(ggplot2)

# per caricare l'immagine satellitare da cartella "lab" attraverso funzione di raster "brick"
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
# associare la banda del rosso a NIR, verde a rosso e blu a verde
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# per usare due classi non supervisionate: non spieghiamo al computer la divisione
# Computer raggruppa pixel che sembrano simili tra loro
d1c <- unsuperClass(defor1, nClasses=2)
d1c # Visualizzo i suoi dettagli

# per creare la mappa
plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

# esempio su significato dollaro
# mappageologica <- geomap(im_sat, nClasees=...)
# plot (mappageologica$lito)
# plot(mappageologica$lineaments)

# Exercise: classificare con due classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
d2c
plot(d2c$map)
plot(d2c$map, col=cl)

dev.off()

# per fare un nuovo grafico delle due carte ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# per visualizzare la suddivisione dei pixel nelle classi
freq(d1c$map)
# aree altre: 35516
# foresta: 305776

totd1 <- 305776 + 35516 # Numero pixel carta
totd1
# 341292

# per calcolare le proporzioni, la percentuale delle frequenze
percent1 <- freq(d1c$map)*100/totd1
percent1

# percentuali:
# aree altre: 10.4
# foreste: 89.6

# fare lo stesso procedimento per la carta 2
freq(d2c$map)
# aree altre: 164321
# foreste: 178405

totd2 <- 164321 + 178405 # Numero pixel carta
totd2
# 342726

percent2 <- freq(d2c$map)*100/totd2
percent2

# aree altre: 48
# foreste: 52

# per creare un dataframe con i dati
cover <- c("Agriculture","Forest")
before <- c(10.4,89.6)
after <- c(48,52)
output <- data.frame(cover,before,after)
View(output)

dev.off()



### Giorno 2 

install.packages("gridExtra")
library(gridExtra)

setwd("C:/lab/") # windows

#caricare l'area di lavoro 
load("defor.RData")
ls()
library(raster)
 
# plottare le due mappe 
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# copertura 
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

output <- data.frame(cover,before,after)
output

# richiamare la libreria ggplot2
library(ggplot2)

# istogramma della % di copertura prima della deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

dev.off()

# Exercise: fare lo stesso procedimento per il dopo deforestazione ("after") 
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# assegnare un nome agli istogrammi 
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# Exercise: utilizzare la funzione grid.arrange(plot1, plot2, nrow=n) per unire i due grafici
grid.arrange(grafico1, grafico2, nrow=1)
