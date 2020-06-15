# R CODE PER L'ESAME DI ECOLOGIA DEL PAESAGGIO (UNIBO)

### INDICE
# 1. R_code_primocode.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_patterns.r  
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r  
# 11. R_code_crop.r
# 12. R_code_sdm.r

##############################################
##############################################
##############################################

### 1. R CODE PRIMOCODE

# PACCHETTI UTILIZZATI: "sp"

# PER IMPORTARE IN R IL PACCHETTO "sp"
# install.packages("sp") # PER INSTALLARE IL PACCHETTO "sp"
library(sp) # IN ALTERNATIVA UTILIZZARE require(sp) 

# FUNZIONE data() PER VISUALIZZARE I DATA SET (IN QUESTO CASA IL DATA SET "meuse")
data(meuse)
meuse # PER VISUALIZZARE I VALORI DEL DATA SET RELATIVI ALLA PRESENZA IN SUPERFICIE DI 4 METALLI PESANTI SULLE SPONDE DEL FIUME MEUSE

# FUNZIONE head() PER VISUALIZZARE SOLAMENTE LE PRIME RIGHE DEL DATA SET
head(meuse)
# FUNZIONE names() PER VISUALIZZARE SOLO I NOMI DELLE VARIABILI
names(meuse)

# FUNZIONE summary() PER VISUALIZZARE INFORMAZIONI ULTERIORI RELATIVE AL DATA SET 
summary(meuse)

# FUNZIONE pairs() CONSENTE DI OTTENERE UN GRAFICO, RISULTATO DELLA MATRICE DI CORRELAZIONE FRA LE VARIABILI DEL DATA SET
pairs(meuse)

# PER VISUALIZZARE SOLO ALCUNE DELLE VARIABILI pairs(~ variabile1 + variabile2 + variabile3, data = meuse)
# LA VIRGOLA ASSUME LA FUNZIONE DI SEPARATORE FRA GLI ARGOMENTI 
# L'ARGOMENTO "data" SERVE A SELEZIONARE IL DATA SET DI RIFERIMENTO
pairs(~ cadmium + copper + lead , data = meuse)

# NOTE: IN R SI POSSONO RICHIAMARE FUNZIONI PRECEDENTI UTILIZZANDO LA FRECCIA IN ALTO
# permette di ridurre i passaggi


# ESERCIZIO: aggiungere "zinc"

# IN QUESTO CASO SI PRENDE UN subset ("[]") partendo da ","colonna 3 a (":") 6 (si ottiene lo stesso grafico)

# PER CAMBIARE COLORE SI INTRODUCE NELLA FUNZIONE col="colore scelto" (es. col="red"), PRECEDUTO DALLA VIRGOLA PER SEPARARE GLI ARGOMENTI
pairs(meuse[,3:6], col="blue")

# PER CAMBIARE LA FORMA DEI PUNTI DEL GRAFICO: pch="numero corrispondente alla forma prescelta" (es. pch=20)
# NOTE: CERCARE SU GOOGLE I NUMERI IN R CORRISPONDENTI ALLA FORME DEI PUNTI
pairs(meuse[,3:6], col="blue", pch=20)

# PER CAMBIARE LE DIMENSIONI DEL PUNTO: "cex" (character exageration) (es. cex=3)
pairs(meuse[,3:6], col="blue", pch=20, cex=3)

# PER CAMBIARE NOME AL GRAFICO: main="nome grafico" (es. main="Primo pairs") 
pairs(meuse[,3:6], col="blue", pch=20, cex=3, main="Primo pairs")

# ESERCIZIO: inserire "elevation" (settima variabile)
pairs(meuse[,3:7], col="blue", pch=20, cex=3, main="Primo pairs")

# FUNZIONE source PER INSERIRE FILE DALL'ESTERNO
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=white", ...)
}
# LA PRIMA FUNZIONE "correlation" ci dà le correlazioni 
# LA FUNZIONE "smoothing" ci darà la linea di correlazione 
# LA FUNZIONE "histogram" fa l'istogramma in mezzo alle variabili
# "lowess" è smoother locale

# "lower.panel" è la parte inferiore del grafico pairs ed è possibile decidere cosa metterci ad esempio le correlazioni
# "upper.panel" è parte superiore e ci metto lo smoothing, ossia il grafico dei punti con le linee di correlazione
# "diag.panel" è diagonale e ci metto gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

plot(meuse$cadmium, meuse$copper)
#fa errore

# per costruire un grafico solo con le variabili "cadmium" e "copper"
attach(meuse)
plot(cadmium, copper)

# PER CAMBIARE LE ETICHETTE SUL GRAFICO: xlab="variabile1" ylab="variabile2" (es. xlab="cadmio, ylab="rame") 
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame")

# PER AUMENTARE I CARATTERI: cex.lab=1.5
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame", cex.lab=1.5, cex=2)


##############################################
##############################################
##############################################


### 2. R CODE SPATIAL

# CODICE PER FUNZIONI SPAZIALI IN ECOLOGIA DEL PAESAGGIO

# PACCHETTI UTILIZZATI: "sp", "GGally",

# install.pachages("sp")   
# install.packages("GGally")
library(sp)
library(GGally)


# FUNZIONE data() PER RICHIAMARE IL DATA SET "meuse" 
data(meuse)

# FUNZIONE head() PER RICHIAMARE SOLO LE PRIME RIGHE DEL DATA SET
head(meuse)

# PLOTTARE "cadmium" E "lead"

# FUNZIONE attach() PER ALLEGARE IL DATA SET 
attach(meuse)

# FUNZIONE plot() PER LA RAPPRESENTAZIONE GRAFICA
plot(cadmium,lead,col="red",pch=19,cex=2)

# ESERCIZIO: plot di copper e zinco con simbolo triangolo(17) e colore verde
plot(copper,zinc,col="green",pch=17)

# PER CAMBIARE LE ETICHETTE DEL GRAFICO: xlab="variabile1" e ylab="variabile2"
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# FUNZIONE par(mfrow=c(numero righe, numero colonne)) PER CREARE MULTIFRAME O MULTIPANEL [es. par(mfrow=c(1,2))]
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

# PER INVERTIRE I GRAFICI riga/colonna IN colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

# MULTIFRAME AUTOMATICO 
# FUNZIONE ggpairs COSTRUISCE UNA MATRICE DI PLOTS DA UN DETERMINATO DATA SET
ggpairs(meuse[,3:6])

# SPAZIALIZZAZIONE

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

# FUNZIONE spplot() PER PLOTTARE SPAZIALMENTE I DATI 
spplot(meuse,"zinc")


##############################################
##############################################
##############################################

### 3. R code spatial 2

# per installare la libreria sp
install.packages("sp")

# per richiamare la libreria sp
library(sp)

# per richiamare il data set meuse
data(meuse)

# per richiamare solo le prime righe del data set usare la funzione head
head(meuse)

# per ottenere le coordinate del data set
coordinates(meuse)=~x+y

# spplot dei dati dello zinco
spplot(meuse,"zinc")

# Exercise: spplot dei dati del rame

# per richiamare i nomi degli argomenti si possono utilizzare due differenti funzioni
head(meuse)
name(meuse)

# spplot dei dati del rame 
spplot(meuse,"copper")

# funzione bubble
bubble(meuse,"zinc")

# Exercise: bubble del rame, colore rosso
bubble(meuse,"zinc",col="red")

# Exercise:
# prendiamo dei dati realtivia a formanifireri (Sofia) e al carbon capture (Marco)
# array 
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# plottiamo i dati per valutare le correlazioni 

# Dati dall'esterno sul covid-19
# Cartella da creare su Windows: C:/Lab, su Linux:~/Lab
setwd("C:/Lab") # Windows

# per leggere una tabella funzione
read.table("covid_agg.csv"),head=TRUE)

# per associare la lettura della tabella a covid
covid <- read.table("covid_agg.csv"),head=TRUE)

##############################################
##############################################
##############################################

### 4. R code point patterns 

# codice per analisi dei point patterns

insall.packages("ggplot2")
library(ggplot2)
install.packages("spatstat")
library(spatstat)
install.packages("rgdal")
library(rgdal)

# per entrare nella cartella di lavoro 
setwd("C:/Lab")

# per importare dati
covid <- read.table("covid_agg.csv", head=TRUE)

# per visualizzare le prime righe della tabella utilizzare la funzione head
head(covid)

# plottiamo i primi dati. Per collegare le colonne al proprio data set si utilivva il $ o la funzione attach
attach(covid)
plot(covid$country, covid$cases)

plot(covid$country, covid$cases, las=0) # parallel labels
plot(covid$country,covid$cases,las=1) # horizontal labels
plot(covid$country,covid$cases,las=2) # perpendicular labels
plot(covid$country,covid$cases,las=3) # vertical labels

# per modificare le labels si utilizza l'argomento cex.axis
plot(covid$country,covid$cases,las=3, cex.axis=0,5) 

# per richiamare il pacchetto ggplot e i dati mpg
library(ggplot2)
data(mpg)
head(mpg)

# data
# aes
# tipo di geometria 
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# per cambiare il tipo di geometria 
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()

# ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# per creare una mappa di densità di covid 

# creare un data set per spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# densità
d <- density(covids)

plot(d)
points(covids)

### salvare il .RData

# richiamare la cartella di lavoro
setwd("C:/Lab")
load("covid.RData")
ls()
library(spatstat)
plot(d)

# palette
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d, col=cl)

# Exercise: plot della mappa della densità da verde a blu
cl2 <- colorRampPalette(c('green','blue')) (200)
plot(d, col=cl2)

# per richiamare i punti dei casi di covid
points(covids)

# per inserire gli shapefile delle coste
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=TRUE)

# Exercise: plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl3 <- colorRampPalette(c('green','yellow','red')) (200)
plot(d, col=cl3)
points(covids)
plot(coastlines, add=TRUE)

### Exercise: caricare in R il workspace covid.RData (funzione load("...")) e creare una mappa di densità

setwd("C:/Lab")
load("covid.RData")
ls()
library(spatstat)
plot(d, main="densità covid-19")
points(covids)

# per inserire gli shapefiles delle coste 
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=TRUE)

# interpolazione punti covid-19
head(covid)
marks(covids) <- covid$cases
s <- Smooth(covids)
plot(s)

# Exercise: plottare s con diversa color palette, punti covids e coastlines shapefiles
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="stima dei casi covid-19")
points(covids)
plot(coastlines, add=T)
# per vederi i valori dei punti sulla mappa
text(covids)

### Mappa finale 
par(mfrow=c(2,1))

# densità 
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="densità covid-19")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="stima dei casi")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

### DATI San Marino (caso di studio)

setwd("C:/Lab")
#richiamare la libreria spatstat
library(spatstat)
load("Tesi.RData")
ls()
head(Tesi)
attach(Tesi)

summary(Tesi)
# x varia da 12.42 a 12.46
# y varia da 43.91 a 43.94

# point pattern: x, y, z
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.90,43.95))

# densità 
dT <- density(Tesippp)
plot(dT)
points(Tesippp)

dev.off()

# 28/04
setwd("/Users/enricopriarone/lab")
load("Tesi.RData")
ls()
# Ottengo i file presenti:
# "dT" è density map di Tesippp
# "Tesi" è un dataset
# "Tesippp" è il point pattern del file "Tesi" (da libreria "Spatstat")

# Associamo i valori che vogliamo stimare
library(spatstat)
plot(dT)
points(Tesippp, col="green")

# Andiamo a stimare la ricchezza specifica
# Con "head" vedo che si trova sotto "Species_richness"
# "marks" va a prendere i valori dalla tabella e li associa ai punti del ppp
head(Tesi)
marks(Tesippp) <- Tesi$Species_richness

# Creo mappa e l'associo a Smooth
interpol <- Smooth(Tesippp)

# Usiamo file su San Marino
# Carico libreria "rgdal"
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T) # Importante! Con "T"/"True" aggiunge nuova mappa a quella precedente
points(Tesippp,col="green")
# Mappa va a sovrapporsi al territorio di San Marino
plot(sanmarino, add=T) # Così i confini di Stato si sovrappongono al plot "interpol"

# Esercizio: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

# Esercizio: inverto la disposizione del grafico
par(mfrow=c(1,2))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

dev.off()

##############################################
##############################################
##############################################

### 5. R code teleril 

# Codice R telerivelamento per analisi di immagini satellitari 

# pacchetti utilizzati: raster, RStoolbox
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
    

##############################################
##############################################
##############################################

### 6. R CODE LAND COVER

# CODICE R PER ANALISI DELLA COPERTURA DI SUOLO 

setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)

# Uso funzione che impila e importa i dati
p224r63_2011 <- brick("p224r63_2011_masked.grd")
install.packages("RStoolbox")
library(RStoolbox)

# Faccio RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# Accorpa i pixel in quattro classi:
# Ottengo un vero e proprio modello
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# Visualizzo le informazioni
p224r63_2011c

# Plotto la mappa
plot(p224r63_2011c$map)

# Stabiliamo noi una legenda
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# Provo con due classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
clclass2 <- colorRampPalette(c('red', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass2)
dev.off()

# In funzione del numero di classi aumenta l'incertezza dell'algoritmo auomatico di classificazione
# riportando potenzialmente classi leggermente differenti
# Con 2 classi l'incertezza è più bassa che con 4

##############################################
##############################################
##############################################

### 7. R CODE MULTITEMP
    
# CODICE R PER ANALISI MULTITEMPORALE DELLA VARIAZIONE DEL LAND COVER ATTRAVERSO IMMAGINI TELERILEVATE 

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

##############################################
##############################################
##############################################
    
### 8. R CODE MULTITEMP NO2

# R CODE PER ANALISI MULTITEMPORALE DEI DATI DI NO2 OTTENUTI DA IMMAGINI ELABORATE DAL PROGETTO COPERNICUS 

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

### Giorno 2

library(raster)

# entrare nella cartella esa_no2 all'interno della cartella Lab
setwd("C:/lab/esa_no2")

# per assegnare a rlist la lista di file .png
rlist <- list.files(pattern=".png")
rlist

# 
listafinale <- lapply(rlist, raster)

#
EN <- stack(listafinale)

# scorporare lo stack per andare a fare un'operazione di differenza fra due immagini
difEN <- EN$EN_0013 - EN$EN_0001

cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

boxplot(EN, horizontal=T)

# per eliminare gli out layers
boxplot(EN, horizontal=T,outline=F,axes=T) # axes=T argomento di defoult riguardante gli assi del grafico
    
##############################################
##############################################
##############################################
 
# 9. R CODE SNOW

# CODICE R PER ANALISI MULTITEMPORALE DELLA COPERTURA NEVOSA ATTRAVERSO IMMAGINI TELERILEVATE

# pacchetti necessari
install.packages("ncdf4")
library(ncdf4)
install.packages("raster")
library(raster)

setwd("C:/Lab/") # windows

# caricare l'immagine raster 
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# stabilire una color ramp 
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# plottare l'immagine snowmay con la color ramp cl
plot(snowmay, col=cl)

### Import snow data

setwd("C:/Lab/snow/") # windows

rlist=list.files(pattern=".tif", full.names=T)

# inserire tutti i file in una lista con lappy
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
snow.multitemp

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snow.multitemp, col=cl)

dev.off()

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

# plottare la differenza fra snow2020 e snow2000
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

### Per caricare su R un intero script dall'esterno
# scaricare il file prediction.r da IOL e portarlo nella cartella snow
source("prediction.r")

# scaricare il file predicted.snow.2025.norm da IOL
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
    
##############################################
##############################################
##############################################

### R CODE PATCHES 
    
# CODICE R PER ANALISI DELLA FRAMMENTAZIONE DEGLI ECOSISTEMI

install.packages("raster")
library(raster)
install.packages("igraph")
library(igraph)
install.packages("ggplot2")
library(ggplot2)

# per entrare nella cartella di lavoro
setwd("C:/Lab/") # windows

# per importare le due immagini
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# plottare le due immagini 
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

# riclassificazione della prima immagine
# land cover 1= agriculture; land cover 2=forest
d1c.for <- reclassify(d1c, cbind(1, NA))

# plottare l'immagine riclassificata con l'immagine originale
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for,col=cl)

dev.off()

# riclassificazione della seconda immagine
d2c.for <- reclassify(d2c, cbind(1, NA))

# plottare le due immagini riclassificate 
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c.for,col=cl)
plot(d2c.for,col=cl)

dev.off()

# funzione clump aggrage i pixels vicini per formare un'unica patche
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# plottare le due immagini riclassificate 
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c.for.patches,col=cl)
plot(d2c.for.patches,col=cl)

# per salvare il raster nella cartella di lavoro
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

#Exercise: plottare con un'altra color palette
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# max patches d1 = 301
# max patches d2 = 1212

# analisi multitemporale del numero di patches
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

    
##############################################
##############################################
##############################################

### 11. R code crop

# Crop an image

install.packages("raster")
library(raster)
install.packages("RStoolBox")
library(RStoolbox)

setwd("C:/Lab/") # per windows

rlist <- list.files(pattern="snow")
rlist 

#save raster into list
#con lappy
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 

# plot to see italian coordinates

snow.multitemp

plot(snow.multitemp$snow2010r, col=clb)

# zoom con extent
extension <- c(6, 18, 40, 50)

# zoom
zoom(snow.multitemp$snow2010r, ext=extention)
    
##############################################
##############################################
##############################################
    
### 12. R CODE SDM 

# CODICE R PER LO SPECIES DISTRIBUTION MODELLING
    
### 12. Species Distribution Modelling

# install.packages("sdm")
# install.packages("rgdal")
library(sdm)
library(raster)
library(rgdal)

# species
file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)

species
species$Occurrence
plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)

points(species[species$Occurrence == 0,],col='red',pch=16)

# environmental variables
path <- system.file("external", package="sdm") 

lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

preds <- stack(lst)

cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# model

d <- sdmData(train=species, predictors=preds)
d

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') 
p1 <- predict(m1, newdata=preds)

plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)
    
##############################################
##############################################
##############################################
