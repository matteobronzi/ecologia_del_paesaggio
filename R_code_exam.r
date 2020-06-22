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
# 13- R_code_ecuador.r

# dati da Copernicus: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html


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

### 3. R CODE SPATIAL 2 

# PACCHETTI UTILIZZATI: "sp"

# install.packages("sp")
library(sp)

# FUNZIONE data() PER RICHIAMARE IL DATA SET meuse
data(meuse)

# FUNZIONE head() PER RICHIAMARE SOLO LE PRIME RIGHE DEL DATA SET
head(meuse)

# FUNZIONE coordinates(data set)=~x+y PER OTTENERE LE COORDINATE DEL DATA SET 
coordinates(meuse)=~x+y

# PER CREARE UN spplot DEI DATI RELATIVI ALLO ZINCO
spplot(meuse,"zinc")

# ESERCIZIO: spplot dei dati del rame

# PER RICHIAMARE I NOMI DEGLI ARGOMENTI SI POSSONO UTILIZZARE DUE DIFFERENTI FUNZIONI: head() o name()
head(meuse)
name(meuse)

# spplot DEI DATI DEL RAME
spplot(meuse,"copper")

# FUNZIONE bubble() PER GENERARE UN "BUBBLE PLOT"
bubble(meuse,"zinc")

# ESERCIZIO: bubble del rame, colore rosso
bubble(meuse,"zinc",col="red")

# ESERCIZIO: inserire dei dati realtivi a due casi di studio: formanifireri (Sofia) e al carbon capture (Marco)
# CREARE UN ARRAY
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# PLOTTARE I DATI PER VALUTARE LE CORRELAZIONI 

# DATI COVID-19

# IMPORTARE DATI COVID DALL'ESTERNO
    
# CREARE UNA NUOVA CARTELLA E RICHIAMARE LA WORKING DIRECTORY
# SU Windows: C:/Lab
# SU Linux: ~/Lab
# SU Mac: 
setwd("C:/Lab") # Windows

# FUNZIONE read.table("") PER LEGGERE UNA TABELLA
read.table("covid_agg.csv"),header=TRUE)

# ASSOCIARE LA TABELLA A covid
# L'ARGOMENTO header=T VUOLE DIRE CHE NELLA PRIMA RIGA DEL FILE SONO CONTENUTI I NOMI DELLE VARIABILI
covid <- read.table("covid_agg.csv"),header=TRUE)

##############################################
##############################################
##############################################

### 4. R CODE POINT PATTERNS

# CODICE R PER L'ANALISI SPAZIALE DI PUNTI (POINT PATTERN ANALYSIS)
    
# PACCHETTI UTILIZZATI: "ggplot2", "spatstat", "rgdal"

#insall.packages("ggplot2")
#install.packages("spatstat")
#install.packages("rgdal")
library(ggplot2)
library(spatstat)
library(rgdal)

# PER ENTRARE NELLA CARTELLA DI LAVORO (Lab)
setwd("C:/Lab")

# PER IMPORTARE DATI RELATIVI AL COVID-19 
# L'ARGOMENTO header=T VUOLE DIRE CHE NELLA PRIMA RIGA DEL FILE SONO CONTENUTI I NOMI DELLE VARIABILI
covid <- read.table("covid_agg.csv", header=TRUE)

# FUNZIONE head() PER VISUALIZZARE SOLO LE PRIME RIGHE DELLA TABELLA 
head(covid)

# PLOTTARE I PRIMI DATI
# PER COLLEGARE LE COLONNE AL PROPRIO DATA SET SI UTILIZZA IL "$" (dollaro) O LA FUNZIONE attach()
attach(covid)
plot(covid$country, covid$cases)

plot(covid$country, covid$cases, las=0) # parallel labels
plot(covid$country,covid$cases,las=1) # horizontal labels
plot(covid$country,covid$cases,las=2) # perpendicular labels
plot(covid$country,covid$cases,las=3) # vertical labels

# PER MODIFICARE LE labels (etichette) SI UTILIZZA L'ARGOMENTO "cex.axis" (es. cex.axis=0,5)
plot(covid$country,covid$cases,las=3, cex.axis=0,5) 

# UTILIZZIAMO IL DATA SET "mpg" CONTENUTO ALL'INTERNO DEL PACCHETTO "ggplot2"
# IL DATA SET "mpg" CONTIENE DATI RELATIVI AL RISPARMIO DI CARBURANTE NEGLI ANNI 1999 E 2008 PER 38 MODELLI DI AUTO POPOLARI
data(mpg)
head(mpg)

# FUNZIONE ggplot() PER CREARE GRAFICI CON IL PACCHETTO "ggplot2"
# PER GENERARE UN GRAFICO CON ggplot() BISOGNA INDICARE: MAPPATURA (aes) E GEOMETRIA (geom) DELLE VARIABILI
# aes() (aesthetic) DESCRIVE L’"ESTETICA" CON CUI VENGONO ASSEGNATE LE VARIABILI AGLI ASSI ("x=" e "y=" ARGOMENTI DELLA FUNZIONE aes)
# geom_ SPECIFICA COSA VOGLIAMO RAPPRESENTARE (es. geom_point)
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# PER CAMBIARE IL TIPO DI GEOMETRIA 
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()

# ggplot DEI DATI COVID-19
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# CREARE UNA MAPPA DI DENSITA' DEI DATI COVID-19

# CREARE UN DATA SET PER spatstat
# FUNZIONE "ppp" PER RAPPRESENTARE UN DATA SET DI UN POINT PATTERN IN UN PIANO BIDIMENSIONALE
# GLI ARGOMENTI "lon" (longitudine) e "lat" (latitudine) RAPPRESENTANO I VETTORI DELLE COORDINATE (x;y)
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

# ASSOCIARE LA FUNZIONE density() (per il calcolo della densità dei punti covid) A "d"
d <- density(covids)

# PLOTTARE "d"
# FUNZIONE points() PER VISUALIZZARE I PUNTI RELATIVI AI DATI SPAZIALIZZATI DEL DATA SET COVID-19
plot(d)
points(covids)

# SALVARE L'.Rdata

### RICHIAMARE L'.RData
    
# RICHIAMARE LA CARTELLA DI LAVORO (Lab)
setwd("C:/Lab")
load("covid.RData")
ls()
# RICHIAMARE IL PACCHETTO "spatstat"
library(spatstat)
plot(d)

# FUNZIONE colorRampPalette() PER GENERARE UNA PALETTE DI COLORI DA UTILIZZAR NEL PLOT
# INSERIRE UN ARRAY DI COLORI COME ARGOMENTO DELLA FUNZIONE colorRampPalette()
# FUORI DALLA FUNZIONE FRA PARENTESI SI AGGIUNGE IL NUMERO DI CLASSI IN CUI SI VOGLIONO SUDDIVIDERE I DATI (es. (100))
# ASSEGNARE LA colorRampPalette() A "cl"
# PER PLOTTARE d CON LA colorRampPalette CREATA: col= cl
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d, col=cl)

# ESERCIZIO: plot della mappa della densità da verde a blu
cl2 <- colorRampPalette(c('green','blue')) (200)
plot(d, col=cl2)

# PER RICHIAMARE I PUNTI RELATIVI AI DATI COVID-19
points(covids)

# FUNZIONE readOGR() PER IMPORTARE SHAPEFILE (IN QUESTO CASO GLI SHAPEFILES RELATIVI ALLE COSTE DEL GLOBO)
# ASSEGNARE LA FUNZIONE readOGR() A "coastlines"
# add=TRUE PER AGGIUNGERE IL NUOVO PLOT ALLO STESSO GRAFICO 
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=TRUE)

# ESERCIZIO: plot della mappa di densità con una nuova colorazione e aggiunta delle coastlines
cl3 <- colorRampPalette(c('green','yellow','red')) (200)
plot(d, col=cl3)
points(covids)
plot(coastlines, add=TRUE)

# ESERCIZIO2: caricare in R il workspace covid.RData (funzione load("...")) e creare una mappa di densità

setwd("C:/Lab")
load("covid.RData")
ls()
library(spatstat)
plot(d, main="densità covid-19")
points(covids)

# INSERIRE GLI SHAPEFILE DELLE COSTE
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=TRUE)

# INTERPOLAZIONE DEI PUNTI COVID-19
# FUNZIONE marks() PER ESTRARRE O MODIFICARE I SIMBOLI ASSEGNATI AD UN DETERMINAT DATA SET DI UN POINT PATTERN
head(covid)
marks(covids) <- covid$cases
s <- Smooth(covids)
plot(s)

# ESERCIZIO: plottare covids con una diversa color palette (punti e shapefile delle coste)
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="stima dei casi covid-19")
points(covids)
plot(coastlines, add=T)
# FUNZIONE text() PER VISUALIZZARE IL VALORE DEI PUNTI SULLA MAPPA
text(covids)

### MAPPA FINALE 
par(mfrow=c(2,1))

# DENSITA'
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="densità covid-19")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# INTERPOLAZIONE NUMERO DI CASI
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="stima dei casi")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

### DATI SAN MARINO (caso di studio)

setwd("C:/Lab")
library(spatstat)
# CARICARE "Tesi.RData"
load("Tesi.RData")
ls()
head(Tesi)
attach(Tesi)

summary(Tesi)
# x varia da 12.42 a 12.46
# y varia da 43.91 a 43.94

# POINT PATTERN : x, y, z
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.90,43.95))

# DENSITA'
dT <- density(Tesippp)
plot(dT)
points(Tesippp)

dev.off()

# 28/04
    
setwd("/Users/enricopriarone/lab")
load("Tesi.RData")
ls()
# OTTENGO I FILE PRESENTI:
# "dT" è density map di Tesippp
# "Tesi" è un dataset
# "Tesippp" è il point pattern del file "Tesi" (da libreria "Spatstat")

# ASSOCIAMO I VALORI CHE VOGLIAMO STIMARE
library(spatstat)
plot(dT)
points(Tesippp, col="green")

# ANDIAMO A STIMARE LA RICCHEZZA SPECIFICA
# CON head() VEDO CHE SI TROVA SOTTO A "Species_richness"
# FUNZIONE marks() PRENDE I VALORI DELLA TABELLA E LI ASSOCIA AI PUNTI DEL PPP
head(Tesi)
marks(Tesippp) <- Tesi$Species_richness

# ASSOCIARE LA FUNZIONE Smooth() A "interpol"
interpol <- Smooth(Tesippp)

# DATI SAN MARINO 
    
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T) 
points(Tesippp,col="green")
# MAPPA VA A SOVRAPPORSI AI CONFINI DI SAN MARINO 
plot(sanmarino, add=T) 

# ESERCIZIO: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

# ESERCIZIO2: inverto la disposizione del grafico
par(mfrow=c(1,2))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

dev.off()

##############################################
##############################################
##############################################

### 5. R CODE TELERIL

# CODICE R PER L'ANALISI DI IMMAGINI SATELLITARI
    
# PACCHETTI UTILIZZATI: "raster", "RStoolbox"

# install.packages("raster")
# install.packages("RStoolbox")
library(raster)
library(RStoolbox)

setwd("C:/Lab")

# FUNZIONE brick() PER IMPORTARE DATI DALL'ESTERNO E ASSOCIARLI AD UNA CERTA IMMAGINE
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# SALVARE L'.RData

# FUNZIONE load("") PER RICARICARE L'.RData
load("teleril.RData")
ls()


plot(p224r63_2011)
# CAMBIARE LA PALETTE DI COLORI IN SCALA DI GRIGI
cl <- colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011, col=cl)
    
# ESERCIZIO: plottare l'immagine con una palette differente
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 

# NOTE: la funzione attach(dataframe) non è utilizzabile con il pacchetto raster
# "$" LEGA LA COLONNA (LA BANDA) AL DATASET (IMMAGINE SATELLITARE) (es. p224r63$B1_sre)
plot(p224r63_2011$B1_sre, col=clb)

#ESERCIZIO: plottare la banda del NIR con colorRampPalette che varia dal rosso all'arancione al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# MULTIFRAME
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

# NATURAL COLOR:
# tre componenti: R G B
# tre bande: R= banda del rosso, G= banda del verde, B= banda del blu
plotRGB(p224r63_2011, r=3, g=2, b=1) # plotRGB: no!
# STRETCH DEI COLORI CON TIPOLOGIA Lineare ("Lin")
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# FALSE COLOR (andiamo ad introdurre la banda nir, non visibile ad occhio nudo)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# FUNZIONE pdf""() PER SALVARE L'IMMAGINE IN PDF
pdf("primo_grafico")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()
    
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# ESERCIZIO: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
# ESERCIZIO2: nir nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")


### GIORNO 2

setwd("C:/lab/") # windows
# setwd("~/lab/") # linux
# setwd("/Users/nome/Desktop/lab") # mac

library(raster)
load("teleril.RData")
ls()

p224r63_1988 <- brick("p224r63_1988_masked.grd")
plot(p224r63_1988)

    
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

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


dev.off()

    
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4

# NATURAL COLOR
# tre componenti: R G B
# tre bande: R= banda del rosso, G= banda del verde, B= banda del blu
plotRGB(p224r63_1988, r=3, g=2, b=1)
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# ESERCIZIO: plottare l'immagine usando il sensore nir al posto della componente r nello spazio RGB
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# PLOTTARE LE IMMAGINI DEL 1988 E DEL 2011 PER ANALIZZARE I CAMBIAMENTI
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# SPECTRAL INDICES
# dvi1988 = nir1988 - red1988
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

plot(dvi1988)
# ESERCIZIO: plottare l'indice dvi per il 2011
# dvi2011 = nir2011 - red2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

plot(dvi2011)

dev.off()

cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 
plot(dvi2011, col=cldvi)

# MULTITEMPORAL ANALYSIS
difdvi <- dvi2011 - dvi1988
plot(difdvi)

cldifdvi <- colorRampPalette(c('red','white','blue'))(100)  
plot(difdvi, col=cldifdvi)

dev.off()

# VISUALIZZARE GLI OUTPUTS
# multiframe 1988rgb, 2011rgb, difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

dev.off()

# RICAMPIONARE L'IMMAGINE DEL 2011
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)


par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
dev.off()

# LOWER RESOLUTION
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
# original 30m <- resampled 1500m 

# PLOT DELLE TRE IMMAGINI CON RISOLUZIONI DIVERSE
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dev.off()
    
# CALCOLARE IL dvi2011lr50
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
plot(dvi2011lr50)

dev.off()

# RICAMPIONARE L'IMMAGINE DEL 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)

# CALCOLARE IL dvi1988lr50
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
plot(dvi1988lr50)

# multitemporal analysis lr50 (low resolution)
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50)

plot(difdvilr50,col=cldifdvi)

dev.off()

# PLOTTARE LA difdvi (ad alta definizione) CON LA difdvilr50 (a bassa risoluzione)
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
    
dev.off()

##############################################
##############################################
##############################################

### 6. R CODE LAND COVER

# CODICE R PER ANALISI DELLA COPERTURA DI SUOLO 

# PACCHETTI UTILIZZATI: "raster", "RStoolbox"

# install.packages("raster")
# install.packages("RStoolbox")    
library(raster)
library(RStoolbox)

    
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()


p224r63_2011 <- brick("p224r63_2011_masked.grd")


# RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# FUNZIONE unsuperClass() ACCORPA I PIXEL IN QUATTRO CLASSI (es. unsuperCLass(immagine, nClasses=4)
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# VISUALIZZARE LE INFORMAZIONI
p224r63_2011c
    
plot(p224r63_2011c$map)
    
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# FUNZIONE unsuperClass() UTILIZZANDO DUE CLASSI ANZICHE' 4
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
clclass2 <- colorRampPalette(c('red', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass2)

dev.off()

# NOTE: In funzione del numero di classi aumenta l'incertezza dell'algoritmo auomatico di classificazione
# NOTE: Con 2 classi l'incertezza è più bassa che con 4

##############################################
##############################################
##############################################

### 7. R CODE MULTITEMP
    
# CODICE R PER ANALISI MULTITEMPORALE DELLA VARIAZIONE DEL LAND COVER ATTRAVERSO IMMAGINI TELERILEVATE 

# PACCHETTI UTILIZZATI: "raster", "RStoolbox", "ggplot2", "Rcmdr", "gridExtra" 

# install.packages("raster")
# install.packages("RStoolbox")
# install.packages("ggplot2")
# install.packages("Rcmdr")
# install.packages("gridExtra")

library(raster)
library(RStoolbox)
library(ggplot2)
library(Rcmdr)
library(gridExtra)

setwd("/Users/enricopriarone/lab") # windows

# IMPORTARE LE IMMAGINI DALLA CARTELLA DI LAVORO (Lab)
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
# ASSOCIARE LA BANDA DEL "rosso" AL "nir", DEL "verde" AL "rosso" E DEL "blu" AL "verde"
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# PER USARE DUE CLASSI NON SUPERVISIONATE (ossia non spieghiamo al computer la divisione)
# NOTE: Computer raggruppa pixel che sembrano simili tra loro
d1c <- unsuperClass(defor1, nClasses=2)
d1c # visualizzo dettagli

plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

# NOTE: esempio sul significato dollaro:
# mappageologica <- geomap(im_sat, nClasees=...)
# plot (mappageologica$lito)
# plot(mappageologica$lineaments)

# ESERCIZIO: classificare con due classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
d2c
plot(d2c$map)
plot(d2c$map, col=cl)

dev.off()

par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# FUNZIONE freq() PER VISUALIZZARE LA SUDDIVISIONE DEI PIXEL NELLE CLASSI
freq(d1c$map)
# aree altre: 35516
# foresta: 305776

totd1 <- 305776 + 35516 # numero pixel carta
totd1
# 341292

# PER CALCOLARE LE PROPORZIONI, LA PERCENTUALE DELLE FREQUENZE
percent1 <- freq(d1c$map)*100/totd1
percent1

# PERCENTUALI:
# aree altre: 10.4
# foreste: 89.6

# STESSO PROCEDIMENTO PER LA CARTA 2
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

# CREARE UN DATAFRAME CON QUESTI DATI
cover <- c("Agriculture","Forest")
before <- c(10.4,89.6)
after <- c(48,52)
output <- data.frame(cover,before,after)
View(output)

dev.off()

### GIORNO 2
    
library(gridExtra)

setwd("C:/lab/") # windows
    
load("defor.RData")
ls()
library(raster)
  
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# COPERTURA DI SUOLO
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

output <- data.frame(cover,before,after)
output

library(ggplot2)

# ISTOGRAMMA DELLA % DI COPERTURA PRIMA DELLA DEFORESTAZIONE
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

dev.off()

# ESERCIZIO: fare lo stesso procedimento per il dopo deforestazione ("after") 
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# ASSEGNARE UN NOME AGLI ISTOGRAMMI
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# ESERCIZIO: utilizzare la funzione grid.arrange(plot1, plot2, nrow=n) per unire i due grafici
grid.arrange(grafico1, grafico2, nrow=1)

##############################################
##############################################
##############################################
    
### 8. R CODE MULTITEMP NO2

# R CODE PER ANALISI MULTITEMPORALE DEI DATI DI NO2 OTTENUTI DA IMMAGINI ELABORATE DAL PROGETTO COPERNICUS 

# PACCHETTI UTILIZZATI: "raster"   

#install.packages("raster")
library(raster)
    
setwd("C:/Lab") # windows

# IMPORTARE E VISUALIZZARE L'IMMAGINE EN_0001.png
EN01 <- raster("EN_0001.png")
plot(EN01)

# ESERCIZIO: importare tutte le altre immagini EN 
# NOTE: è possibile utilizzare anche un "ciclo for" che importa automaticamente le immagini richieste
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

cl <- colorRampPalette(c('red','orange','yellow'))(100) 

# PLOTTARE L'IMMAGINE INIZIALE E L'IMMAGINE FINALE 
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

dev.off()

# DIFFERENZA FRA EN13 ED EN01
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)

# PLOTTATR TUTTE LE IMMAGINI
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

### GIORNO 2

library(raster)

# ENTRARE NELLA CARTELLA esa_no2 ALL'INTERNO DELLA CARTELLA DI LAVORO (Lab)
setwd("C:/lab/esa_no2")

# FUNZIONE list.files() PER ASSEGNARE A rlist LA LISTA DI TUTTI I FILE ".png" ALL'INTERNO DELLA CARTELLA esa_no2
rlist <- list.files(pattern=".png")
rlist

listafinale <- lapply(rlist, raster)
EN <- stack(listafinale)

# SCORPORARE LO stack PER ANDARE AD ANALIZZARE LA DIFFERENZA FRA DUE IMMAGINI
difEN <- EN$EN_0013 - EN$EN_0001

cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

# FUNZIONE boxplot() PER COSTRUIRE UN DIAGRAMMA DEL TIPO "boxplot"
# horizontal= T (TRUE) PER COSTRUIRE IL GRAFICO ORIZZONTALMENTE 
boxplot(EN, horizontal=T)

# outline=F (FALSE) PER ELIMINARE GLI OUTLAYERS 
boxplot(EN, horizontal=T,outline=F,axes=T) # axes=T argomento di defoult riguardante gli assi del grafico
    
##############################################
##############################################
##############################################
 
# 9. R CODE SNOW

# CODICE R PER ANALISI MULTITEMPORALE DELLA COPERTURA NEVOSA ATTRAVERSO IMMAGINI TELERILEVATE

# PACCHETTI UTILIZZATI: "raster", "ncdf4"

# install.packages("ncdf4")
# install.packages("raster")
library(ncdf4)
library(raster)

setwd("C:/Lab/") # windows

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
    
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

plot(snowmay, col=cl)

### IMPORTARE I DATI RELATIVI ALLA COPERTURA NEVOSA

setwd("C:/Lab/snow/") # windows

rlist=list.files(pattern=".tif", full.names=T)

# FUNZIONE lappy() PER INSERIRE TUTTI I FILES IN UNA LISTA 
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

# PLOTTARE LA DIFFERENZA FRA snow2020 E snow2000
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

# FUNZIONE source() PER IMPORTARE UN INTERO SCRIPT DALL'ESTERNO
# SCARICARE IL FILE prediction.r DA IOL (Corso Ecologia del Paesaggio)E PORTARLO NELLA CARTELLA SNOW
source("prediction.r")

# SCARICARE IL FILE predicted.snow.2025.norm DA IOL
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
    
##############################################
##############################################
##############################################

### 10. R CODE PATCHES 
    
# CODICE R PER ANALISI DELLA FRAMMENTAZIONE DEGLI ECOSISTEMI

# PACCHETTI UTILIZZATI: "raster", "igraph", "ggplot2"

# install.packages("raster") 
# install.packages("igraph") 
# install.packages("ggplot2")
library(raster)
library(igraph)
library(ggplot2)

setwd("C:/Lab/") # windows

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

# FUNZIONE reclassify() PER RICLASSIFICARE LA PRIMA IMMAGINE
# land cover 1= agriculture; land cover 2=forest
d1c.for <- reclassify(d1c, cbind(1, NA))

# PLOTTARE L'IMMAGINE RICLASSIFICATA CON L'IMMAGINE NORMALE
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for,col=cl)

dev.off()

# RICLASSIFICAZIONE DELLA SECONDA IMMAGINE 
d2c.for <- reclassify(d2c, cbind(1, NA))

# PLOTTARE LE DUE IMMAGINI RICLASSIFICATE 
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c.for,col=cl)
plot(d2c.for,col=cl)

dev.off()

# FUNZIONE clump() PER RAGGRUPPARE I PIXELS VICINI PER FORMARE UN'UNICA PATCH
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c.for.patches,col=cl)
plot(d2c.for.patches,col=cl)

# FUNZIONE writeRaster PER SALVARE IL RASTER NELLA CARTELLA DI LAVORO
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

# ESERCIZIO: plottare con un'altra color palette
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# max patches d1 = 301
# max patches d2 = 1212

# ANALISI MULTITEMPORALE DEL NUMERO DI PATCHES
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

    
##############################################
##############################################
##############################################

### 11. R CODE CROP

# CODICE R PER IL RITAGLIO DI UN RASTER 

# PACCHETTI UTILIZZATI: "raster", "RStoolbox"

# install.packages("raster") 
# install.packages("RStoolBox")
library(raster)
library(RStoolbox)

setwd("C:/Lab/") # per windows

rlist <- list.files(pattern="snow")
rlist 


# SALVARE IL RASTER NELLA LISTA CON LA FUNZIONE lappy()
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 

# PLOTTARE LA MAPPA PER VEDERE LE COORDINATE DELL'ITALIA

snow.multitemp

plot(snow.multitemp$snow2010r, col=clb)

# FUNZIONE extent PER FARE UNO ZOOM 
extension <- c(6, 18, 40, 50)

# ZOOM
zoom(snow.multitemp$snow2010r, ext=extention)
    
##############################################
##############################################
##############################################
    
### 12. R CODE SDM 

# CODICE R PER LO SPECIES DISTRIBUTION MODELLING
   
# PACCHETTI UTILIZZATI: "raster", "sdm", "rgdal"

# install.packages("raster")
# install.packages("sdm")
# install.packages("rgdal")
library(raster)
library(sdm)
library(rgdal)

# SPECIES
file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)

species
species$Occurrence
plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)

points(species[species$Occurrence == 0,],col='red',pch=16)

# VARIABILI AMBIENTALI

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

# MODELLO

d <- sdmData(train=species, predictors=preds)
d

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') 
p1 <- predict(m1, newdata=preds)

plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)
    
##############################################
##############################################
##############################################
    
### 13. CODICE R PROGETTO ESAME ECOLOGIA DEL PAESAGGIO 

# Analisi del processo di deforestazione e frammentazione della Foresta Amazzonica Ecuadoriana dal 2005 al 2020
# a causa del progressivo utilizzo del suolo per l'estrazione petrolifera nei territori indigeni 

# dati da:
# NDVI: https://land.copernicus.eu/global/themes/vegetation
# SHAPEFILE: https://www.amazoniasocioambiental.org/en/maps/

# PACCHETTI UTILIZZATI: "raster", "ncdf4", "ggplot2", "RStoolbox", "rgdal", "igraph")

# install.packages("raster")
# install.packages("ncdf4")
# install.packages("ggplot2")
# install.packages("RStoolbox")
# install.packages("rgdal")
# install.packages("igraph")
library(raster)
library(ncdf4)
library(ggplot2)
library(RStoolbox)
library(rgdal)
library(igraph)

setwd("E:/ecuador")

# IMPORTO SU R LE IMMAGINI DA Copernicus DELL'INDICE NDVI GLOBALE risoluzione 1km (2005, 2010, 2015, 2020)

ndvi2005 <- raster("c_gls_NDVI_200501110000_GLOBE_VGT_V2.2.1.nc")
ndvi2010 <- raster("c_gls_NDVI_201001110000_GLOBE_VGT_V2.2.1.nc")
ndvi2015 <- raster("c_gls_NDVI_201501110000_GLOBE_PROBAV_V2.2.1.nc")
ndvi2020 <- raster("c_gls_NDVI_202001110000_GLOBE_PROBAV_V2.2.1(1).nc")

# RITAGLIO LA PORZIONE RELATIVA ALL'ECUADOR

ext <- c(-81,-75,-5,2)
ecuador2005 <- crop(ndvi2005, ext)
ecuador2010 <- crop(ndvi2010, ext)
ecuador2015 <- crop(ndvi2015, ext)
ecuador2020 <- crop(ndvi2020, ext)

# COLOR PALETTE PER NDVI
cl <- colorRampPalette(c('light green', 'green', 'dark green', 'red', 'dark red')) (400)

# CONFRONTO LE VARIAZIONI DELL'INDICE NDVI DI 5 ANNI IN 5 ANNI

par(mfrow=c(1,2))
plot(ecuador2005, col=cl)
plot(ecuador2010, col=cl)

dev.off()

par(mfrow=c(1,2,))
plot(ecuador2010, col=cl)
plot(ecuador2015, col=cl)

dev.off()

par(mfrow=c(1,2))
plot(ecuador2015, col=cl)
plot(ecuador2020, col=cl)

# PLOTTO LE MAPPE RITAGLIATE DELL'ECUADOR DEI DIVERSI ANNI 
par(mfrow=c(2,4))
plot(ecuador2005, col=cl)
plot(ecuador2010, col=cl)
plot(ecuador2015, col=cl)
plot(ecuador2020, col=cl)

dev.off()

# CARICO LO shapefile DELLE ZONE RELATIVE AI PROCESSI DI ESTRAZIONE PETROLIFERA (dati da : Amazonia Socioambiental)
petrolio <- shapefile("petroleo.shp")

# PLOTTO L'INDIVE NDVI DEL 2020 CON LO shapefile DELLE ZONE DI ESTRAZIONE
plot(ecuador2020, col=cl)
plot(petrolio, add=T)

dev.off()

# IMPORTO SU R LO shapefile DEI TERRITORI INDIGENI
terr_indigeni <- shapefile("Tis_TerritoriosIndigenas.shp")

# PLOTTO L'INDICE NDVI DEL 2020 CON LO shapefile DEI TERRITORI INDIGENI (dati da: Amazonia Socioambiental)
plot(ecuador2020, col=cl)
plot(terr_indigeni, add=T)

# CARICO LE IMMAGINI DI COPERNICUS INSIEME 
# CREO UNA LISTA CHE COMPRENDA TUTTI I FILE DI ESTENZIONE ".nc" CONTENUTI ALL'INTERNO DELLA CARTELLA DI LAVORO (ecuador)
ecuador_list <- list.files(pattern=".nc")
final_list <- lapply(ecuador_list, raster)
globo_NDVI <- stack(final_list)
# plot(globo_NDVI)

dev.off()

# PLOTTO IN RGB LE IMMAGINI DI COPERNICUS

# Bande 
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# SOSTITUISCO LA BANDA DEL ROSSO (r=3) CON QUELLA DELL'INFRAROSSO (r=4)
ecuador_NDVI <- crop(globo_NDVI, ext)
plotRGB(ecuador_NDVI, r=4, g=3, b=2, stretch="Lin")

dev.off()

### ANALISI DELLE PATCHES

# FUNZIONE unsuperClass() PER RICLASSIFICARE L'IMMAGINE UTILIZZANDO 2 CLASSI DI PIXELS

clclass <- colorRampPalette(c('dark green', 'yellow'))(100) 

ecuador2005_rec <- unsuperClass(ecuador2005, nClasses=2)
ecuador2020_rec <- unsuperClass(ecuador2020, nClasses=2)

ecuador2005_rec
ecuador2020_rec 

plot(ecuador2005_rec$map, col=clclass)

dev.off()

plot(ecuador2020_rec$map, col=clclass)

dev.off()

par(mfrow=c(1,2))
plot(ecuador2005_rec$map, col=clclass, las=1, main="Before Deforestation")
plot(ecuador2020_rec$map, col=clclass, las=1, main="After Deforestation)

dev.off()

clclass2 <- colorRampPalette(c('dark green', 'white')) (100)
ecuador2005_for <- reclassify(ecuador2005_rec$map, cbind(2, NA))
ecuador2020_for <- reclassify(ecuador2020_rec$map, cbind(2, NA))

ecuador2005_for
ecuador2020_for

ecuador2005_for.patches <- clump(ecuador2005_for)
ecuador2020_for.patches <- clump(ecuador2020_for)

ecuador2005_for.patches
ecuador2020_for.patches

# numero patches:
# 2005: 879
# 2020: 4848

# FUNZIONE writeRaster() PER SALVARE LE MAPPE DELLE PATCH IN FORMATO .tif ALL'INTERNO DELLA CARTELLA DI LAVORO
writeRaster(ecuador2005_for.patches, "ecuador2005_for.patches.tif")
writeRaster(ecuador2020_for.patches, "ecuador2020_for.patches.tif")

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 

par(mfrow=c(1,2))
plot(ecuador2005_for.patches,col=clp, main="Before Deforestation")
plot(ecuador2020_for.patches,col=clp, main="After Deforestation")

# ANALISI MULTITEMPORALE DEL NUMERO DI PATCHES

time <- c("1.Before deforestation","2.After deforestation")
npatches <- c(879,4848)

output <- data.frame(time,npatches)
attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill=" dark blue")


