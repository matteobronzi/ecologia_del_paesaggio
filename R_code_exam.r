# R code exam

### 1. R code first

# pacchetti utilizzati 

install.packages("sp")
library(sp) # in alternativa require(sp)

# funzione data per richiamare il dataset meuse
data(meuse)
meuse

# head(meuse) visualizza solo le prime righe
head(meuse)
# names(meuse) visualizza solo i nomi delle variabili
names(meuse)

# summary(meuse) visualizza informazioni ulteriori sul data set
summary(meuse)

# pairs(meuse) mostra il grafico
pairs(meuse)

# permette di visualizzare solo levariabili di nostro interesse
# occorre fare la tilde "~"
# la virgola è il separatore di argomenti 
pairs(~ cadmium + copper + lead , data = meuse)

# in R si possono richiamare funzioni precedenti freccia in alto e la premo quattro volte per richiamare names
# esercizio: aggiungo zinc

# permette di ridurre i passaggi pairs: richiamo names(meuse)
# in questo caso prendo un subset ("[]") partendo da ","colonna 3 a (":") 6 (ci dà lo stesso grafico)

# per cambiare colore funzioni: col="red" o altri colori 
pairs(meuse[,3:6], col="blue")

# per cambiare forma punti cercare su google numeri corrispondenti alle forme dei punti e aggiungere pch=20
pairs(meuse[,3:6], col="blue", pch=20)

# per cambiare la dimensione del punto uso argomento funzione "character exageration" ("cex")
pairs(meuse[,3:6], col="blue", pch=20, cex=3)

# per cambiare nome al grafico main="Primo pairs"
pairs(meuse[,3:6], col="blue", pch=20, cex=3, main="Primo pairs")

# Esercizio: inserire "elevation" (settiman variabile)
pairs(meuse[,3:7], col="blue", pch=20, cex=3, main="Primo pairs")

# funzione "source" per inserire file dall'esterno

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
# la prima funzione "correlation" ci dà le correlazioni fra le variabili
# la funzione "smoothing" ci darà la linea di correlazione 
# la funzione "histogram"fa l'istogramma in mezzo alle variabili
# "lowess" è smoother locale

# "lower.panel" è la parte inferiore del grafico pairs e decido cosa metterci ad esempio le correlazioni
# "upper.panel" è parte superiore e ci metto lo smoothing, ossia il grafico dei punti con le linee di correlazione
# "diag.panel" è diagonale e ci metto gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

plot(meuse$cadmium, meuse$copper)
#fa errore

# per costruire un grafico solo con le variabili "cadmium" e "copper"
attach(meuse)
plot(cadmium, copper)

# per cambiare le etichette xlab="" ylab=""
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame")

# per aumentare i caratteri delle etichetti cex.lab=1.5
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame", cex.lab=1.5, cex=2)


##############################################
##############################################
##############################################


### 2. R code spatial

# codice per funzioni spaziali in Ecologia del Paesaggio

# richiamare il pacchetto "sp" di R attraverso la funzione library
library(sp)

# per richiamare i dati del data set "meuse" utilizzo la funzione data
data(meuse)

# per richiamare solo le prime righe della tabella utilizzo la funzione head
head(meuse)

# plot cadmium e lead

# alleghiamo il dataframe con la funzione attach
attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise: plot di copper e zinco con simbolo triangolo(17) e colore verde
plot(copper,zinc,col="green",pch=17)

# per cambiare le etichette si utilizzano xlab e ylab
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# per creare multiframe o multipanel si utilizza la funzione par(mfrow)
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

# per invertire i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

# multiframe automatico
install.packages("GGally")
library(GGally)

# la funzione ggpairs costruisce una matrice di plots da un dato data set
ggpairs(meuse[,3:6])

# Spatial

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

# per plottare i dati spazialmente utilizzo la funzione spplot
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



### DATI San Marino

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
