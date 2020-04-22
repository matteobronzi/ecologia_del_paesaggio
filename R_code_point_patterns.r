# CODICE per analisi dei point patterns

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
