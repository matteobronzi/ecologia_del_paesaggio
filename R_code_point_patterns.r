# CODICE per analisi dei point patterns

insall.packages("ggplot2")
library(ggplot2)
install.packages("spatstat")
library(spatstat)

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




