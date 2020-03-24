# R Spatial: codice per funzioni spaziali in Ecologia del Paesaggio

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
