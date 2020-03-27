# R SPATIAL

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
