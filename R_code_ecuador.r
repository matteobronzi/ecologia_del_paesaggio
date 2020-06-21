
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

# PLOTTE LE MAPPE RITAGLIATE DELL'ECUADOR DEI DIVERSI ANNI 
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
# CREO UNA LISTE CHE COMPRENDA TUTTI I FILE DI ESTENZIONE ".nc" CONTENUTI ALL'INTERNO DELLA CARTELLA DI LAVORO (ecuador)
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






