
### CODICE R progetto per esame di ecologia del paesaggio 

# dati da:
# NDVI: https://land.copernicus.eu/global/themes/vegetation
# SHAPEFILE: https://www.amazoniasocioambiental.org/en/maps/

# pacchetti utilizzati: "raster", "ncdf4", "ggplot2", "RStoolbox", "rgdal", "igraph")

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

# importo su R le immagini da Copernicus dell'indice NDVI globale risoluzione 1km (2005, 2010, 2015, 2020)

ndvi2005 <- raster("c_gls_NDVI_200501110000_GLOBE_VGT_V2.2.1.nc")
ndvi2010 <- raster("c_gls_NDVI_201001110000_GLOBE_VGT_V2.2.1.nc")
ndvi2015 <- raster("c_gls_NDVI_201501110000_GLOBE_PROBAV_V2.2.1.nc")
ndvi2020 <- raster("c_gls_NDVI_202001110000_GLOBE_PROBAV_V2.2.1(1).nc")

# ritaglio la porzione dell'immagine relativa all'Ecuador

ext <- c(-81,-75,-5,2)
ecuador2005 <- crop(ndvi2005, ext)
ecuador2010 <- crop(ndvi2010, ext)
ecuador2015 <- crop(ndvi2015, ext)
ecuador2020 <- crop(ndvi2020, ext)

# color palette per ndvi
cl <- colorRampPalette(c('light green', 'green', 'dark green', 'red', 'dark red')) (400)

# plotto le mappe ritagliate dell'ecuador dei diversi anni
par(mfrow=c(2,4))
plot(ecuador2005, col=cl)
plot(ecuador2010, col=cl)
plot(ecuador2015, col=cl)
plot(ecuador2020, col=cl)

dev.off()

# carico shapefile relativo alle zone di estrazione petrolifera
petrolio <- shapefile("petroleo.shp")

# plotto l'indice NDVI del 2020 con lo shapefile delle zone di estrazione
plot(ecuador2020)
plot(petrolio, add=T)

dev.off()

# carico shapefile relativo ad i territori indigeni 
terr_indigeni <- shapefile("Tis_TerritoriosIndigenas.shp")

# plotto l'indice NDVI del 2020 con lo shapefile dei territori indigeni
plot(ecuador2020, col=cl)
plot(terr_indigeni, add=T)

# carico le immagini di copernicus insieme 
# creo una lista che comprenda tutti i file di estensione ".nc" contenuti all'interno della cartella di lavoro (ecuador)
ecuador_list <- list.files(pattern=".nc")
final_list <- lapply(ecuador_list, raster)
globo_NDVI <- stack(final_list)
plot(globo_NDVI)

dev.off()

# plotto in RGB le immagini di copernicus

# Bande 
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# sostituisco la banda del rosso (r=3) con quella dell'infrarosso (r=4)
ecuador_NDVI <- crop(globo_NDVI, ext)
plotRGB(ecuador_NDVI, r=4, g=3, b=2, stretch="Lin")

dev.off()

### Analisi delle patches

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
plot(ecuador2005_rec$map, col=clclass, las=1)
plot(ecuador2020_rec$map, col=clclass, las=1)

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
plot(ecuador2005_for.patches,col=clp)
plot(ecuador2020_for.patches,col=clp)

# ANALISI MULTITEMPORALE DEL NUMERO DI PATCHES

time <- c("1.Before deforestation","2.After deforestation")
npatches <- c(879,4848)

output <- data.frame(time,npatches)
attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")






