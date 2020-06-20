
### codice R progetto per esame di ecologia del paesaggio 

# pacchetti utilizzati: "raster", "ncdf4", "ggplot2", "RStoolbox", "rgdal" 

# install.packages("raster")
# install.packages("ncdf4")
# install.packages("ggplot2")
# install.packages("RStoolbox")
# install.packages("rgdal")
library(raster)
library(ncdf4)
library(ggplot2)
library(RStoolbox)
library(rgdal)

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

# plotto le mappe ritagliate dell'ecuador dei diversi anni
par(mfrow=c(4,2))
plot(ecuador2005)
plot(ecuador2010)
plot(ecuador2015)
plot(ecuador2020)

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
plot(ecuador2020)
plot(terr_indigeni, add=T)

