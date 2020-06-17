
### codice R progetto per esame di ecologia del paesaggio 

# pacchetti utilizzati 
install.packages("ncdf4")
library(raster)
library(ncdf4)

setwd("E:/ecuador")

# carico su R le immagini scaricate da Copernicus dell'indice NDVI mondiale del 2014 e del 2020 (risoluzione 300m)

ndvi2020 <- raster("c_gls_NDVI300_202002010000_GLOBE_PROBAV_V1.0.1.nc")
ndvi2014 <- raster("c_gls_NDVI300_201402010000_GLOBE_PROBAV_V1.0.1.nc")

# ritaglio la porzione dell'immagine relativa al Sud America con la funzione 

ext <- c(-80,0,-50,0)

ecuador2014 <- crop(ndvi2014, ext)
ecuador2020 <- crop(ndvi2020, ext)

par(mfrow=c(1,2))
plot(ecuador2014)
plot(ecuador2020)

