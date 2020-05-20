# R Code snow

# pacchetti necessari
install.packages("ncdf4")
library(ncdf4)
install.packages("raster")
library(raster)

setwd("C:/Lab/") # windows

# caricare l'immagine raster 
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# stabilire una color ramp 
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# plottare l'immagine snowmay con la color ramp cl
plot(snowmay, col=cl)

### Import snow data

setwd("C:/Lab/snow/") # windows

rlist=list.files(pattern=".tif", full.names=T)

# inserire tutti i file in una lista con lappy
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

# plottare la differenza fra snow2020 e snow2000
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

### Per caricare su R un intero script dall'esterno
# scaricare il file prediction.r da IOL e portarlo nella cartella snow
source("prediction.r")

# scaricare il file predicted.snow.2025.norm da IOL
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

