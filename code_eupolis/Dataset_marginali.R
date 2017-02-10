setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/dati originali download")
list.files()

library(maptools)
library(lattice)
library(rgdal)
load("gadm")
lombardia <- gadm[10,]
lombardia <- spTransform(lombardia,CRS("+proj=utm +zone=32 +datum=WGS84"))
load("province")
province <- spTransform(province,CRS("+proj=utm +zone=32 +datum=WGS84"))

nidi <- readShapePoly("Centri_storici_polygon")
nidi.df <- as.data.frame(nidi)
nidi.sp <- SpatialPointsDataFrame(nidi, data=nidi.df)
proj4string(nidi.sp) <- CRS("+proj=utm +zone=32 +datum=WGS84")

str(nidi.df)
levels(nidi.df$DESCRIZION)
levels(nidi.df$NOTE)
#plotto la mappa con i puntini
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(nidi.sp, col=2, pch=1, cex=0.5)
title(main=list(shape[i], cex=1.5))