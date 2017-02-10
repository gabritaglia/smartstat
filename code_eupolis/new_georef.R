dataset<-list.files("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/csv");

listanew<-list()
for( i in dataset){
  listanew[[i]]<-read.csv(i, sep=",", header=T)
}
names(listanew)
a<-(listanew[[2]])

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/shape")

library(maptools)
library(lattice)

oldwd<-getwd()
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/csv")


library(rgdal)
load("gadm")
lombardia <- gadm[10,]
proj4string(lombardia)
lombardia <- spTransform(lombardia,CRS("+proj=utm +zone=32 +datum=WGS84"))
load("province")
province <- spTransform(province,CRS("+proj=utm +zone=32 +datum=WGS84"))

require(rgdal)
shape<-"Rilevanze_Architettura_fortificata_point"
nidi <- readShapePoints(paste0(shape,"/",shape))
nidi.df <- as.data.frame(nidi)
nidi.sp <- SpatialPointsDataFrame(nidi, data=nidi.df)
proj4string(nidi.sp) <- CRS("+proj=utm +zone=32 +datum=WGS84")
nidi.sp <- spTransform(nidi.sp,CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
plot(lombardia)
points(nidi.sp)
shape<-list.files("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/shape")
folder<-paste0(shape, "/", shape)

i=24
shape[i]
nidi <- readShapePoints(folder[i])
nidi.df <- as.data.frame(nidi)
nidi.sp <- SpatialPointsDataFrame(nidi, data=nidi.df)
proj4string(nidi.sp) <- CRS("+proj=utm +zone=32 +datum=WGS84")

str(nidi.df)

#plotto la mappa con i puntini
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
lines(nidi.sp,col=2,pty=1, cex=0.3)
points(a$lng.y, a$lat.y)
title(main=list(dataset[2], cex=0.8))



