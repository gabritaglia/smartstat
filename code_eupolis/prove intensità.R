getwd()
setwd("OpenData/arte/csvbelli")
load("ARTE")
ARTE[1:2,]
levels(ARTE$provincia)

#dati mancanti
missing<-function(x){noquote(paste0(round(length(which(is.na(x)==T | x==""))/length(x)*100, digits=0),"%"))}
data.frame(apply(ARTE, 2, missing))
#PER IL 45% DEI DATI MANCA L'INDICAZIONE DELLA PROVINCIA E DEL COMUNE

data.frame(apply(CIBO, 2, missing))

#macrotipo
table(ARTE$macrotipo, useNA="always");pie(table(ARTE$macrotipo))
miss.macrotipo<-ARTE[is.na(ARTE$macrotipo),]

cont.table<-table(ARTE$macrotipo, ARTE$provincia,useNA="always")
chisq.test(cont.table) 


library(rgdal)
spTransform(y,CRS("+proj=utm +zone=32 +datum=WGS84"))
####################################################################
#ora modifico le coordinate adeguandole al sistema UTM32
library(rgdal)
load("gadm")
lombardia <- gadm[10,]
is(lombardia)
coordinates(architetture)<-c(architetture$lng, architetture$lat)
coo <- spTransform(architetture$lat,CRS("+proj=utm +zone=32 +datum=WGS84"))
names(architetture)
states<-architetture[1:30,-c(3:18)]
is.na(states$lat)
is.na(states$lng)
states <- data.frame(state.x77, state.center)
library(maps)
map("world")
map("usa")
map("italy")
points(states$x, states$y)
points(states$lng, states$lat, col=2)
coordinates(states) <- c("x", "y")
coordinates(states) <- c("lng", "lat")
is(states)
require(rgdal)
states <- spTransform(states,CRS("+proj=utm +zone=32 +datum=WGS84"))
a <- spTransform(states$lng,CRS("+proj=utm +zone=32 +datum=WGS84"))
b <- spTransform(states$lat,CRS("+proj=utm +zone=32 +datum=WGS84"))
proj4string=CRS("+proj=longlat")
spTransform(states, CRS("+proj=utm +zone=32"))
proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
proj4string(states) <- CRS("+proj=utm +zone=32 +datum=WGS84")
states
proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
summary(states)
state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
summary(state.ll83)
state.merc <- spTransform(states, CRS=CRS("+proj=merc +ellps=GRS80"))
summary(state.merc)



library(rgdal)
load("gadm")
lombardia <- gadm[10,]
is(lombardia)
lombardia <- spTransform(lombardia,CRS("+proj=utm +zone=32 +datum=WGS84"))
load("province")
province <- spTransform(province,CRS("+proj=utm +zone=32 +datum=WGS84"))
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(states, cex=0.1, col=2)


data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS(paste("+init=epsg:28992",
                                "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))
# see http://trac.osgeo.org/gdal/ticket/1987
summary(meuse)
meuse.utm <- spTransform(meuse, CRS("+proj=utm +zone=32 +datum=WGS84"))
summary(meuse.utm)
cbind(coordinates(meuse), coordinates(meuse.utm))




levels(as.factor(ARTE$origine))
ARTEnoa<-ARTE[which(ARTE$origine!="architetture.georef.csv"),]

###############descrittive

data.frame(table(ARTE$provincia, useNA="always"))
barplot(table(ARTE$provincia),cex.names=0.5)

data.frame(table(ARTE$macrotipo, useNA="always"))
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(table(ARTE$macrotipo),cex.names=1.2,horiz = T)
data.frame(table(ARTE$provincia, useNA="always"))
barplot(table(ARTE$provincia),cex.names=0.5)

table(ARTE$macrotipo,ARTE$provincia, useNA="always" )



#plotto sulla mappa
a<-ARTE
load("gadm")
lombardia <- gadm[10,]
load("province")
#plotto la mappa con i puntini
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(a$lng, a$lat,col=2, pch=1, cex=0.3)
title(main=list("ARTE", cex=2))

par(mfrow=c(1,2))
#plotto la mappa con i puntini
load("gadm")
lombardia <- gadm[10,]
load("province")
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(ARTE$lng, ARTE$lat,col=2, pch=1, cex=0.3)
title(main=list("\n \n tutti i 25.632 siti", cex=1.5))

#plotto la mappa con i puntini ma senza le architetture.csv
ARTEnoa<-ARTE[which(ARTE$origine!="architetture.georef.csv"),]
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(ARTEnoa$lng, ARTEnoa$lat,col=2, pch=1, cex=0.5)
title(main=list("\n \n senza i 13.581 siti di \n Architetture", cex=1.5))






macrotipo

par(mfrow=c(2,3))
for (i in c(1,4:8)){
  plot(lombardia)
  plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
  a<-ARTE[which(ARTE$macrotipo==macrotipo[i]),]
  points(a$lng, a$lat,col=i, pch=1, cex=0.7)
  #legend("bottomleft",macrotipo[i] ,bty="n",  col=i, pch=1, cex=.7)
  title(main=as.character(macrotipo[i]), cex.main=2, 
        sub=paste0("numerosità ",as.character(nrow(a)),"\n \n"), cex.sub=2)
}


for (i in c(1:8)){
  plot(lombardia)
  plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
  a<-ARTE[which(ARTE$macrotipo==macrotipo[i] & ARTE$origine!="architetture.georef.csv" ),]
  points(a$lng, a$lat,col=i, pch=1, cex=0.5)
  #legend("bottomleft",macrotipo[i] ,bty="n",  col=i, pch=1, cex=.7)
  title(main=list(as.character(macrotipo[i])), sub=list(paste0("numerosità ",as.character(nrow(a)),"\n \n")))
}
#prove di densità
par(mfrow=c(1,1))
require(spdep) #--- aggiunge il pacchetto spdep a sessione lavoro
require(spatstat)
require(maptools)
ppp=ARTEnoa[which(!is.na(ARTEnoa$lng)& !is.na(ARTEnoa$lat)),c("lng","lat")]
ppp0=as.ppp(ppp,W=lombardia);ppp0
Z <- density.ppp(ppp0, varcov=diag( c(var(ppp$lng),var(ppp$lat))/1000))
plot(Z,main="mappa dell'intensità");
plot(ppp0,add=T,cex=0.01, pch=1, col=2)

ARTEc<-ARTEnoa[which(!is.na(ARTEnoa$lng) & !is.na(ARTEnoa$lat)),]
par(mfrow=c(3,2))
for (i in c(1,4:8)){
  a<-ARTEc[which(ARTEc$macrotipo==macrotipo[i]),]
  ppp=a[,c("lng","lat")]
  ppp0=as.ppp(ppp,W=lombardia);ppp0
  Z <- density.ppp(ppp0)
  plot(Z,main="mappa dell'intensità kernel");
  title(sub=list(macrotipo[i], cex=1.5))
}
par(mfrow=c(1,1))
for (i in c(1:8)){
  plot(lombardia)
  plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
  a<-architetture[which(architetture$macrotipo==macrotipo[i]),]
  points(a$lng, a$lat,col=i, pch=1, cex=0.5)
  legend("bottomleft",macrotipo[i] ,bty="n",  col=i, pch=1, cex=.7)
  title(main=list(as.character(macrotipo[i])))
}
#prove di densità

require(spdep) #--- aggiunge il pacchetto spdep a sessione lavoro
require(spatstat)
require(maptools)
ppp=ARTE[,c("lng","lat")]
ppp0=as.ppp(ppp,W=lombardia);ppp0
which(is.na(ARTE$lng))
which(is.na(ARTE$lat))
yy<-ARTE[c(14141,14142,14170, 14171),]
plot(ppp0,cex=0.5,main=,"Incendi in Lombardia nel 2003")
Z <- density.ppp(ppp0, varcov=diag( c(var(ppp$lng),var(ppp$lat))/1000))
plot(Z,main="mappa dell'intensità kernel");
plot(ppp0,add=T,cex=0.01, pch=1, col=2)

par(mfrow=c(3,2))
for (i in c(1,4:8)){
  a<-architetture[which(architetture$macrotipo==macrotipo[i]),]
  ppp=a[,c("lng","lat")]
  ppp0=as.ppp(ppp,W=lombardia);ppp0
  Z <- density.ppp(ppp0, varcov=diag( c(var(ppp$lng),var(ppp$lat))/ 16 )  )
  plot(Z,main="mappa dell'intensità kernel");
  title(sub=list(macrotipo[i], cex=1.5))
}



#faccio un controllo
# read in bear data, and turn it into a SpatialPointsDataFrame
bears<-bears.all
coordinates(bears) <- c("lng", "lat")
# read in National Parks polygons
parks <- readShapePoly("Comuni_2013_polygon/Comuni_2013_polygon")

proj4string(bears)<-CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(parks)<-CRS("+proj=utm +zone=32 +datum=WGS84")
parks <- spTransform(parks,CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
plot(parks)
points(bears, cex=0.1, col=bears$provincia)
title("Arte e Cultura \n Siti divisi per provincia")


#studio solo la provincia di milano e al max la MB
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/csvbelli")
ARTE<-read.csv("ARTE2.csv")
#polygons province lombarde
province <- readShapePoly("RLGeoDATA/Province_2014_polygon")
proj4string(province)<-CRS("+proj=utm +zone=32 +datum=WGS84")
province <- spTransform(province,CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
province.df <- as.data.frame(province)
province.sp <- SpatialPolygonsDataFrame(province, data=province.df)
plot(province.sp)

table(ARTE$provincia)
province.df
province@data
str(province@polygons)
province@polygons[[12]]@area

#"densità" di arte
t<-data.frame(table(ARTE$provincia))
names(t)
province@data$SIGLA
id<-c();aree<-c()
for (i in 1:12){
aree[i]<-province@polygons[[i]]@area
id[i]<-province@polygons[[i]]@ID
a<-data.frame(id, aree)}
str(province)
province@data
a
t
c<-cbind(province@data,a)
b<-merge( c,t,  by.x="SIGLA", by.y="Var1")

p<- as.data.frame.matrix(table(ARTE$provincia,ARTE$macrotipo)) 
p<-cbind(PROV=dimnames(p)[[1]], p)
b<-merge( b,p,  by.x="SIGLA", by.y="PROV") 

names(b)
fun<-function(x){x/b[6]}
m<-apply(b[,7:15],1,fun)
m<-data.frame(b)
for(i in 7:15)
{m[,i+1]<-b[i]/b[6]}
m[,c(1, 5:16)]
names(m)

