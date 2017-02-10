
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte")


dataset<-list.files("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/csv");dataset
dataset<-dataset[-c(4,7:10,12)]

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/arte/csv")
lista<-list()

for( i in dataset){
  lista[[i]]<-read.csv(i, sep=",", header=T)
}
names(lista)<-dataset

i=2
dataset[i]
str(lista[[i]])
names(lista)[i]
nrow(lista[[i]])
names(lista[[i]])
t(lista[[i]][1,])

a<-lista[[i]]
load("gadm")
lombardia <- gadm[10,]
load("province")
#plotto la mappa con i puntini
plot(lombardia)
plot(province[province@data$NAME_1=="Lombardia",],add=TRUE,lty=2)
points(a$lng, a$lat,col=2, pch=1, cex=0.5)
title(main=list(dataset[i], cex=1.5))



