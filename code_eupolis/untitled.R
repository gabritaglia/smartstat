#pacchetti da importare (nel dubbio tutti!)
require(spdep) #--- aggiunge il pacchetto spdep a sessione
require(spatstat)
require(maptools)
setwd("/media/tagliabue/DATI/Dropbox/MAGISTRALE/Corsi/Statistica Spaziale - Borgoni/Esercitazioni")
incendi <- read.table("Incendi_2003.csv", header=T,sep=";");
str(incendi) 
#--- descrizione sintetica, 380 incendi con epicentri e ettari interessati
#--- lettura shape file Lombardia
lomb.poly<-readShapePoly("lombardia",verbose=TRUE) #--- lettura shape file in R

lomb.poly@data
proj4string(lomb.poly)
class(lomb.poly)
plot(lomb.poly)

#shape file: contiene in forma elementare punti, linee,.. che costituiscono un poligono

##--- incendi_lombardia

ppp=incendi[,c("EstN","Nord")]
ppp0=as.ppp(ppp,W=lomb.poly)

plot(ppp0,cex=0.5,main="Incendi in Lombardia nel 2003", cex.main=0.8)




#SITI PREISTORICI
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData/cultura storia arte/Rilevanze_Siti_preistorici_point")
p <- readShapePoints("Rilevanze_Siti_preistorici_point",verbose=TRUE) #--- lettura shape file in R
proj4string(p)

plot(lomb.poly)
plot(p)
d <- lomb.poly@data

