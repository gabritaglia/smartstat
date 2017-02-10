# Mappe di Kohonen

setwd("/home/gabriele/skyFTP")

#popolazione: prendo il file cookies2 che contiene 687.960 di cookies
library(dplyr)

load("sample2.1/cookies")


var_navi<-c("n_visite", "n_days_visite", "n_pagine_viste","n_entry_pages_distinte","n_entry_pages_totali")
var_day<-names(cookies)[c(171:177)] 
var_we<-names(cookies)[179] #no wd
var_fascia<-names(cookies)[c(180:186)] 
var_fasciawe<-names(cookies)[c(187:200)]  
var_fasciasabdom<-names(cookies)[201:221] 
var_piattaforma<-names(cookies)[c(228:231,233)] 
var_mobile<-names(cookies)[232]
var_pagesgiorno<-names(cookies)[c(235,238,240)]
var_pagesgiorno2<-names(cookies)[c(236,237,239)]
var_pagesArea<-names(cookies)[c(242:244,246:258)]
var_video<-names(cookies)[c(259,308)]
var_commerciale<-names(cookies)[c(245,260:262,264:267,294,309:311,313:316)] #noskytv
var_pagesSem<-names(cookies)[268:289] 
var_entryArea<-names(cookies)[c(291:293,295:307)]
var_entrySem<-names(cookies)[317:337] #notuttoilresto
var_insPack<-names(cookies)[354:365]
var_log<-names(cookies)[401]

subset<-cookies[,c(var_pagesArea, var_commerciale[1])]

rownames(subset)<-cookies$visitorID


library(kohonen)

str(subset)


soggetti<-c("soggetto","soggetto_calcio", "soggetto_sport", "soggetto_cinema", "soggetto_famiglia", "soggetto_tecnologiasky")





#Per le areee devo fare delle aggregazioni intelligenti
subset$Calcio<-subset$pages_AreaCalcio
subset$Cinema<-subset$pages_AreaCinema
subset$Programmi_e_serie_TV<-rowSums(subset[,c("pages_AreaProgrammitvAltro",
                                               "pages_AreaProgrammitvMusica",
                                               "pages_AreaProgrammitvGastronomia",
                                               "pages_AreaSerietv",
                                               "pages_AreaIntrattenimento_altro",
                                               "pages_AreaOroscopo")])
subset$Sport_e_Motori<-rowSums(subset[,c("pages_AreaSport_altro",
                                         "pages_AreaMotori")])
subset$News<-rowSums(subset[,c("pages_AreaNews",
                               "pages_AreaMeteo")])
subset$Assistenza_e_Commerciale<-rowSums(subset[,c("pages_AreaAssistenza",
                                                   "pages_AreaCont_clienti","pages_AreaCommerciale",
                                                   "pages_AreaForum","pages_AreaResiduale")])

subset<-subset[,c("Calcio","Cinema","Programmi_e_serie_TV","Sport_e_Motori","News","Assistenza_e_Commerciale")]
#subset<-subset[,c("Calcio","Cinema","Intrattenimento","Sport","News")] 
str(subset)


data<-subset

d<-data[rowSums(data)!=0,]
for(j in 1:ncol(d)) d[,j]<-as.numeric(d[,j])
d<-as.matrix(d)
d<-d*1
d<-d/rowSums(d)
d[is.na(d)]<-0

d1<-d[rowSums(d)!=0,]
set.seed(123)
d1<-d1[sample(1:nrow(d1),300000),]

#   d<-scale(d)  
str(d1)
set.seed(856)
library(kohonen)
mappa <- som(data=d1, grid = somgrid(30, 30, "hexagonal"))
plot(mappa)
save(mappa, file="kohonen/mappa_Aree_sky")


#---------MAPPE PIU' BELLE --------------
#######--- KOHONEN IGPDecaux Barberis ------##########
library(sp)
kohonen_plot_info <- function(mappa){
  centroidi <- SpatialPoints(mappa$grid$pts)
  grid_data <- data.frame(mappa$grid$pts,mappa$codes)
  griglia <- grid_data
  gridded(griglia) <- ~x + y
  griglia <- HexPoints2SpatialPolygons(griglia)
  output <- as.data.frame(grid_data)[over(SpatialPoints(mappa$grid$pts),griglia),]
  output <- SpatialPolygonsDataFrame(griglia,output,match.ID=FALSE)
  pixel <- SpatialPoints(mappa$grid$pts)
  pixel <- SpatialPixels(pixel)
  #-- scala di colori   
  colore.rgb <- col2rgb(rainbow(dim(mappa$codes)[2]))%*%t(mappa$codes)
  #-- conversione in rgb
  #colore.hex <- apply(colore.rgb,2,rgb_converter_1,max=max(colore.rgb))
  colore.hex <- apply(colore.rgb,2,rgb_converter)
  return(list(poligoni=output,pixel=pixel,colore=colore.hex))
}
rgb_converter <- function(x){
  rgb(x[1],x[2],x[3],maxColorValue=255)
}

rgb_converter_1 <- function(x,max){
  rgb(x[1],x[2],x[3],maxColorValue=max)
}




sel <- d1[,]
library(kohonen)

mappa
mappa$codes

plot_legend <- function(mappa){
  colori <- col2rgb(rainbow(dim(mappa$codes)[2]))
  colore.hex <- apply(colori,2,rgb_converter)
  #par(mar=c(0,0,0,0),bg="beige")
  plot(1,type="n",axes=FALSE,xlab="",ylab="")
  legend("right",legend=colnames(mappa$data),col=colore.hex,pch=19,pt.cex=2,inset=.05, ncol=2, bty="n")
}

kohonen_map_proj <- function(mappa,dati){
  cluster <- data.frame(id=rownames(dati),cluster=as.numeric(map(mappa,dati)$unit.classif))
  #cluster <- data.frame(id=as.numeric(dati$id),cluster=as.numeric(map(mappa,dati)$unit.classif))
  pts <- data.frame(mappa$grid$pts)
  pts$cluster <- 1:dim(pts)[1]
  unione <- merge(cluster,pts,by=c("cluster"))
  unione$x <- jitter(unione$x,1.5)
  unione$y <- jitter(unione$y,1.5)
  return(list(coordinate=unione[,3:4],info=unione[,1:2]))
}



d_carrello<-which(row.names(d1)%in%cookies[cookies$soggetto=="carrello","visitorID"])
d_compra<-which(row.names(d1)%in%cookies[cookies$soggetto=="compra","visitorID"])

d_compra_sport<-which(row.names(d1)%in%cookies[cookies$soggetto_sport=="compra","visitorID"])             
d_compra_calcio<-which(row.names(d1)%in%cookies[cookies$soggetto_calcio=="compra","visitorID"])
d_compra_cinema<-which(row.names(d1)%in%cookies[cookies$soggetto_cinema=="compra","visitorID"])
d_compra_famiglia<-which(row.names(d1)%in%cookies[cookies$soggetto_famiglia=="compra","visitorID"])
d_compra_HD<-which(row.names(d1)%in%cookies[cookies$soggetto_HD=="compra","visitorID"])

d_carrello_sport<-which(row.names(d1)%in%cookies[cookies$soggetto_sport=="carrello","visitorID"])             
d_carrello_calcio<-which(row.names(d1)%in%cookies[cookies$soggetto_calcio=="carrello","visitorID"])
d_carrello_cinema<-which(row.names(d1)%in%cookies[cookies$soggetto_cinema=="carrello","visitorID"])
d_carrello_famiglia<-which(row.names(d1)%in%cookies[cookies$soggetto_famiglia=="carrello","visitorID"])
d_carrello_HD<-which(row.names(d1)%in%cookies[cookies$soggetto_HD=="carrello","visitorID"])


save(mappa, file="kohonen/mappa_Aree_sky_def")
save(d1, file="kohonen/d1_Aree_def")

prof <- kohonen_map_proj(mappa,d1)$coordinate
pippo<-kohonen_plot_info(mappa)

par(mar=c(0,0,0,0), mfrow=c(1,2))
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),
       widths=c(3,1), heights=c(3,1))
print(plot(pippo$poligoni,col=pippo$colore))

points(prof,col="black",pch=20,cex=.7)

points(prof[d_carrello,],col="black",pch=20,cex=.7)
points(prof[d_compra,],col="black",pch=20,cex=.7)

points(prof[d_compra_sport,],col="black",pch=20,cex=.7)
points(prof[d_compra_calcio,],col="black",pch=20,cex=.7)
points(prof[d_compra_cinema,],col="black",pch=20,cex=.7)
points(prof[d_compra_famiglia,],col="black",pch=20,cex=.7)
points(prof[d_compra_HD,],col="black",pch=20,cex=.7)

points(prof[d_carrello_sport,],col="black",pch=20,cex=.7)
points(prof[d_carrello_calcio,],col="black",pch=20,cex=.7)
points(prof[d_carrello_cinema,],col="black",pch=20,cex=.7)
points(prof[d_carrello_famiglia,],col="black",pch=20,cex=.7)
points(prof[d_carrello_HD,],col="black",pch=20,cex=.7)


print(plot_legend(mappa))









hc<-cutree(hclust(dist(mappa$codes)),6)
add.cluster.boundaries(mappa,hc)