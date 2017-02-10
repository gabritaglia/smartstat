#install.packages("rgdal")

library(ggmap)
#library(rgdal)
library(MASS)
library(spatstat)
setwd("~/EUPOLIS/SpatialObject")

tema.format.ggplot <- function(){
  tema <- theme(
    plot.background=element_rect(fill="beige"),
    plot.title=element_text(lineheight=.8,face="bold"),
    panel.background = element_rect(fill="lightsteelblue1"),
    axis.text.x=element_text(angle=0,size=0,face="bold"),
    axis.text.y=element_text(angle=0,size=0,face="bold"),
    axis.title.x=element_text(angle=0,size=15,face="italic"),
    axis.title.y=element_text(angle=90,size=15,face="italic"),
    strip.background=element_rect(colour="blue",fill=c("yellow")),
    strip.text=element_text(angle=0,size=17,face="italic"),
    legend.title=element_blank(),
    legend.background=element_rect(fill="beige"),
    legend.position = "none"
  )
  return(tema)
}


#------MAPPE
#get_map
bboxMI<-c(bbox(comuni.mi.wgs))
milano.roadmap <- get_map(location=c(lon= 9.159498,lat=45.461265),zoom=12,color="bw",maptype="roadmap") #centro del bbox milano

ggmap(milano.roadmap,extent="panel",maprange=FALSE)
#preparazione dell'area di riferimento

#città
load("comuni.mi.wgs")
area <- comuni.mi.wgs
str(area)
area <- area[1]@polygons[[1]]@Polygons[[1]]@coords
area <- data.frame(area)
names(area) <- c("x","y")

load("pol2.mi.wgs")
#dati
polyDF <- pol2.mi.wgs
names(polyDF@data)[1]<-"cella"

names(polyDF@data)
scelta<-c("cul", "sto","sto-rur", "sto-rel", "art-rel-rur","art-sto","art-sto-rur","art-sto-rel-rur","tra-rur-spo","tra-rel-rur","tra-cul","tra-sto-rur","tra-sto-rel-rur")
sceltaLab<-c("Cultura",
             "Storia",
             "Storia-Ruralita",
             "Storia-Religiosita",
             "Arte-Religiosita-Ruralita",
             "Arte-Storia",
             "Arte-Storia-Ruralita",
             "Arte-Storia-Religiosita-Ruralita",
             "Tradizione-Ruralita-Sport",
             "Tradizione-Religiosita-Ruralita",
             "Tradizione-Cultura",
             "Tradizione-Storia-Ruralita",
             "Tradizione-Storia-Religiosita-Ruralita")
AllLab<-c("Sport",
          "Sport in campagna",#"rur-spo",
          "Religiosita",
          "Spiritualità nella campagna lombarda", #"rel-rur",
          "Cultura",
          "Storia",
          "Antica campagna lombarda", #"sto-rur",
          "L'architettura storica lombarda", #"sto-rel",
          "Storia-Religiosita-Ruralita",
          "Cascine di rilevanza artistica", # "art-rur",
          "Chiese d'arte", #"art-rel",
          "Arte-Religiosita-Ruralita",
          "L'arte nella storia", #"art-sto",
          "Arte-Storia-Ruralita",
          "Arte-Storia-Religiosita",
          "Arte-Storia-Religiosita-Ruralita",
          "Sport in campagna", #"tra-Sport",
          "Tradizione-Ruralita-Sport",
          "Lombardia agricola", # "tra-rel",
          "Tradizione-Religiosita-Ruralita",
          "Cultura lombarda", #"tra-cul",
          "Tradizione-Storia",
          "Tradizione-Storia-Ruralita",
          "Tradizione-Storia-Religiosita",
          "Tradizione-Storia-Religiosita-Ruralita",
          "Tradizione-Arte",
          "Tradizione-Arte-Ruralita",
          "Tradizione-Arte-Religiosita",
          "Tradizione-Arte-Religiosita-Ruralita",
          "Tradizione-Arte-Cultura",
          "Tradizione-Arte-Storia",
          "Tradizione-Arte-Storia-Ruralita",
          "Tradizione-Arte-Storia-Religiosita",
          "Tradizione-Arte-Storia-Religiosita-Ruralita")


sceltaCol<-names(polyDF)%in%scelta
sceltaCol<-which(sceltaCol==T)

#se si fanno tutte le mappe
if(TRUE){
  sceltaCol=13:ncol(polyDF)
  sceltaLab<-AllLab
}

png("/home/gabriele/EUPOLIS/mappe/milano/map%03d.png", width=1280, height=1280)
for(j in sceltaCol ){
  #preparazione della covariata
  cultura<- polyDF[which(!is.na(polyDF@data[,j])),c(1,j)]
  names(cultura@data)[2]<-"z"
  cultura@data[which(cultura$z==0),"z"] <- 0.1
  
  #require(rgdal)
  #cultura.wgs <- spTransform(cultura,CRS("+proj=longlat +datum=WGS84"))
  #cultura.coords <- data.frame(coordinates(cultura.wgs),cultura$z)
  cultura.coords <- data.frame(coordinates(cultura),cultura$z)
  names(cultura.coords) <- c("x","y","z")
  cultura.coords$z <- cultura.coords$z+abs(min(cultura.coords$z))+1
  n <- cultura.coords$z
  res <- c()
  for(i in 1:length(n)){
    res <- c(res,rep(i,n[i]))
  }
  cultura.coords <- cultura.coords[res,]
  
  titolo<-sceltaLab[which(sceltaCol==j)]
  
  print(
    ggmap(milano.roadmap,extent="panel",maprange=FALSE)+
      stat_density2d(data=cultura.coords,aes(x=x,y=y,fill=..level..,alpha=..level..),h=.015,size =0.003,bins=16,geom='polygon')+
      #geom_path(data=area,aes(x=x,y=y),colour="blue",size=1)+
      #xlab("")+ylab("")+ggtitle(titolo)+
      xlab("")+ylab("")+ggtitle(titolo)+
      theme(plot.title = element_text(size=20))+
      scale_fill_gradient(low="green",high="red")+
      scale_alpha(range=c(0.00,0.9),guide=TRUE)+
      tema.format.ggplot()
  )
}
dev.off()
