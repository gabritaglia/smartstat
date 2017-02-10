# Correspondence Analysis
load("sample/cookies")

table(cookies$soggetto)
#ribilancio il campione:
n_compra<-sum(cookies$soggetto=="compra")
n_carrello<-sum(cookies$soggetto=="carrello")
sample_naviga<-sample(which(cookies$soggetto=="naviga"),(n_compra+n_carrello))

cookies2<-rbind(cookies[cookies$soggetto%in%c("compra", "carrello"),],
                cookies[sample_naviga,])


summary(cookies2$n_days_visite)
summary(cookies$p_days_visiteSUperiodo)
summary(as.numeric(cookies2$n_pagine_viste))
#creo qualche variabile categoriale
processo<-function(var1, var2,data=cookies2, type="visite"){
if(type=="visite"){
classe<-rep("Mai", nrow(data))
classe[data[,var2]==1]<-"Una mese"
classe[data[,var2]>1 & cookies2[,var2]<=34]<-"Una giorno"
classe[data[,var2]>34]<-"Più di una giorno"
}
if(type=="pagineviste"){
  classe<-rep("Mai", nrow(data))
  classe[data[,var2]==1]<-"Una mese"
  classe[data[,var2]>1 & cookies2[,var2]<=34]<-"Una giorno"
  classe[data[,var2]>34]<-"Più di una giorno"
}
if(type=="factor"){
classe<-factor(data[,var2])
}
if(type=="giorni"){
  classe<-rep("Zero", nrow(data))
  classe[data[,var2]==1]<-"Uno Mese"
  classe[data[,var2]>1 & cookies2[,var2]<=4]<-"Uno Sett"
  classe[data[,var2]>4]<-"Più di uno Sett"
}
#ca sui cookies2
PLOTCA<-function(var1,var2, data=cookies2)
library(ca)
mytable <- table(data[,var1],classe)
print(mytable)
# prop.table(mytable, 1) # row percentages
# prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
# print(fit)
# summary(fit) # extended results
titolo<-paste("CA",var1, "contro" ,var2)
png(file=paste0("images/",titolo,".png"),1024,1024)
par(cex.lab=1, cex=3)
print(plot(fit))# symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map 

title(titolo)
dev.off()
}


processo("soggetto", "n_visite")
processo("soggetto", "pages_commerciale")
processo("soggetto", "pages_intrattenimento")
processo("soggetto", "pages_meteo")
processo("soggetto", "pages_news")
processo("soggetto", "pages_sport")
processo("soggetto", "pages_assist_e_cont_clienti")
processo("soggetto", "vendor", type="factor")
processo("soggetto", "piattaforma", type="factor")
# processo("soggetto", "old", type="factor")
processo("soggetto", "n_days_visite", type="giorni")






mytable <- table(cookies2[,"soggetto"],cookies2[,"old"])
print(mytable)
# prop.table(mytable, 1) # row percentages
# prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
# print(fit)
# summary(fit) # extended results
titolo<-paste("CA",var1, "contro" ,var2)
par(cex.lab=1, cex=3)
print(plot(fit))# symmetric map
