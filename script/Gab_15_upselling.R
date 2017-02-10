# Mappe di Kohonen

setwd("/home/gabriele/SKY")

#popolazione: prendo il file cookies2 che contiene 687.960 di cookies
library(dplyr)

load("sample2.1/cookies")

var_insPack<-names(cookies)[354:365]
var_acqBool<-names(cookies)[366:377]
var_Nacq<-names(cookies)[378:389]

#chi mette nel carrello/acquista qualcosa cosa altro acquista
nomi<-c("globale","calcio", "sport", "cinema", "famiglia", "HD","tecnologiasky")
soggetti<-c("soggetto","soggetto_calcio", "soggetto_sport", "soggetto_cinema", "soggetto_famiglia","soggetto_HD","soggetto_tecnologiasky")

tabella_acq<-matrix(ncol=7,nrow=7)
rownames(tabella_acq)<-nomi
colnames(tabella_acq)<-nomi


tabella_car<-matrix(ncol=7,nrow=7)
rownames(tabella_car)<-nomi
colnames(tabella_car)<-nomi

tabella_car_acq<-matrix(ncol=7,nrow=7)
rownames(tabella_car_acq)<-nomi
colnames(tabella_car_acq)<-nomi


for(i in 1:7){
  for(j in 1:7){
print(c(soggetti[i],soggetti[j]))
print(table(cookies[,soggetti[i]],cookies[,soggetti[j]]))
a<-table(cookies[,soggetti[i]],cookies[,soggetti[j]])[2,2]
tabella_acq[i,j]<-a

b<-table(cookies[,soggetti[i]],cookies[,soggetti[j]])[1,1]
tabella_car[i,j]<-b

e<-table(cookies[,soggetti[i]],cookies[,soggetti[j]])[1,1]
tabella_car[i,j]<-e

}}


tabella_acq<-tabella_acq[-1,-1]
tabella_car<-tabella_car[-1,-1]

tabella_car
tabella_acq




cookies_acq<-cookies[,343:389]
table(cookies$tot_inserimenti_PacchettoCalcio[cookies$tot_inserimenti_PacchettoCalcio>0])
table(cookies$tot_acquisti_PacchettoCalcio[cookies$tot_acquisti_PacchettoCalcio>0])


for (j in 2:7){
cookies_sub<-cookies[cookies[,soggetti[j]]%in%c("compra") & cookies[,var_Nacq[j]]!=0,]

for(i in 2:7){
conversione<-cookies_sub[,var_insPack[i]]/cookies_sub[,var_Nacq[i]]
print(paste("ha comprato",nomi[j],"compra",nomi[i]))
print(summary(conversione))

print(hist(conversione, main=paste("ha comprato",nomi[j],"compra",nomi[i])))

}}


