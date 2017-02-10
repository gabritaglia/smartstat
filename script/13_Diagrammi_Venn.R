library(dplyr)

load("sample2/cookies")

a<-cookies %>%
    filter(carrello==1) %>%
    filter(PacchettoCinema==1 & PacchettoSport==1 & PacchettoCalcio==0) %>%
    filter()
nrow(a)

a<-data.frame(table(prodotti_sample$products))


products<-as.character(unique(prodotti_sample$products))
pacchetti <- strsplit(products, split="\\+")

f <- function(x) x[-grep("SkyTV|MySkyHD",x)]
pacchetti<-lapply(pacchetti,f)

qualepacchetto<-function(nome){
  yes<-sapply(nome, grepl, pacchetti, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  yes<-as.numeric(yes)
  return(yes)
}

PacchettoCalcio<-qualepacchetto("Calcio")
PacchettoCinema<-qualepacchetto("Cinema")
PacchettoSport<-qualepacchetto("Sport")
PacchettoFamiglia<-qualepacchetto("SkyFamiglia")
PacchettoHD<-qualepacchetto("HD")
products<-data.frame(cbind(products, PacchettoCalcio, PacchettoCinema, PacchettoSport,PacchettoFamiglia, PacchettoHD))
pd<-merge(prodotti_sample,products, by="products")

pd[,8:12]<-apply(pd[,8:12],2,function (x) as.numeric(as.character(x)))
apply(pd[,8:12],2,sum)


