#apertura files

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting")
datiSmartStat<-read.csv("Anse2013.csv", header=TRUE, sep=",", dec=".")
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux")
datiUfficiale<-read.csv("Ranking Ufficiale Aziende IGPDecaux.csv", header=TRUE, sep=",")

nrow(datiUfficiale)-nrow(datiSmartStat)
table(classePR1)
