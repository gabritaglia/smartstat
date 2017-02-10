setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting")
rank<-read.csv("Anse2013 Ranking attuale SmartStat v2.0 (ALLINEATO).csv", header=TRUE, sep=",", dec=".")

#cerco il valore soglia 
# loro prendono i primi 5000 come top
#profilo del 5000°
dati[which(dati$RANK==5000),c("Profilo")]
dati[which(dati$RANK==5001),c("PR1","PR2","PR3")]
apply(dati[,c("PR1","PR2","PR3")],2, table)

oldwd<-getwd()
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting/Confronto Poset- metodo attuale")
pdf("Distribuzione ranking tra i profili.pdf")
par(mfrow=c(3,3))
r<-seq(1,18000, by=2000)
for (i in r){
t<-table(dati[which(dati$RANK %in% seq(i,i+999,1)),c("Profilo")])
barplot(t, main=(paste0("rank ",i, ":",i+1999)))
}
dev.off()
setwd(oldwd)

