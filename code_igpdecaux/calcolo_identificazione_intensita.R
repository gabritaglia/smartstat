#8 ottobre 2014
#in questo script si calcolano i due punteggi identificazione e intensità a partire dal poset identificato con Debora.
library(parsec)
set.seed(7)


#Poset definitivo:
PR1.SCALA<-c(1,2,3,4)   #Scala delle modalit? della prima variabile
PR2.SCALA<-c(1,2,3,4)   #Scala delle modalit? della seconda variabile
PR3.SCALA<-c(1,2)       #Scala delle modalit? della terza variabile
vrb<-list(PR1.SCALA, PR2.SCALA, PR3.SCALA)      #Lista delle variabili
prf<-var2prof(vrb)      #Costruisco tutti i profili possibili (combinazioni di punteggi sulle tre variabili)
Z<-getzeta(prf)     #Costruisco la matrice Z del poset
G<-incidence2cover(Z)
#T = colonna copre riga 
#F = colonna non copre riga/colonna incomparabile riga
#G[ "X" , "Y"]<-T # Y copre X
G["341", "332"]<-T #332 copre 341
G["341", "412"]<-T #412 copre 341
G["341", "242"]<-T #242 copre 341
G["242", "411"]<-T #411 copre 242
G["321", "411"]<-T #411 copre 321
G["112", "321"]<-T #321 copre 112
Z<-cover2incidence(G)
HasseDiagram(Z, prf)

#stabilisco soglia e iterazioni
threshold<-soglia<-"142"
iterazioni<-10000000

indicator.computation<-function(threshold, iterazioni, Z=Z.1212){
  eval<-evaluation(zeta=Z, threshold=threshold, nit=iterazioni, inequality=FALSE)
  #identification_function -> vector that reports the relative frequency of times that a profile is equal or lower than the threshold of the linear extension.
  identificazione<-1-eval$identification_function #=>relative frequency of times that a profile is greater than the threshold of the linear extension
  #point_absolute_wealth_gap  ->  vector that reports the average absolute distance from the threshold of the linear extension. This gap is set equal to 0 when the profile is lower than or equal to the threshold in the linear extension.
  intensita<-eval$threshold
  NotDownset<-which(eval$point_absolute_wealth_gap!=0)
  intensita[NotDownset]<-eval$point_absolute_wealth_gap[NotDownset]
  intensita[NotDownset]<-intensita[NotDownset]/identificazione[NotDownset] #sistemo il calcolo dell'intensita facendo si che calcoli la media solo per quelle estensioni lineari in cui il profilo è sopra la soglia
  intensita[NotDownset]<-intensita[NotDownset]/max(intensita[NotDownset])
  #adesso metto una distanza negativa a chi sta sotto la soglia
  #point_absolute_poverty_gap ->  vector that reports the average absolute distance from the first profile greater than the threshold of the linear extension. This gap is set equal to 0 when the profile is greater than the threshold in the linear extension.
  intensita[-NotDownset]<-eval$point_absolute_poverty_gap[-NotDownset]-1
  intensita[-NotDownset]<--intensita[-NotDownset]/eval$identification_function[-NotDownset]#conto inutile in quanto il downset è sempre uguale o minore alla soglia  
  intensita[-NotDownset]<-intensita[-NotDownset]/max(abs(intensita[-NotDownset]))
  intensita
  inds<-cbind(identificazione, intensita)
  inds<-data.frame(inds, profilo=row.names(inds))
}

indicatori<-indicator.computation(threshold, iterazioni) 

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting/Confronto Poset- metodo attuale - Sett 2014")
sintesi.poset.2012<-read.csv("sintesi.poset.2012.csv")
sintesi.poset.2013<-read.csv("sintesi.poset.2013.csv")
dati<-merge(sintesi.poset.2012,sintesi.poset.2013, by="AZICAT",all=T);rm(sintesi.poset.2012, sintesi.poset.2013)

#aggiungo i punteggi

#creo un "ranking poset", ordinando per Propensione e poi per intensità
dati[order(-dati$Propensione, -dati$Intensita),"rank.poset" ]<-1:nrow(dati)


COLORI<-rep("gray",32);names(COLORI)=names(prf$freq)
D<-downset(Z, SOGLIA)
U<-upset(Z, SOGLIA)
COLORI[which(U==T)]<-"green";COLORI[which(D==T)]<-"red"
HasseDiagram(Z, prf, colori=COLORI)


cols<-cbind(profilo=names(COLORI),COLORI)
indicatori<-merge(indicatori, cols, by="profilo")


#--- Output grafici
table(indicatori$identificazione)
table(indicatori$intensita)

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting/Confronto Poset- metodo attuale - Sett 2014")
pdf("Scatter_punteggi.pdf")
plot(indicatori$identificazione, indicatori$intensita, col=COLORI, pch=16, ylab="Quanto è distante dalla soglia", xlab="Quanto è ''probabile'' che sia sopra la soglia")
with(indicatori, text(intensita[NotDownset]~identificazione[NotDownset], labels =profilo[NotDownset], cex=0.7, pos=3))
with(indicatori, text(intensita[-NotDownset]~identificazione[-NotDownset], labels =profilo[-NotDownset], cex=0.7, pos=4))
dev.off()






p