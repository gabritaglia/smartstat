
# Modello di targeting IGPDecaux - Versione 1.1

#Analisi di sensitività dei parametri del processo di targeting IGPDecaux

#Definizione funzione omnicomprensiva
#dai dati e parametri al rank
modellotargetingIGP<-function(DATI,
                              H=0.06,
                              K=0.467,
                              pesi.classi=c(0, 1, 0.4, 0.3, 0.2),
                              pesi.cluster=c(0.1, 0.3, 0.7, 1.0),
                              pesi.territorio=c(0.4, 1)){

#creazione della variabile inv2013= totale investimenti in advertising nell'anno 2013
  DATI["inv2013"]<-DATI$OUTOFHOME+DATI$QUOTIDIANI+DATI$CINEMA+DATI$INTERNET+DATI$PERIODICI+DATI$RADIO+DATI$TELEVISIONI+DATI$PROFESSIONALI+DATI$FREEPAYPRESS
    #Quote di mercato OOH e IGP
#Questa prima funzione crea le quote di mercato per ogni unità statistica (che chiamo poi azienda)
#Quote.OOH = quota di investimento dell'azienda in OUTOFHOME (OOH), sul totale del proprio investimento in advertising
#Quote.IGP = quota di investimento dell'azienda in IGPDecaux, sul totale del suo investimento in OOH
Quote.OOH.IGP<-function(dati){  
  Quote.OOH<-dati$OUTOFHOME/dati$inv2013
  
  Quote.IGP<-vector("numeric", nrow(dati))
  Quote.IGP[which(dati$OUTOFHOME==0)]<-0
  Quote.IGP[which(dati$OUTOFHOME>0)]<-dati[dati$OUTOFHOME>0, "IGP2013"]/dati[dati$OUTOFHOME>0, "OUTOFHOME"]
  Quote<-data.frame(Quote.OOH, Quote.IGP)
  Quote
}

#propensione 1 PR1
#Detti
# H = quota di mercato di OOH sul totale del mercato dell'advertising
# K = quota di IGPDecaux sul totale del volume di OOH nell'anno precedente
# h = Quote.OOH (vedi sopra)
# k = Quote.IGP (vedi sopra)
# pesi classi = valori di PR1 associati a ciascuno dei 5 gruppi 
# La funzione seguente crea il punteggio PR1 per ciascuna azienda
#La funzione classePR1 è puramente ausiliaria a PR1
# crea per ogni azienda la variabile classePR1 che indica il gruppo PR1 di appartenza (uno dei 5)
classePR1<-function(dati, H, K, h, k){
  hk<-cbind(h,k)
  classePR1<-vector("numeric", nrow(dati))
  classePR1[which(hk[,1]>=H & hk[,2]>=K )]<-1
  classePR1[which(hk[,1]>=H & hk[,2]<K )]<-2
  classePR1[which(hk[,1]<H & hk[,2]<K & hk[,1]>0 )]<-3
  classePR1[which(hk[,1]<H & hk[,2]>=K & hk[,1]>0 )]<-4
  classePR1[which(hk[,1]==0 & hk[,2]==0)]<-5
  classePR1
}
propensione.1<-function(classePR1 ,pesi.classi){
  PR1<-vector("numeric", length(classePR1))
  PR1[which(classePR1==1)]<-pesi.classi[1]
  PR1[which(classePR1==2)]<-pesi.classi[2]
  PR1[which(classePR1==3)]<-pesi.classi[3]
  PR1[which(classePR1==4)]<-pesi.classi[4]
  PR1[which(classePR1==5)]<-pesi.classi[5]
  PR1
}
#propensione 2 PR2
# La funzione calcola
# la quota di aziende della categoria che hanno investito in OOH (prop.cat) (frequenza relativa di categoria)
# e la associa tale quota a ciascuna azienda (prop.azi) 
# quindi ad ogni azienda è associata la freq relativa di aziende della sua categoria che hanno investito in OOH l'anno precedente
# Su questo valore viene fatta una cluster analysis con metodo delle k medie con k=4
# I 4 gruppi vengono poi ordinati rispetto al centroide dal minore al maggiore
# A ciascun gruppo viene associato il punteggio pesi.cluster che determina PR2 
propensione.2<-function(dati, pesi.cluster){ 
  #calcolo la quota di aziende che investono in OOH per ciascuna categoria
  propensione.cat<-data.frame(table(dati$DESCAT[which(dati$OUTOFHOME>0)])/table(dati$DESCAT))
  names(propensione.cat)<-c("DESCAT", "prop.cat")
  propensione.cat$prop.cat<-round(propensione.cat$prop.cat,digits=6) #arrotondo come fanno loro
    
  #aggiungo a dati il valore di propensione per ciascuna categoria
  dati.prop.cat<-merge(dati, propensione.cat, by.x="DESCAT", by.y="DESCAT")
  prop.azi<-dati.prop.cat$prop.cat 
  
  require(classInt)
  classi<-classIntervals(propensione.cat$prop.cat, n=4, style="jenks") #di che metodo si tratta?
  #NOTA!! gli intervalli vengono fatti su prop.cat NON su prop.azi
  soglia1<-classi$brks[2]
  soglia2<-classi$brks[3]
  soglia3<-classi$brks[4]
  PR2<-vector("numeric",nrow(dati))
  PR2[prop.azi<=soglia1] <- pesi.cluster[1]
  PR2[prop.azi>soglia1 & prop.azi<=soglia2] <- pesi.cluster[2]
  PR2[prop.azi>soglia2 & prop.azi<=soglia3] <- pesi.cluster[3]
  PR2[prop.azi>soglia3] <- pesi.cluster[4]
  PR2
}

#propensione 3 PR3
# La funzione associa a ciascuna azienda i punteggi pesi.territorio
# a seconda se l'azienda è territoria o no
# il punteggio determina il valore di PR3
propensione.3<-function(dati, pesi.territorio){
  PR3<-vector("numeric", nrow(dati))
  PR3[which(dati$Territorio.2==0)]<-pesi.territorio[1]
  PR3[which(dati$Territorio.2==1)]<-pesi.territorio[2]
  PR3
}

#potenziale PE
#detti
# classePR1 (vedi sopra)
# H quota di mercato di OOH sul totale del mercato dell'advertising (come sopra)
# K quota di IGPDecaux sul totale del volume di OOH nell'anno precedente (come sopra)
# inv2013 = investimento dell'azienda in advertising, nell'anno precedente
# OOH = investimento dell'azienda in OOH, nell'anno precedente
# IGP = investimento dell'azienda in IGPDecaux, nell'anno precedente
# Viene creato il punteggio PE per ogni azienda con calcoli diversi a seconda del gruppo
# per la classe 2 (MANTENIMENTO) non viene calcolato e viene inizializzato a valore mancante (NA)
potenziale<-function(dati, classePR1, H, K){ 
  OOH<-dati$OUTOFHOME
  IGP<-dati$IGP2013
  
  PE<-vector("numeric", nrow(dati))
  cl1<-which(classePR1==1); PE[cl1]<-dati$inv2013[cl1]*H*K-IGP[cl1]
  cl2<-which(classePR1==2); PE[cl2]<-OOH[cl2]*K-IGP[cl2]
  cl3<-which(classePR1==3); PE[cl3]<-dati$inv2013[cl3]*H*K-IGP[cl3]
  cl4<-which(classePR1==4); PE[cl4]<-dati$inv2013[cl4]*H*1-IGP[cl4]  
  cl5<-which(classePR1==5); PE[cl5]<-dati$inv2013[cl5]*H*K-IGP[cl5]
  #per le classi 3 5 1 il potenziale viene calcolato nel medesimo modo
  PE<-round(PE, digits=0)
  PE
}

#fine definizione funzioni

#primo passaggio Debora ordinamento per DESCAT
#Ordina per DESCAT
DATI<-DATI[order(DATI$DESCAT),]

#Calcolo quote investimento in OOH e IGP per unità statistica
Quote.OOH<-Quote.OOH.IGP(DATI)[,1]  #quota di investimento dell'azienda in OUTOFHOME (OOH), sul totale del proprio investimento in advertising
Quote.IGP<-Quote.OOH.IGP(DATI)[,2]  #quota di investimento dell'azienda in IGPDecaux, sul totale del suo investimento in OOH

#Calcolo punteggi di propensione PR1 PR2 PR3 PR
classePR1<-classePR1(DATI, H=soglia.quota.ooh, K=soglia.quota.IGP, h=Quote.OOH, k=Quote.IGP)
PR1<-propensione.1(classePR1=classePR1, pesi.classi=pesi.classi)
PR2<-propensione.2(dati=DATI, pesi.cluster=pesi.cluster)
PR3<-propensione.3(dati=DATI, pesi.territorio=pesi.territorio)
#il calcolo della propensione totale PR è data dalla somma di PR1 PR2 PR3
PR<-PR1+PR2+PR3

#Calcolo punteggi di potenziale PE
PE<-potenziale(dati=DATI, H=soglia.quota.ooh, K=soglia.quota.IGP, classePR1=classePR1)



DATI.punteggi<-cbind(DATI,classePR1,PR1, PR2, PR3, PR, PE)

#ATTENZIONE
#prima di creare il ranking viene assegnato punteggio 0 alla azienda "30842/PICCOLI INSERZIONISTI"
DATI.punteggi$PR[which(DATI.punteggi$AZICAT== "30842/PICCOLI INSERZIONISTI")]<-0

#Ranking
#le aziende vengono ordinate rispetto a PR e poi rispetto a PE.
#questo ordine determina il ranking delle aziende

rankIGP<-function(dati.punteggi)
{ #
  #prima vengono ordinati i potenziali rispetto a PR e PE
  potenziali<-dati.punteggi[which(dati.punteggi$classePR1!= 1),]
  potenziali<-potenziali[order(-potenziali$PR, -potenziali$PE),]
  #poi vengono aggiunti gli EX (che non abbiamo)
  #
  #poi vengono ordinati i mantenimento rispetto all'investimento in advertising 2013
  mantenimento<-dati.punteggi[which(dati.punteggi$classePR1== 1),]
  mantenimento<-mantenimento[order(-mantenimento$inv2013),]
  
  #poi metto i NUOVI (che non abbiamo)
  #
  #e infine concateno il tutto
  dati.punteggi.rank<-rbind(potenziali, mantenimento)
  RANK<-1:nrow(dati.punteggi.rank)
  dati.punteggi.rank<-cbind(dati.punteggi.rank,RANK)
  # e aggiungo il rank
  return(dati.punteggi.rank)
}
DATI.punteggi.rank<-rankIGP(dati.punteggi=DATI.punteggi)

return(DATI.punteggi.rank)
}
###########
#   Main  #
###########
#Importazione dei dati
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting")
Anse2013<-read.csv("Anse2013.csv", header=TRUE, sep=",", dec=".")

#Impostazione dei Parametri
#Quota di mercato di OOH sul totale del mercato dell'advertising nell'anno precedente
soglia.quota.ooh<-0.06  #(parametro H delle funzioni precedenti)
#Quota di IGPDecaux sul totale del volume di OOH nell'anno precedente
soglia.quota.IGP<-0.467  #(parametro K delle funzioni precedenti)
#valori del punteggio PR1 per le 5 classi di PR1
pesi.classi<-c(0, 1, 0.4, 0.3, 0.2)
#valori del punteggio di PR2 per ciascuno dei k=4 cluster (delle k medie)
pesi.cluster<-c(0.1, 0.3, 0.7, 1.0)
#valori del punteggio di PR3 (azienda territoriale)
pesi.territorio<-c(0.4, 1) #prima componente relativa a NO, seconda a SI


IGPDecaux<-modellotargetingIGP(DATI=Anse2013,
                         H= soglia.quota.ooh,
                         K=soglia.quota.IGP,
                         pesi.classi=pesi.classi,
                         pesi.cluster=pesi.cluster,
                         pesi.territorio=pesi.territorio)

#Salvataggio
#salvo la tabella dati che include i punteggi e il ranking
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/IGPDecaux/Progetto II - 2014 - Processo Targeting")
#write.table(IGPDecaux, "Anse2013 Ranking attuale SmartStat v2.0 (ALLINEATO).csv", sep=",", dec=".",row.names=T)
##
