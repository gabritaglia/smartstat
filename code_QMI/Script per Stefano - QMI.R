require(randomForest)
require(quantregForest)
require(intervals)
setwd("C:/Users/Marco Fattore/Dropbox/Attività - Consulenza/QMI")
load("dati definitivi per Albero")

# Lo script da mettere in Shiny deve fare due cose principali:
# avere una sezione di back end in cui si costruiscono le foreste (normali e quantili)
# avere una sezione di front end in cui si applicano le foreste già calcolate ai dati desiderati
# Nelle parti di back end, lo script deve essere parametrico, nel senso che 
# i parametri passabili alle isruzioni "randomForest" e "quantregForest" devono
# poter essere scelti dall'utente.
# Nella parte di front end, l'utente dovrà poter inserire un insieme di informazioni esogene, che poi descrivo.
# Back end e front end sono connessi perché il front end usa le foreste generate (e quindi archiviate) nel back end
# e perché le covariate selezionate come utili da inserire nella foresta (il che avviene per tentativi) 
# dovranno essere le uniche disponibili sul front end.

#Back end - fase preliminare
#Costruzione db su cui si fa il training delle foreste

#I dati originari ricostruiti dal tuo script vengono assegnati al dataframe db
#Faccio alcune modifiche sul dataframe db perché le Foreste vogliono in input dei Factor (verifica) e perché voglio minimizzare i missing
# A. Assegno d'ufficio durata media alle campagne valide "per tutta la durata della programmazione"
# B. Trasformo in factor una serie di campi
# D. Identifco i campi inutili in db
# C. Identifico le covariate da tenere e genero l'elenco dei loro nomi (variabile "covariate")
# Nota bene. L'identificazione delle covariate da tenere o no fa parte del tuning del modello, perché
# poi le covariate tenute saranno quelle che loro potranno usare
# giocando con il prototipo.
# Quindi puoi anche decidere di tenerle tutte.
# Io ne ho omesse alcune perché facevano solo rumore.
# Scelgo l'outcome (redemption)




db<-dati
db$durata[is.na(db$durata)]<-mean(na.omit(db$durata))# attribuisco ai missing la durata media delle campagne
db$forfait<-as.factor(db$forfait)
db$mese<-as.factor(db$mese)
db$intero<-as.factor(db$intero)
db$modalita<-as.factor(db$modalita)
db$distribuzione<-as.factor(db$distribuzione)
db$cartaceo<-as.factor(db$cartaceo)

#covariate
campi_da_eliminare<-c("id_promozione", "cliente", "titolo", "data_inizio", "data_fine", "descrizione_meccanica_operazione", "modalita_operazione", "modalita_distribuzione", "numero_buoni", "film", "conta_caratteri_film", "ean", "prezzo_unitario", "quantiBigliettiRedenti", "importoRedento", "redemption", "p.h.stampati", "p.h.codici.generati")
covariate.tutte<-db[,!(colnames(db) %in% campi_da_eliminare)]
covariate_da_eliminare<-c("natale", "capodanno", "epifania", "pasqua", "pasquetta", "aprile_25", "maggio_1", "giugno_2", "ferragosto", "santi", "immacolata", "p.h.carnet_mensile", "p.h.numero_giorni_validita_da_associazione", "lunedi", "martedi", "mercoledi", "giovedi", "venerdi", "sabato", "domenica")
covariate<-covariate.tutte[,!(colnames(covariate.tutte) %in% covariate_da_eliminare)]
#Outcome
redemption<-db$redemption

# Da "db" genero "db.baseline" che è il db depurato delle cose inutili
# da cui poi estraggo i dati per le elaborazioni
# Trasformo gli NA della variabile "plurale" in un livello fittizio "NS" (che sta per "Non Specificato")
# in modo che le foreste possano trattare il (di fatto) dato mancante come livello.
# Naturalmente sto inserendo un bias, perché "NS" non è ordinabile in maniera oggettiva rispetto alle altre modalità
# Tutto dipende dal fatto che c'erano molti missing.
# Se nei dati nuovi non ci sono più, allora questo passaggio è inutile.

db.baseline<-data.frame(redemption, covariate)
levels(db.baseline$plurale)<-c("NS", levels(db.baseline$plurale))
db.baseline$plurale[is.na(db.baseline$plurale)]<-"NS"

#db.baseline è la base dati di rierimento da cui parto per le analisi.
# siccome il previsore l'ho costruito a sua volta su un sottoinsieme di db.baseline.
#ho introdotto il dataframe db.input che contiene la selezione dei record su cui costruisco la foresta.

#Back end - Costruzione foresta casuale
set.seed(10)
anno.iniziale<-2010
anno.finale<-2013
db.input<-db.baseline[db.baseline$anno>=anno.iniziale & db.baseline$anno<=anno.finale,]

#Splitto il campione (i record di db.input) in campione di training (trn) e di test
trn<-sample(1:nrow(db.input), floor(nrow(db.input)*9/10))
test<-!(1:nrow(db.input) %in% trn)

#Back end: Costruisco la foresta casuale (che assegno alla variabile "rf")
#dentro rf c'è la varianza spiegata (da mostrare nell'output)

rf<-randomForest(db.input[trn, colnames(covariate)], db.input$redemption[trn], ntree=1000, importance=TRUE)


#Estraggo le variabili rilevanti per la previsione
ord.var<-order(rf$importance[,1], decreasing=TRUE)
imp<-rf$importance[ord.var,1]/(sum(rf$importance[,1]))*100 #normalizzo l'importanza
imp
plot(imp, type="b", pch=19, col="blue")

#Confronto le previsioni della foresta con i dati di test

pre.test<-predict(rf, newdata=db.input[test,colnames(covariate)])
z<-db.input[test , c("redemption")]
plot(cbind(z, pre.test), xlim=c(0,100), ylim=c(0,100), pch=20, col="white")
abline(0, 1, col="white")


# BAck end regressione quantile
# Per tenere separate le cose, riassegno db.input al dataframe db.qrf
# (sto replicando troppe volte il db, si può rendere più efficiente)
# e calcolo la foresta quantile


db.qrf<-db.input[trn,colnames(covariate)]
qrf <- quantregForest(x=db.qrf, y=db.input$redemption[trn]) #Stimo i quantili sui dati di training
# NB. La foresta casuale e la foresta "quantile" devono essere costruite sullo stesso set di variabili


#Front end
#Nel front end si deve poter fare principalmente una cosa
#Il front end eredita dal back end rf e qrf
# Passare dei profili di campagne al sistema + altri parametri e ottenere dal sistema delle risposte (numeriche e o grafiche)

# Le informazioni da passare sono:
# Il profilo della campagna ("profilo.campagna"), ottenuto da menu a tendina sulle valore delle dimensioni sulle quali è costruita la foresta casuale
# Il rischio "rsk" che si è disposti a correre che la redemption reale superi quella ipotizzata



red<-predict(rf, newdata=profilo.campagna)
qrtl  <- predict(qrf, newdata= profilo.campagna, c(.25, .5, .75))
prc<-<- predict(qrf, newdata= profilo.campagna, seq(from = ,1, to = ,99, by= .01))
sgl  <- predict(qrf, newdata= profilo.campagna, rsk)

# red contiene la redemption prevista
#qrtl contiene primo quartile, mediana e secondo quartile
# prc contiene i percentili
#sgl contiene il valore di soglia corrispondente a un rischio di superamento pari a "rsk"

#L'output del front end è red, qrtl e sgl + un grafico che riporta prc sulle y e seq(from = ,1, to = ,99, by= .01) sulle x.

#Poi facciamo altre cose, ma ne parliamo a voce. 

