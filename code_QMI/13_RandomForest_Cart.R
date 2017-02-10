#QMI
require(randomForest)
require(quantregForest)
require(intervals)

#DATI NUOVI 10 Marzo 2015
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")

dati<-dati[dati$promo_tipologia=="Cartaceo",]
names(dati)

table(dati$buoni_inizio_validita_aa)



#PULIZIA:
dati$promo_id[is.na(dati$cliente_sottosettore)] #nessuno ottimo
dati$promo_id[is.na(dati$cliente_settore)] #nessuno ottimo
t<-table(dati$buoni_dati_promo_definitivi, dati$buoni_inizio_validita_aa) 
t #ridotte le non definitive 2013, aumentate le non definitive 2014+2015
prop.table(t,2)
rm(t)
no0<-dati$promo_id[dati$buoni_dati_promo_definitivi==0] #141 casi (troppi?)
no0
no3<-dati$promo_id[dati$set_anomalo!=0] #coincide con no1+no2
# no3%in%unique(c(no1,no2))
rm(no3)

#SISTEMAZIONE VARIABILI
length(levels(dati$cliente_settore))
length(levels(dati$cliente_sottosettore))
table(dati$buoni_film_target, useNA="always")
table(dati$buoni_film_genere1, useNA="always")
table(dati$buoni_film_genere2, useNA="always")
table(dati$buoni_film_genere1, useNA="always")
table(dati$buoni_film_genere3, useNA="always")
table(dati$codici_carnet, useNA="always")
str(dati)
tofactor<-c( "buoni_inizio_validita_aa",
             "buoni_stagione_primavera",
             "buoni_stagione_estate",
             "buoni_stagione_autunno",
             "buoni_stagione_inverno",
             "buoni_feste_capodanno",
             "buoni_feste_epifania",
             "buoni_feste_sanvalentino",
             "buoni_feste_venticinqueaprile",
             "buoni_feste_primomaggio",
             "buoni_feste_pasqua",
             "buoni_feste_duegiugno",
             "buoni_feste_ferragosto",
             "buoni_feste_primonovembre",
             "buoni_feste_ottodicembre",
             "buoni_feste_natale",
             "buoni_feste_santostefano",
             "buoni_feste_ultimodellanno",
             "buoni_per_tutta_la_durata",
             "buoni_3d",
             "buoni_valido_lun",
             "buoni_valido_mar",
             "buoni_valido_mer",
             "buoni_valido_gio",
             "buoni_valido_ven",
             "buoni_valido_sab",
             "buoni_valido_dom",
             "codici_carnet"
)
dati[,tofactor]<-data.frame(apply(dati[,tofactor],2,function(x) factor(as.character(x))))
rm(tofactor)
# levels(dati$buoni_film_target)[1]<-NA
levels(dati$buoni_film_target)[levels(dati$buoni_film_target)%in%c(" Per tutti","Per tutti")]<-"Per tutti"
levels(dati$buoni_film_target)[levels(dati$buoni_film_target)%in%c("Solo adulti","Solo adulti ")]<-"Solo adulti"
# levels(dati$buoni_film_genere1)[1]<-NA
# levels(dati$buoni_film_genere2)[1]<-NA
# levels(dati$buoni_film_genere3)[1]<-NA

#ELENCO DELLE VARIABILI SU CUI SI FARA' LA RANDOM FOREST
var_names<-c("promo_meccanica",
             "promo_distribuzione_biglietti",
             "cliente_settore",
             "cliente_sottosettore",
#              "buoni_inizio_validita_aa",
             "buoni_durata",
             "buoni_stagione_primavera",
             "buoni_stagione_estate",
             "buoni_stagione_autunno",
             "buoni_stagione_inverno",
             "buoni_feste_capodanno",
             "buoni_feste_epifania",
             "buoni_feste_sanvalentino",
             "buoni_feste_venticinqueaprile",
             "buoni_feste_primomaggio",
             "buoni_feste_pasqua",
             "buoni_feste_duegiugno",
             "buoni_feste_ferragosto",
             "buoni_feste_primonovembre",
             "buoni_feste_ottodicembre",
             "buoni_feste_natale",
             "buoni_feste_santostefano",
             "buoni_feste_ultimodellanno",
             "buoni_per_tutta_la_durata",
             "buoni_redemption_o_forfait",
             "buoni_intero_o_sconto",
             "buoni_tipologia",
             "buoni_film_target",
             "buoni_film_genere1",
             "buoni_film_genere2",
             "buoni_film_genere3",
             "buoni_3d",
             "buoni_valido_lun",
             "buoni_valido_mar",
             "buoni_valido_mer",
             "buoni_valido_gio",
             "buoni_valido_ven",
             "buoni_valido_sab",
             "buoni_valido_dom",
             "buoni_qta_emessa"
              )


#Funzione per la valutazione performance Random Forest
X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
#variable importance
importanzaVar<-function(rf){
  ord.var<-order(rf$importance[,1], decreasing=TRUE)
  var.rf<-data.frame(rf$importance[ord.var,1])
  print(plot(var.rf[,1], type="b", pch=19, col="blue", ylab="%incMSE"))
  text(var.rf[,1],label=row.names(var.rf),cex=0.7)
  return(var.rf)
}
#resultsRF
resultsRF<-function(rf,pred.matrix=pred.matrix1,outcome=outcome1, train=train1, test=test1){
  a<-X.VarExp(outcome[train], rf$predicted)
  pred.test<-predict(rf, pred.matrix[test,])
  b<-X.VarExp(outcome[test],pred.test)
  results<-list(X.VarExp.train=a,X.VarExp.test=b)
  return(results)}
#resultsRF_notrain
resultsRF_notrain<-function(rf,pred.matrix=pred.matrix1,outcome=outcome1){
  a<-X.VarExp(outcome, rf$predicted)
  results<-list(X.VarExp=a)
  return(results)}

#DATI PER LA RANDOM FOREST
ok<-!dati$promo_id%in%c(no0) & dati$buoni_inizio_validita_aa%in%c(2011,2012,2013,2014)
dati1<-dati[ok,]
#################
# #Splitto il campione (i record di dati) in campione di training  e di test
# set.seed(10)
# quota.train=9/10
# sample<-sample(1:nrow(dati1), floor(nrow(dati1)*quota.train))
# train1<-(1:nrow(dati1) %in% sample)
# test1<-!(1:nrow(dati1) %in% sample)
#################################################################
#   RANDOM FOREST
library(randomForest)
# #################################################################
# pred.matrix1<-dati1[,var_names]
# outcome1<-dati1[,"promo_redemption"]
# rf<-randomForest(pred.matrix1[train1,],
#                  outcome1[train1],
#                  ntree=500,
#                  importance=T)
# rf
# #################################################################
#################################################################
pred.matrix1<-dati1[,var_names]
outcome1<-dati1[,"promo_redemption"]
rf<-randomForest(pred.matrix1,
                 outcome1,
                 ntree=1000,
                 importance=T,
                 keep.forest=T)
rf
#################################################################
# resultsRF(rf=rf,pred.matrix=pred.matrix1,outcome=outcome1, train=train1, test=test1)
resultsRF_notrain(rf=rf,pred.matrix=pred.matrix1,outcome=outcome1)
importanzaVar(rf)

# pred.test<-predict(rf, pred.matrix1[test1,])
# plot(outcome1[test1], pred.test, xlim=c(0,100), ylim=c(0,100))
# abline(0,1)


#salvo l'oggetto random forest
rf_cartaceo<-rf
save(rf_cartaceo, file="Oggetti_per_Piattaforma/rf_cartaceo")



tree<-getTree(rf, k=1, labelVar=T)


prf<-predict(object=rf, newdata=pred.matrix1,
                          type="response", predict.all=T, nodes=T )

agg<-(prf$aggregate)
ind<-(prf$individual)


all(ind%in%tree$prediction)

a<-unique(c(ind))
