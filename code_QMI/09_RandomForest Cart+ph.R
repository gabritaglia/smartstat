#QMI
#DATI NUOVI 19 Febbraio 2015
dati<-read.csv2("dati originali/2015-02-19 statistiche_promozioni.csv")

d$dati<-dati[dati$promo_tipologia=="Print@home",]
names(dati)

table(dati$buoni_inizio_validita_aa)
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
dati$promo_id[dati$codici_inizio=="0/0/0000"] #nessuno ottimo
dati$promo_id[dati$codici_fine=="0/0/0000"] #nessuno ottimo
summary(dati$codici_durata)
table(dati$codici_numero_utilizzi) #nessuna anomalia
#codici_redemption_digitale
no1<-dati$promo_id[dati$codici_redemption_digitale>100]
no1
table(dati$codici_redemption_digitale)
summary(dati$codici_numero_utilizzi)
summary(dati$codici_inseriti)
nonzeri<-dati$codici_inseriti!=0
codici_redemption_digitale<-dati$codici_redemption_digitale
codici_redemption_digitale[!nonzeri]<-0
codici_redemption_digitale[nonzeri]<-dati$codici_stampati[nonzeri]/(dati$codici_inseriti[nonzeri]*dati$codici_numero_utilizzi[nonzeri])*100
rm(nonzeri)
control<-cbind(dati$codici_redemption_digitale,codici_redemption_digitale)
summary(dati$codici_redemption_digitale)
summary(codici_redemption_digitale,)
#codici_conversione
no2<-dati$promo_id[dati$codici_conversione>100]
no2
no3<-dati$promo_id[dati$set_anomalo!=0] #coincide con no1+no2
no3%in%unique(c(no1,no2))
rm(no3)

#introduco la variabile promo_dati_definitivi_smartstat
#per cui consideriamo definitive le promozioni la cui dati di buoni_fine_validita è inferiore al 31/12/2014.
buoni_fine_validita<-as.Date(dati$buoni_fine_validita, format="%d/%m/%Y")
promo_dati_definitivi_smartstat<-buoni_fine_validita<as.Date("2014-12-31")
# sum(promo_dati_definitivi_smartstat)
no4<-dati$promo_id[!promo_dati_definitivi_smartstat]
#SISTEMAZIONE VARIABILI
# length(levels(dati1$cliente_settore))
# length(levels(dati1$cliente_sottosettore))
# table(dati1$buoni_film_target, useNA="always")
# table(dati1$buoni_film_genere1, useNA="always")
# table(dati1$buoni_film_genere2, useNA="always")
# table(dati1$buoni_film_genere1, useNA="always")
# table(dati1$buoni_film_genere3, useNA="always")
# table(dati1$codici_carnet, useNA="always")
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

#creo la variabile n_film che indica quanti film diversi si possono andare a vedere con il buono
titolo<-dati$buoni_film_titolo
titolo<-strsplit(as.character(titolo), "\\|")
titolo1<-unlist(lapply(titolo, function(x)x[1]))
titolon<-unlist(lapply(titolo, length))
dati$n_film[which(titolon==1)]<-"1"
dati$n_film[which(titolon!=1 | titolo1=="qualsiasi")]<-"2+"
dati$n_film<-factor(dati$n_film)
rm(titolo, titolo1, titolon)
#creo la variabile settore2 che era utile nel mondo Cartaceo
cbind(table(dati$cliente_settore),prop.table(table(dati$cliente_settore)))
dati[,"settore2"]<-as.character(dati$cliente_settore)
dati$settore2[which(dati$cliente_settore %in% c( "APPAREL",
                                                 "AUTOMOTIVE",
                                                 "CONSUMER ELECTRONICS",
                                                 "ENERGY&PETROL",
                                                 #"FINANCIAL&INSURANCE",
                                                 #"MEDIA&PUBLISHING",
                                                 "PUBLIC ADMIN",
                                                 "RETAIL",
                                                 "TELCO",
                                                 "TRAVEL"))]<-"ALTRO"
dati$settore2<-factor(dati$settore2)
table(dati$settore2)

feste<-apply(dati[,c("buoni_feste_capodanno",
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
                     "buoni_feste_ultimodellanno")],2,function(x)x<-as.numeric(as.character(x)))
dati$n_feste<-apply(feste,1,sum)
rm(feste)

we<-apply(dati[c("buoni_valido_dom","buoni_valido_sab","buoni_valido_ven")],2,function(x)x<-as.numeric(as.character(x)))
dati$n_days_weekend<-apply(we,1,sum)
dati$weekend<-dati$buoni_valido_dom==1|dati$buoni_valido_sab==1|dati$buoni_valido_ven==1
sum(dati$weekend)
rm(we)

var_names<-c("promo_meccanica",
             "promo_distribuzione_biglietti",
             "cliente_settore",
             "cliente_sottosettore",
             "buoni_inizio_validita_aa",
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
             "buoni_qta_emessa",
             "codici_durata",
             "codici_validita",
#              "codici_inseriti",
#              "codici_stampati",
             "codici_carnet",
             "codici_numero_utilizzi",
             "n_film",
             "settore2",
             "n_feste",
             "weekend",
             "n_days_weekend")
# var_names2<-var_names[!var_names%in%c("codici_inseriti","codici_stampati")]
var_names3<-c(var_names2, "promo_tipologia")
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

#DATI PER LA RANDOM FOREST
ok<-!dati$promo_id%in%c(no4,no1,no2) & dati$buoni_inizio_validita_aa%in%c(2011,2012,2013,2014)
dati1<-dati[ok,]
#################
#Splitto il campione (i record di dati) in campione di training  e di test
set.seed(10)
quota.train=9/10
sample<-sample(1:nrow(dati1), floor(nrow(dati1)*quota.train))
train1<-(1:nrow(dati1) %in% sample)
test1<-!(1:nrow(dati1) %in% sample)
#################################################################
#   RANDOM FOREST
library(randomForest)
#################################################################
pred.matrix1<-dati1[,var_names]
outcome1<-dati1[,"promo_redemption"]
rf<-randomForest(pred.matrix1[train1,],
                  outcome1[train1],
                  ntree=500,
                  importance=T)
rf
#################################################################
resultsRF(rf=rf,pred.matrix=pred.matrix1,outcome=outcome1, train=train1, test=test1)
importanzaVar(rf)




plot(outcome1[test1], pred.test, xlim=c(0,100), ylim=c(0,100))
abline(0,1)
# 

# 
# Results<-matrix(,nrow=length(variabili), ncol=3)
# colnames(Results)<-c("Nvar","%train","%test")
# Results
# for (i in nrow(Results):2){
#   var.sel<-which(rf1$importance[,1]>=var.rf[i,1])
#   pred.matrix.min<-pred.matrix[,var.sel]
#   set.seed(10)
#   rf.min<-randomForest( pred.matrix.min[train1,],
#                         outcome1[train1],
#                         ntree=1000)
#   Results[i,1]<-length(var.sel)
#   Results[i,2]<-X.VarExp(outcome1[train1], rf.min$predicted)
#   Results[i,3]<-X.VarExp(outcome1[test1], predict(rf.min, pred.matrix.min[test1,]))
# }
# 
# 
# #osservando il grafico risulta che la varianza spiegata usando una RF con poche variabili equivale a varianza spiegata usando tutte le variabili
# plot(Results[,1],Results[,2], pch=NA,
#      xlab="numero di variabili",
#      ylab="%varianza spiegata",
#      ylim=c(30,100))
# lines(Results[,1],Results[,2], col="red")
# lines(Results[,1],Results[,3], col="blue")
# abline(h=Results[nrow(Results),3], lty="dotted", col="green")
# abline(v=c(5,10,20,30,40,50))
# legend("topright", legend=c("Training Set","Test Set"),
#        fill=c("red","blue"), bty="n", cex=0.7, ncol=2)