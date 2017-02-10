# Input: Dati di QMI certificati da noi
# Output: Foresta Casuale per il cartaceo + Foresta Quantile per il Cartaceo + profili delle campagne della popolazione di training
# Descrizione: La funzione fa tre cose. A) costruisce la foresta casuale per il cartaceo. Come popolazione di training, usiamo tutti i dati certificati dell’intervallo di tempo che abbiamo deciso. Lo script deve avere come parametri il numero di alberi della foresta e il numero di variabili sulle quali fare gli split. Possono andare bene anche i valori di default, ma per chiarezza, definisci delle variabili esplicite e passale alla funzione randomForest. B) costruisce i profili (elenco dei nodi dei singoli alberi) delle campagne usate per la costruzione della foresta. Per fare questo, basta far riprevedere alla foresta i dati stessi, con la funzione predict (vedi mio script). C) Costruisce la foresta quantile per il cartaceo. Ricordati di identificare in modo preciso e non ambiguo il file di dati di input (quello di QMI certificato da no). Potemmo chiamarlo QMI.carta.input.1. 





#QMI
require(randomForest)
require(quantregForest)
require(intervals)

#DATI NUOVI 10 Marzo 2015
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")


#ELENCO DELLE VARIABILI SU CUI SI FARA' LA RANDOM FOREST
var_names<-c("promo_meccanica",
             "promo_distribuzione_biglietti",
             "cliente_settore",
#              "cliente_sottosettore",
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

###################################################
#######    INIZIO  DEFINIZIONE  FUNZIONI    #######
###################################################

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
resultsRF<-function(rf,pred.matrix=pred.matrix1,outcome=outcome1){
  a<-X.VarExp(outcome, rf$predicted)
  results<-list(X.VarExp=a)
  return(results)}


PREPARAZIONEDATI<-function(DATI_INPUT=dati,
                           TIPOLOGIA="Cartaceo",
                           VARIABILI_INCLUSE=var_names,
                           VARIABILI_ESCLUSE="cliente_sottosettore",
                           OUTCOME="promo_redemption",
                           ANNI_TRAIN=c(2011,2012,2013,2014),
                           NOME_FILE_OUTPUT=""Oggetti_per_Piattaforma/dati_input""){
  
  
      if(!TIPOLOGIA %in% c("Cartaceo", "Print@home")) stop("scrivi come TIPOLOGIA Cartaceo o Print@home")
      if(is.null(VARIABILI_INCLUSE)) VARIABILI_INCLUSE=names(DATI_INPUT)
      if(!is.null(VARIABILI_ESCLUSE)) VARIABILI_INCLUSE<-VARIABILI_INCLUSE[!VARIABILI_INCLUSE%in%VARIABILI_ESCLUSE]
      #SISTEMAZIONE VARIABILI
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
      levels(dati$buoni_film_target)[levels(dati$buoni_film_target)%in%c(" Per tutti","Per tutti")]<-"Per tutti"
      levels(dati$buoni_film_target)[levels(dati$buoni_film_target)%in%c("Solo adulti","Solo adulti ")]<-"Solo adulti"
      no0<-dati$promo_id[dati$buoni_dati_promo_definitivi==0] 
      no1<-dati$promo_id[dati$set_anomalo!=0] 
      WHICH_PROMO_ESCLUSE=c(no0,no1)
      
      ok<-!DATI_INPUT$promo_id%in%c(WHICH_PROMO_ESCLUSE) & DATI_INPUT$buoni_inizio_validita_aa%in%ANNI_TRAIN & DATI_INPUT$promo_tipologia %in% TIPOLOGIA
      dati1<-DATI_INPUT[ok,]
      pred.matrix<-dati1[,VARIABILI_INCLUSE]
      outcome<-dati1[,OUTCOME]
      dati_a_posto<-cbind(outcome,pred.matrix)
      save(dati_a_posto, file=NOME_FILE_OUTPUT )
      return(dati_a_posto)
      }



                    
RANDOMFOREST<-function(DATI_INPUT=dati_a_posto,
                       NTREE=1000,
                       N_VAR_SPLIT= "default",
                       NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_cartaceo"){
    #DATI PER LA RANDOM FOREST
    outcome<-DATI_INPUT[,"outcome"]
    pred.matrix<-DATI_INPUT[,2:ncol(DATI_INPUT)]
    if(N_VAR_SPLIT=="default")N_VAR_SPLIT=max(floor(ncol(DATI_INPUT)/3))
    rf<-randomForest(pred.matrix,
                     outcome,
                     ntree=NTREE,
                     mtry=N_VAR_SPLIT,
                     importance=T,
                     keep.forest=T)
    #################################################################
    print(resultsRF(rf=rf,pred.matrix=pred.matrix,outcome=outcome))
    #importanzaVar(rf)
    #salvo l'oggetto random forest
    rf_cartaceo<-rf
    save(rf_cartaceo, file=NOME_FILE_OUTPUT)
    return(rf)
    }





QUANTILERANDOMFOREST<-function(DATI_INPUT=dati_a_posto,
                               NTREE=100,
                               N_VAR_SPLIT= "default",
                               ANNI_TRAIN=c(2011,2012,2013,2014),
                               NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/qrf_cartaceo"){
#####################################
#  QUANTILE REGRESSION FOREST
#####################################
library(quantregForest)

if(N_VAR_SPLIT=="default")N_VAR_SPLIT=ceiling(ncol(DATI_INPUT)/3)
outcome<-DATI_INPUT[,"outcome"]
pred.matrix<-DATI_INPUT[,2:ncol(DATI_INPUT)]
#Limite delle quantregForest
#Can not handle categorical predictors with more than 32 categories.

qrf <- quantregForest(x=pred.matrix, 
                      y=outcome,
                      ntree=NTREE,
                      mtry=N_VAR_SPLIT) #Stimo i quantili sui dati di training
# NB. La foresta casuale e la foresta "quantile" devono essere costruite sullo stesso set di variabili

#salvo la foresta quantile
qrf_cartaceo<-qrf
save(qrf_cartaceo, file=NOME_FILE_OUTPUT)
return(qrf)
}

PROFILICAMPAGNE<-function(RF=rf,
                          DATI=dati_a_posto,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/Profili_campagne_cartaceo"){
  cmp.profili<-attr(predict(rf, dati_a_posto, nodes=TRUE), "nodes")
  save(cmp.profili, file=NOME_FILE_OUTPUT)
  return(cmp.profili)
}

##################################################
#######    FINE  DEFINIZIONI FUNZIONI      #######
##################################################


###################################################
#######    INIZIO  APPLICAZIONE FUNZIONI    #######
###################################################
dati_a_posto<-PREPARAZIONEDATI(DATI_INPUT=dati,
                               TIPOLOGIA="Cartaceo",
                               VARIABILI_INCLUSE=var_names,
                               VARIABILI_ESCLUSE="cliente_sottosettore",
                               OUTCOME="promo_redemption",
                               ANNI_TRAIN=c(2011,2012,2013,2014))


rf<-RANDOMFOREST(DATI_INPUT=dati_a_posto,
                 NTREE=100,
                 N_VAR_SPLIT= "default",
                 NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_cartaceo_PROVA")



qrf<-QUANTILERANDOMFOREST(DATI_INPUT=dati_a_posto,
                          NTREE=100,
                          N_VAR_SPLIT= "default",
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/qrf_cartaceo_PROVA")




prof.cmp<-PROFILICAMPAGNE(RF=rf,
                          DATI=dati_a_posto,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/Profili_campagne_cartaceo_PROVA")


###################################################
#######    FINE    APPLICAZIONE FUNZIONI    #######
###################################################


