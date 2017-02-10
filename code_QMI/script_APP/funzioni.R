
###################################################
#######    INIZIO  DEFINIZIONE  FUNZIONI    #######
###################################################
require(randomForest)
require(quantregForest)
require(intervals)

#LA FUNZIONE VA A EVIDENZIARE INCOERENZE e MISSING NEI DATI
CONTROLLODATI<-function(DATI){
  
  vuoto_o_missing<-function(DATI=DATI, VARIABILE){
  id_errore<-DATI$promo_id[which(DATI[,VARIABILE]=="" | is.na(DATI[,VARIABILE]))]
  elenco_id_errore<-paste(id_errore, collapse=",")
  if(length(id_errore)>0) print(paste("Campo", VARIABILE, "mancante o missing per", length(id_errore)," promozioni")) else print(paste("Campo", VARIABILE, "OK"))}
 
  for(j in 1:ncol(DATI))
    vuoto_o_missing(DATI, names(DATI)[j])

  condizione<-length(which(DATI$buoni_qta_emessa<0))
  if(condizione>0) print(paste("Campo buoni_qta_emessa < 0 per", condizione," promozioni")) else print(paste("Campo buoni_qta_emessa OK"))

  condizione<-length(which(DATI$promo_redemption>100))
  if(condizione>0) print(paste("Campo promo_redemption >100 per", condizione," promozioni")) else print(paste("Campo promo_redemption OK"))
  
  condizione<-length(which(DATI$promo_redemption_digitale>100))
  if(condizione>0) print(paste("Campo promo_redemption_digitale >100 per", condizione," promozioni")) else print(paste("Campo promo_redemption_digitale OK"))

}


#va a creare il set di dati pulito per stimare la Random Forest e la Quantile Random Forest
#salva il dataset risultatnte che sarà usato successivamente
PREPARAZIONEDATI<-function(DATI_INPUT=dati,
                           TIPOLOGIA="Cartaceo",
                           VARIABILI_INCLUSE=var_names_cartaceo,
                           VARIABILI_ESCLUSE="cliente_sottosettore",
                           OUTCOME="promo_redemption",
                           ANNI_TRAIN=c(2011,2012,2013,2014),
                           NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/dati_input"){
  
  
  if(!TIPOLOGIA %in% c("Cartaceo", "Print@home")) stop("scrivi come TIPOLOGIA Cartaceo o Print@home")
  
  #SELEZIONO IL SUBSET DI DATI RELATIVO ALLA TIPOLOGIA DI CAMPAGNA
  
  
  if(is.null(VARIABILI_INCLUSE)) VARIABILI_INCLUSE=names(DATI_INPUT)
  if(!is.null(VARIABILI_ESCLUSE)) VARIABILI_INCLUSE<-VARIABILI_INCLUSE[!VARIABILI_INCLUSE%in%VARIABILI_ESCLUSE]
  
#   DATI_INPUT$buoni_film_genere1<-as.character(DATI_INPUT$buoni_film_genere1)
#   DATI_INPUT$buoni_film_genere1[DATI_INPUT$buoni_film_genere1==""]<-"qualsiasi"
# #   DATI_INPUT$buoni_film_target<-factor(DATI_INPUT$buoni_film_target)
#   DATI_INPUT$buoni_film_target<-as.character(DATI_INPUT$buoni_film_target)
#   DATI_INPUT$buoni_film_target[DATI_INPUT$buoni_film_target==""]<-"qualsiasi"
#   
    
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

for (j in tofactor)
  DATI_INPUT[,j]<-factor(DATI_INPUT[,j])
# DATI_INPUT[,tofactor]<-data.frame(apply(DATI_INPUT[,tofactor],2,function(x) factor(as.character(x))))
  rm(tofactor)

  if(TIPOLOGIA=="Print@home") DATI_INPUT$codici_carnet<-as.integer(DATI_INPUT$codici_carnet)

  DATI_INPUT<-DATI_INPUT[DATI_INPUT$promo_tipologia %in% TIPOLOGIA,]    

  no0<-DATI_INPUT$promo_id[DATI_INPUT$buoni_dati_promo_definitivi==0] 
  no1<-DATI_INPUT$promo_id[DATI_INPUT$set_anomalo!=0] 
  WHICH_PROMO_ESCLUSE=c(no0,no1)
  ok<-!DATI_INPUT$promo_id%in%c(WHICH_PROMO_ESCLUSE) & DATI_INPUT$buoni_inizio_validita_aa%in%ANNI_TRAIN
  

  dati1<-DATI_INPUT[ok,]
  pred.matrix<-dati1[,c(VARIABILI_INCLUSE)]
  outcome<-dati1[,OUTCOME]
  dati_a_posto<-cbind(outcome,pred.matrix)
  save(dati_a_posto, file=NOME_FILE_OUTPUT )
  return(dati_a_posto)
}

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


#STIMA LA RANDOM FOREST e la salva su disco
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
                   keep.forest=T,
                   proximity=T)
  print(resultsRF(rf=rf,pred.matrix=pred.matrix,outcome=outcome))
  #importanzaVar(rf)
  #salvo l'oggetto random forest
  save(rf, file=NOME_FILE_OUTPUT)
  return(rf)
}

#STIMA LA RANDOM FOREST PER LA SIMILARITA
RANDOMFOREST_SIMILARITA<-function(DATI_INPUT=dati_a_posto,
                       NTREE=1000,
                       N_VAR_SPLIT= "default",
                       MAXNODES=10,
                       NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_cartaceo_similarita"){
  #DATI PER LA RANDOM FOREST
  outcome<-DATI_INPUT[,"outcome"]
  pred.matrix<-DATI_INPUT[,2:ncol(DATI_INPUT)]
  if(N_VAR_SPLIT=="default")N_VAR_SPLIT=max(floor(ncol(DATI_INPUT)/3))
  rf<-randomForest(pred.matrix,
                   outcome,
                   ntree=NTREE,
                   mtry=N_VAR_SPLIT,
                   importance=T,
                   keep.forest=T,
                   maxnodes=MAXNODES)
  print(resultsRF(rf=rf,pred.matrix=pred.matrix,outcome=outcome))
  #importanzaVar(rf)
  #salvo l'oggetto random forest
  save(rf, file=NOME_FILE_OUTPUT)
  return(rf)
}


#STIMA LA QUANTILE RANDOM FOREST e la salva su disco
QUANTILERANDOMFOREST<-function(DATI_INPUT=dati_a_posto,
                               NTREE=1000,
                               N_VAR_SPLIT= "default",
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
  qrf<-qrf
  save(qrf, file=NOME_FILE_OUTPUT)
  return(qrf)
}

#SELEZIONA DALLA RANDOM FOREST I PROFILI DELLE UNITA' DI TRAIN E LE SALVA SU DISCO
PROFILICAMPAGNE<-function(RF_SIMILARITA=rf_similarita,
                          DATI_INPUT=dati_a_posto,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/Profili_campagne_cartaceo"){
  cmp.profili<-attr(predict(RF_SIMILARITA, DATI_INPUT, nodes=TRUE), "nodes")
  save(cmp.profili, file=NOME_FILE_OUTPUT)
  return(cmp.profili)
}

#PREVISIONE PRENDE UN NUOVO PROFILO DI CAMPAGNA CON TUTTI I VALORI COMPLETI E LO PASSA NELLA RF E QRF RESITUENDO REDEMPTION MEDIA MEDIANA E PROBABILITA' DI SUPERARE UNA SOGLIA DI REDEMPTION (BreakEven)
PREVISIONE<-function(CMP.NUOVA,RF=rf_cartaceo, RF_SIMILARITA=rf_similarita, QRF=qrf_cartaceo, BreakEven=70){
  
  
  if(is.na(CMP.NUOVA$promo_meccanica)){
    CMP.NUOVA$promo_meccanica<-"Altro"
    CMP.NUOVA$promo_meccanica<-factor(CMP.NUOVA$promo_meccanica,levels=rf_cartaceo$forest$xlevels$promo_meccanica)
  }
  
  if(is.na(CMP.NUOVA$promo_distribuzione_biglietti)){
    CMP.NUOVA$promo_distribuzione_biglietti<-"Altro"
    CMP.NUOVA$promo_distribuzione_biglietti<-factor(CMP.NUOVA$promo_distribuzione_biglietti,levels=rf_cartaceo$forest$xlevels$promo_distribuzione_biglietti)
  }

  if(is.na(CMP.NUOVA$buoni_film_target)){
    CMP.NUOVA$buoni_film_target<-"Per tutti"
    CMP.NUOVA$buoni_film_target<-factor(CMP.NUOVA$buoni_film_target,levels=rf_cartaceo$forest$xlevels$buoni_film_target)
  }
  if(is.na(CMP.NUOVA$buoni_film_genere1)){
    CMP.NUOVA$buoni_film_genere1<-"non noto"
    CMP.NUOVA$buoni_film_genere1<-factor(CMP.NUOVA$buoni_film_genere1,levels=rf_cartaceo$forest$xlevels$buoni_film_genere1)
  }

  redemption_media<-round(predict(RF, newdata=CMP.NUOVA),1)
  
  #redemption mediana

  centili<-seq(from = .00, to = .99, by= .01)
  prc<- data.frame(predict(QRF, newdata=CMP.NUOVA, centili))
  
  
  
  ventili<-seq(from = .00, to = .99, by= .05)
  prc.v<- data.frame(predict(QRF, newdata=CMP.NUOVA, ventili))
  differenza_q<-prc.v[1,2:20]-prc.v[1,1:19]
  posizione_moda<-max(which(differenza_q==min(differenza_q)))
  redemption_moda<-round(prc.v[,posizione_moda],1)
  redemption_moda_up<-round(prc.v[,posizione_moda+1],1)
  if(posizione_moda!=1)posizione_moda_down=posizione_moda-1
  if(posizione_moda==1) posizione_moda_down=1
  redemption_moda_down<-round(prc.v[,posizione_moda_down],1)
  
  redemption_mediana<-round(prc[,which(dimnames(prc)[[2]]=="quantile..0.5")],1)
  
  #rischio di superare la soglia
  if(BreakEven<1 & BreakEven>0) BreakEven<-BreakEven*100
  if(BreakEven>prc[ncol(prc)]) {rischio_di_superare_BE=0}
  else {quantile_soglia<-names(prc[which(prc==min(prc[prc>BreakEven]))])
  rischio_di_superare_BE<-round(1-as.numeric(substr(quantile_soglia, 11,14)),1)}
  
  cmp.nuova.profilo<-attr(predict(RF_SIMILARITA, CMP.NUOVA, nodes=TRUE), "nodes")
  
  previsione<-list(redemption_media=redemption_media,
                   redemption_mediana=redemption_mediana,
                   redemption_moda=redemption_moda,
                   redemption_moda_up=redemption_moda_up,
                   redemption_moda_down=redemption_moda_down,
                   rischio_di_superare_BE=rischio_di_superare_BE,
                   profilo=cmp.nuova.profilo)
  return(previsione)
  
}

#SOMIGLIANZE: A PARTIRE DAI DATI DI INPUT, RF E QRF E UN PROFILO CAMPAGNA NUOVO RESTITUISCE LA PORZIONE DI DATI DI INPUT COSTITUITA DALLE 20 CAMPAGNE PIU' SIMILE ALLA NUOVA, LA MISURA DI SIMILARITA' E IL GRAFICO SOMIGLIANZA-REDEMPTION
SOMIGLIANZE<-function(DATI_INPUT=dati_a_posto,
                      CMP.PROFILI=prof.cmp,
                      CMP.NUOVA.PREVISIONE=previsione,
                      N_SIMILI=20){
  CMP.NUOVA.PROFILO<-CMP.NUOVA.PREVISIONE$profilo
#   CMP.NUOVA.REDEMPTION_MEDIA<-CMP.NUOVA.PREVISIONE$redemption_media
  CMP.NUOVA.REDEMPTION_MODA<-CMP.NUOVA.PREVISIONE$redemption_moda
  CMP.NUOVA.REDEMPTION_MODA_UP<-CMP.NUOVA.PREVISIONE$redemption_moda_up
  CMP.NUOVA.REDEMPTION_MODA_DOWN<-CMP.NUOVA.PREVISIONE$redemption_moda_down
  
  cmp.somiglianze<-apply(CMP.PROFILI, 1, function(CMP.PROFILI) mean(CMP.NUOVA.PROFILO==CMP.PROFILI))
  plot(cmp.somiglianze, DATI_INPUT[,"outcome"], col="blue", pch=20, xlim=c(0,1), ylim=c(0,100),xlab="Grado di somiglianza con la campagna in esame", ylab="Redemption ", main="Mappa campagne simili")
  abline(v=0.5, col="red")
  abline(h=50, col="red")
  abline(v=0.75, col="red")
  points(x=1, y=CMP.NUOVA.REDEMPTION_MODA, col="red", pch=20, cex=2.5)
  segments(x0=1,y0=CMP.NUOVA.REDEMPTION_MODA_DOWN,x1=1,y1=CMP.NUOVA.REDEMPTION_MODA_UP, col="red", lwd=2)
  points(x=1, y=CMP.NUOVA.REDEMPTION_MODA_DOWN, col="red", pch="-", cex=2)
  points(x=1, y=CMP.NUOVA.REDEMPTION_MODA_UP, col="red", pch="-", cex=2)
  index<-order(cmp.somiglianze, decreasing=TRUE)
  cmp.simili<-DATI_INPUT[index[1:N_SIMILI],]
  cmp.simili.out<-data.frame(cmp.simili, Somiglianza=cmp.somiglianze[index[1:N_SIMILI]])
  return(cmp.simili.out)
}




##################################################
#######    FINE  DEFINIZIONI FUNZIONI      #######
##################################################