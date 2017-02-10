#per prove
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")
dati_a_posto_cartaceo<-PREPARAZIONEDATI(DATI_INPUT=dati,
                               TIPOLOGIA="Cartaceo",
                               VARIABILI_INCLUSE=var_names,
                               VARIABILI_ESCLUSE="cliente_sottosettore",
                               OUTCOME="promo_redemption",
                               ANNI_TRAIN=c(2011,2012,2013,2014))
#


cmp.nuova<-dati_a_posto_cartaceo[100,2:ncol(dati_a_posto_cartaceo)]
write.csv2(cmp.nuova, file="Oggetti_per_piattaforma/nuova_campagna_cartaceo_PROVA.csv", row.names=F)


cmp.nuova<-dati_a_posto_printhome[100,2:ncol(dati_a_posto_printhome)]
write.csv2(cmp.nuova, file="Oggetti_per_piattaforma/nuova_campagna_printhome_PROVA.csv", row.names=F)

load("Oggetti_per_Piattaforma/rf_cartaceo_PROVA")
load("Oggetti_per_Piattaforma/qrf_cartaceo_PROVA")
load("Oggetti_per_Piattaforma/Profili_campagne_cartaceo_PROVA")

prev<-previsione(cmp.nuova, rf=rf_cartaceo, qrf=qrf_cartaceo, BreakEven=70)



SOMIGLIANZE<-function(DATI_INPUT=dati_a_posto,
                      cmp.rf=rf_cartaceo,
                      cmp.profili=prof.cmp,
                      cmp.nuova.profilo=prev$profilo){
  cmp.somiglianze<-apply(cmp.profili, 1, function(cmp.profili) mean(cmp.nuova.profilo==cmp.profili))
  plot(cmp.somiglianze, DATI_INPUT[,"outcome"], col="blue", pch=20, xlim=c(0,1), ylim=c(0,100),xlab="Grado di somiglianza con la campagna in esame", ylab="Redemption ", main="Mappa campagne simili")
  abline(v=.5, col="red")
  abline(h=.5, col="red")
  index<-order(cmp.somiglianze, decreasing=TRUE)
  cmp.simili<-DATI_INPUT[index[1:20],]
  cmp.simili.out<-data.frame(cmp.simili, Somiglianza=cmp.somiglianze[index[1:20]])
  cmp.simili.out
}




som<-somiglianze(cmp.rf=rf_cartaceo,
                 cmp.profili=prof.cmp,
                 cmp.nuova.profilo=prev$profilo)
