# Input: Feature della campagna, più valore critico (tipo break-even), foresta casuale, foresta casuale quantile
# Output: Previsione redemption tramite media, Previsione redemption tramite mediana, probabilità di superamento valore critico, profilo (nel senso dei nodi) della campagna sotto esame.
# Descrizione: La funzione applica le foreste (normale e quantile) al profilo in input e generae gli output previsti.


#per prove
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")
dati_a_posto<-PREPARAZIONEDATI(DATI_INPUT=dati,
                               TIPOLOGIA="Cartaceo",
                               VARIABILI_INCLUSE=var_names,
                               VARIABILI_ESCLUSE="cliente_sottosettore",
                               OUTCOME="promo_redemption",
                               ANNI_TRAIN=c(2011,2012,2013,2014))
#


cmp.nuova<-dati_a_posto[100,]



load("Oggetti_per_Piattaforma/rf_cartaceo_PROVA")
load("Oggetti_per_Piattaforma/qrf_cartaceo_PROVA")


previsione<-function(cmp.nuova, rf=rf_cartaceo, qrf=qrf_cartaceo, BreakEven=70){
  
  #redemption media
  Redemption_media<-predict(rf, newdata=cmp.nuova)
  
  #redemption mediana
  NEWDATA=cmp.nuova[,2:ncol(cmp.nuova)]
  centili<-seq(from = .00, to = .99, by= .01)
  prc<- data.frame(predict(qrf, newdata=NEWDATA, centili))
  Redemption_mediana<-prc[,which(dimnames(prc)[[2]]=="quantile..0.5")]
  
  #rischio di superare la soglia
  if(BreakEven<1 & BreakEven>0) BreakEven<-BreakEven*100
  quantile_soglia<-names(prc[which(prc==min(prc[prc>BreakEven]))])
  rischio_di_superare_BE<-1-as.numeric(substr(quantile_soglia, 11,14))
  
  cmp.nuova.profilo<-attr(predict(rf, cmp.nuova, nodes=TRUE), "nodes")
  
  previsione<-list(Redemption_media=Redemption_media,
                   Redemption_mediana=Redemption_mediana,
                   profilo=cmp.nuova.profilo)
  return(previsione)
  
}


previsione(cmp.nuova, rf=rf_cartaceo, qrf=qrf_cartaceo, BreakEven=70)



