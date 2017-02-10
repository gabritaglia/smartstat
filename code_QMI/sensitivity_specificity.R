outcome2<-0
outcome2[which(outcome1>70)]<-0
outcome2[which(outcome1<=70)]<-1
outcome2<-factor(outcome2)
summary(outcome2)


prevalenza<-sum(outcome2==0)/sum(outcome2==1)
prevalenza
rf<-randomForest(pred.matrix,
                 outcome2,
                  ntree=500, 
                 cutoff=c(prevalenza, 1-prevalenza))
rf


rf<-randomForest(pred.matrix,
                 outcome2,
                 ntree=500, 
                 cutoff=c(0.5,0.5))
rf
# prevalenza: della malattia il numero dei soggetti malati presenti nella popolazione: una prevalenza dello 0.5% significa che 5 persone su mille sono affette dalla malattia, ecc.
# 
# specificità: la capacità di un test di individuare i soggetti che presentano la malattia. La sensibilità è importante quando è necessario massimizzare i veri positivi, come nel caso di malattie gravi, a decorso rapido, in cui un intervento tempestivo può essere cruciale. Se un test molto specifico risulta positivo, si può ragionevolmente ritenere che la malattia è presente e si può generalmente procedere con i trattamenti previsti. Viceversa, se un test è poco specifico, si rischia di ottenere un falso positivo (viene segnalata una patologia inesistente).
# 
# sensibilità: la capacità di un test di individuare i soggetti che non presentano la malattia. La specificità è importante quando è necessario essere sicuri della diagnosi fatta, come nel caso di una diagnosi alla quale segue un intervento di chirurgia demolitiva. Se un test molto sensibile risulta negativo, si può ragionevolmente ritenere che la malattia non c'è e non occorre generalmente procedere con ulteriori esami. Viceversa, se un test è poco sensibile, si rischa di ottenere un falso negativo (la patologia c'è ma non viene individuata). 