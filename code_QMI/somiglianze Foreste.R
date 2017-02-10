library(randomForest)


data(iris)

# cmp.rf: rabdom foret precedentemente costruita
# cmp.profili: profili delle campagne su cui è stata costruita la foresta
# (per profilo, qui intendo l'elenco dei nodi degli alberi della foresta)
# cmp.nuova: nuova campagna a esaminare (vettore di feature)
# cmp.nuova.profilo: proflo della campagna sotto esame
# Queste entità provengono da script precedenti

# La funzione Somiglianze prende in input la foresta, i profili e il profilo della campagna nuova
# calcola le somiglianze con le campagne vecchie, produce un plot e l'elenco delle 20 campagne
# vecchie pià simili

# ----NB---- 
# Ho fatto finta che la colonna 2 di Iris contenesse le redemption. Ovviamente,
# questo sballa il grafico che prevede una scala 0-100 di redeption
# La funzione mean(cmp.nuova.profilo==cmp.profili) calcola la frazione
# di nodi in comune fra i profilo della campagna nuova e
# (tramite la funzione apply), i singoli profili delle campagne vecchie
# cmp.somiglianze è quindi un vettore la cui componente h-esima
# contiene la frazione di nodi in comune fra il profilo della campgane nuova 
# e il profilo della h-esima campagna vecchia.

Somiglianze<-function(cmp.rf, cmp.profili, cmp.nuova.profilo)
{
  cmp.somiglianze<-apply(cmp.profili, 1, function(cmp.profili) mean(cmp.nuova.profilo==cmp.profili))
  plot(cmp.somiglianze, iris[,2], col="blue", pch=20, xlim=c(0,1), ylim=c(0,100),xlab="Grado di somiglianza con la campagna in esame", ylab="Redemption ", main="Mappa campagne simili")
  abline(v=.5, col="red")
  abline(h=.5, col="red")
  index<-order(cmp.somiglianze, decreasing=TRUE)
  cmp.simili<-iris[index[1:20],]
  cmp.simili.out<-data.frame(cmp.simili, Somiglianza=cmp.somiglianze[index[1:20]])
  cmp.simili.out
}


cmp.rf <- randomForest(Species ~ ., data=iris)
cmp.profili<-attr(predict(cmp.rf, iris, nodes=TRUE), "nodes")
cmp.nuova<-iris[100,]
cmp.nuova.profilo<-attr(predict(cmp.rf, cmp.nuova, nodes=TRUE), "nodes")
Somiglianze(cmp.rf, cmp.profili, cmp.nuova.profilo)


cmp.rf <- rf
cmp.profili<-attr(predict(rf, dati, nodes=TRUE), "nodes")
cmp.nuova<-iris[100,]
cmp.nuova.profilo<-attr(predict(cmp.rf, cmp.nuova, nodes=TRUE), "nodes")
Somiglianze(cmp.rf, cmp.profili, cmp.nuova.profilo)








