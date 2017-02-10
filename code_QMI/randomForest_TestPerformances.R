#Random Forest esempio
require(randomForest)
#install.packages("zoo")
library(zoo)

load("work/d")
covariate<-c(2:39,41:42)
d<-d[1:1000,]


randomForestPerformances<-function(data,fattoreRighe=1, fattoreColonne=1, ntree=1000){
#RANDOM FOREST REGRESSIONE
#aumento le righe
data<-coredata(data)[rep(seq(nrow(data)),fattoreRighe),]
outcome1<-data[,"promo_redemption"]
#aumento le colonne (covariate)
pred.matrix<-data[,covariate]
pred.matrix<-coredata(pred.matrix)[,rep(seq(ncol(pred.matrix)),fattoreColonne)]
set.seed(10)
t<-system.time(randomForest(pred.matrix, 
    outcome1,
    ntree=ntree, 
    importance=TRUE,
    #proximity=T)
    ))
t<-c(righe=nrow(pred.matrix), colonne=ncol(pred.matrix), t)
return(t)
}
   
randomForestPerformances( data=d,
                          fattoreRighe=1, 
                          fattoreColonne=1, 
                          ntree=1000)

#in prove mettere i fattori moltiplicativi di righe e colonne
#ricorda che il database di partenza ha 1000 righe e 40 colonne
prove<-list(c(1,1),     #      1.000 x 40
            c(10,1),    #     10.000 x 40
            c(100,1),   #    100.000 x 40
            c(1000,1),  #  1.000.000 x 40
            c(10,4),    #     10.000 x 160
            c(10,5),    #     10.000 x 200
            c(10,10),   #     10.000 x 400
            c(100,10),  #    100.000 x 400
            c(1000,5),  #  1.000.000 x 200
            c(10000,1), # 10.000.000 x 40
            c(10000,5), # 10.000.000 x 200
            c(10000,10),# 10.000.000 x 400
            )
results<-vector("list",length(prove))
for(nprova in 1:length(prove)){
  i<-prove[[nprova]][1]
  j<-prove[[nprova]][2]
  results[[nprova]]<-(randomForestPerformances( data=d[1:2,],
                           fattoreRighe=i, 
                           fattoreColonne=j, 
                           ntree=1000))
}

#results contiene i tempi di esecuzione della RF
results
