require(randomForest)
require(quantregForest)
require(intervals)

##############################################################################
#funzioni logit e "antilogit" e variance explained
##############################################################################
logit<-function(x) log(x/(100-x))
antilogit<-function(x)100*exp(x)/(1+exp(x))

X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
X.VarRes<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-rss/tss*100
  return(perc)
}
##############################################################################
##############################################################################
load("dati/printhome")

d<-printhome
#escludo le promo cui i dati non sono definitivi
nondef<-which(d$buoni_dati_promo_definitivi==0)
d<-d[-nondef,]
#Costruzione d su cui si fa il training delle foreste
table(d$buoni_inizio_validita_aa)
#escludo i campi (per ora) non utilizzabili
names(d)
d<-subset(d, select=-c( promo_id,
                        cliente_idcrm,
                        buoni_inizio_validita,
                        buoni_fine_validita,
                        buoni_dati_promo_definitivi
                        buoni_qta_redenta))

#SELEZIONE RECORD PER RF
#escludo le promo dell'anno 2015 e prendo come campione previsivo X anni simili
table(d$buoni_inizio_validita_aa)
selezione<-which(d$buoni_inizio_validita_aa %in% c(2009,2010,2011,2012,2013,2014))
length(selezione)  
d<-d[selezione,]

#creazione della variabile settore2
data.frame(table(d$cliente_settore))
d[,"settore2"]<-as.character(d$cliente_settore)
d$settore2[which(d$cliente_settore %in% c( "APPAREL",
                                           #"AUTOMOTIVE",
                                           "CONSUMER ELECTRONICS",
                                           "ENERGY&PETROL",
                                           "FINANCIAL&INSURANCE",
                                           "MEDIA&PUBLISHING",
                                           #"PUBLIC ADMIN",
                                           "RETAIL",
                                           #"TELCO",
                                           "TRAVEL"))]<-"ALTRO"
d$settore2<-factor(d$settore2)
d$cliente_settore<-factor(d$cliente_settore)
table(d$settore2)
#SELEZIONE VARIABILI PER RF
#escludo dalle variabili della RF l'outcome promo_redemption e promo_id
names(d)
variabili<-names(d)[!names(d)%in%c("promo_redemption",
                                   "promo_id",
                                   "buoni_inizio_validita",
                                   "buoni_fine_validita",
                                   "buoni_qta_redenta",
                                   "codici_inizio",
                                   "codici_fine",
                                   "cliente_idcrm",
                                   "buoni_dati_promo_definitivi")]


################# 
d.checkpoint<-d
#################
#da qui in avanti non modifico più d
#################
#Splitto il campione (i record di d) in campione di training (train) e di test
set.seed(10)
quota.train=9/10
train1<-sample(1:nrow(d), floor(nrow(d)*quota.train))
test1<-!(1:nrow(d) %in% train1)
#################################################################
#   RANDOM FOREST
#################################################################
outcome1<-d[,"promo_redemption"] #outcome1
pred.matrix<-d[,variabili]
set.seed(10)
rf1<-randomForest(pred.matrix[train1,],
                  outcome1[train1],
                  ntree=1000,
                  importance=T)
rf1
#ntree= 500 % Var explained: 64.52
#ntree=1000 % Var explained: 65.22
#ntree=2000 % Var explained: 65.18
#ntree=5000 % Var explained: 64.99

X.VarExp(outcome1[train1], rf1$predicted)
pred.test<-predict(rf1, pred.matrix[test1,])
X.VarExp(outcome1[test1],pred.test)
plot(outcome1[test1], pred.test)
abline(0,1)

#variable importance
ord.var<-order(rf1$importance[,1], decreasing=TRUE)
var.rf<-data.frame(rf1$importance[ord.var,1])
plot(var.rf[,1], type="b", pch=19, col="blue", ylab="%incMSE")
text(var.rf[,1],label=row.names(var.rf),cex=0.7)

Results<-matrix(,nrow=length(variabili), ncol=3)
colnames(Results)<-c("Nvar","%train","%test")
Results
for (i in nrow(Results):2){
var.sel<-which(rf1$importance[,1]>=var.rf[i,1])
pred.matrix.min<-pred.matrix[,var.sel]
set.seed(10)
rf.min<-randomForest( pred.matrix.min[train1,],
                      outcome1[train1],
                      ntree=1000)
Results[i,1]<-length(var.sel)
Results[i,2]<-X.VarExp(outcome1[train1], rf.min$predicted)
Results[i,3]<-X.VarExp(outcome1[test1], predict(rf.min, pred.matrix.min[test1,]))
}


#osservando il grafico risulta che la varianza spiegata usando una RF con poche variabili equivale a varianza spiegata usando tutte le variabili
plot(Results[,1],Results[,2], pch=NA,
      xlab="numero di variabili",
      ylab="%varianza spiegata",
      ylim=c(30,100))
lines(Results[,1],Results[,2], col="red")
lines(Results[,1],Results[,3], col="blue")
abline(h=Results[nrow(Results),3], lty="dotted", col="green")
abline(v=c(5,10,20,30,40,50))
legend("topright", legend=c("Training Set","Test Set"),
       fill=c("red","blue"), bty="n", cex=0.7, ncol=2)




