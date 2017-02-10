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
X.VarExp2<-function(df){
  y<-df[,1]
  yhat<-df[,2] 
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
X.VarRes2<-function(df){
  y<-df[,1]
  yhat<-df[,2] 
  n<-length(y)
  perc<-(sum((y-yhat)^2)/ (var(y)*(n-1)))*100
  return(perc)
}


##############################################################################
##############################################################################



load("dati/cartaceo")

d<-cartaceo

apply(d,2,function(x)sum(is.na(x)))

#Back end - fase preliminare
#Costruzione d su cui si fa il training delle foreste

#escludo le promo cui i dati non sono definitivi
nondef<-which(d$buoni_dati_promo_definitivi==0)
length(nondef)
table(d$buoni_inizio_validita_aa[nondef])
d<-d[-nondef,]

#escludo i campi (per ora) non utilizzabili
names(d)
d<-subset(d, select=-c(cliente_idcrm,
                       promo_tipologia,
                       buoni_dati_promo_definitivi,
                       buoni_inizio_validita,
                       buoni_fine_validita))

#SELEZIONE RECORD PER RF
#escludo le promo dell'anno 2015 e prendo come campione previsivo X anni simili
selezione<-which(d$buoni_inizio_validita_aa %in% c(2009,2010,2011,2012,2013,2014))
length(selezione)  
d<-d[selezione,]

buoni_inizio_validita_aa<-d$buoni_inizio_validita_aa
d<-subset(d, select=-c(buoni_inizio_validita_aa))
d<-cbind(d,buoni_inizio_validita_aa)
#SELEZIONE VARIABILI PER RF
#escludo dalle variabili della RF l'outcome promo_redemption e promo_id
names(d)
variabili<-2:(length(names(d))-2)
names(d)[variabili]

d[,"settore2"]<-as.character(d$cliente_settore)
d$settore2[which(d$cliente_settore %in% c( "MEDIA&PUBLISHING",
                                           "CONSUMER ELECTRONICS",
                                           "PUBLIC ADMIN",
                                           "TELCO",
                                           "APPAREL",
                                           "AUTOMOTIVE",
                                           "ENERGY&PETROL"))]<-"ALTRO"
d$settore2<-factor(d$settore2)



################# 
d.checkpoint<-d
#################

nozeri<-which(!d$promo_redemption%in%c(0,100))
zeri<-which(d$promo_redemption%in%c(0,100))

length(nozeri)
length(zeri)
d.zeri<-d[zeri,]
d<-d[nozeri,]
#################
#da qui in avanti non modifico più d
#################
#chi sono gli 0 e 100
table(d.zeri$promo_redemption, d.zeri$buoni_inizio_validita_aa )
table( d.zeri$cliente_settore,d.zeri$promo_redemption )

#Splitto il campione (i record di d) in campione di training (train) e di test

set.seed(10)
quota.train=9/10
train1<-sample(1:nrow(d), floor(nrow(d)*quota.train))
test1<-!(1:nrow(d) %in% train1)
#################################################################
#   Random Forest zeri
#################################################################
outcome0<-d.zeri[,"promo_redemption"]
pred.matrix0<-d.zeri[,variabili]
rf0<-randomForest(pred.matrix0, outcome0, ntree=1000, importance=TRUE)
rf0

#################################################################
#   RANDOM FOREST NO zeri
#################################################################
outcome1<-d[,"promo_redemption"] #outcome1
outcome2<-logit(d[,"promo_redemption"]) #outcome2
pred.matrix<-d[,variabili]
set.seed(10)
rf1<-randomForest(pred.matrix[train1,], outcome1[train1], ntree=1000, importance=TRUE)
rf1
rf2<-randomForest(pred.matrix[train1,], outcome2[train1], ntree=1000, importance=TRUE)
rf2

#################################################################
#   RANDOM FOREST ALL non zeri e zeri
#################################################################
#la logit non da ottimi risulatati quindi useremo il NON logit
#Ri-includo i 0 100 per vedere se le cose migliorano
set.seed(10)
quota.train=9/10
train0<-sample(1:nrow(d.checkpoint), floor(nrow(d.checkpoint)*quota.train))
test0<-!(1:nrow(d.checkpoint) %in% train0)
outcome1<-d.checkpoint[,"promo_redemption"] #outcome1
pred.matrix<-d.checkpoint[,variabili]
set.seed(10)
rf00<-randomForest(pred.matrix[train0,], outcome1[train0], ntree=1000, importance=TRUE)
rf00

#################################################################
#   Random Forest settore2 NO zeri
#################################################################
#introduco la variabile settore2 raggruppando settori piccoli
data.frame(table(d$cliente_settore))
data.frame(table(d$settore2))
outcome1<-d[,"promo_redemption"] #outcome1
variabili3<-c(variabili,42)
pred.matrix3<-d[,variabili3]
rf3<-randomForest(pred.matrix3[train1,], outcome1[train1], ntree=1000, importance=TRUE)
rf3

######################################
#   Random Forest rf11 NO cliente_sottosettore
######################################
outcome1<-d[,"promo_redemption"] #outcome1
names(d)
covariate<-variabili[variabili!=5]
pred.matrix1<-d[,covariate]
set.seed(10)
rf11<-randomForest(pred.matrix1[train1,], outcome1[train1], ntree=1000, importance=TRUE, proximity=T)
rf11

######################################
#confronto tra le rf stimate
######################################

#poiché l'outcome non è "logit" per rendere confrontabili le stime della % Var explained tra logit e non logit bisogna calcolare a mano (approssimando perché non considero i oob)

y1<-d[train1,"promo_redemption"]

#rf1
X.VarExp(y=y1, yhat=rf1$predicted) #variance explained
#rf2
X.VarExp(y=logit(y1), yhat=rf2$predicted) #output di rf2
X.VarExp(y=y1, yhat=antilogit(rf2$predicted) #variance explained not logit
#rf3
X.VarExp(y=y1, yhat=rf3$predicted)
#rf0 #non utile
#rf00
y00<-d.checkpoint[,"promo_redemption"]
X.VarExp(y=y00, yhat=rf00$predicted)
#rf11
X.VarExp(y=y1, yhat=rf11$predicted)

         

##########################################
#selezione ulteriore del campione escludendo valori estremi
##########################################
d1<-d.checkpoint

br<-c(5,10,15,20,25,30,35,40)
lista.sel<-vector("list", length=length(br)+1)
lista.sel[[1]]<-which(!d1$promo_redemption%in%c(0,100))
names(lista.sel)[1]<-("0|100")
for (i in 2:length(lista.sel))
{
  lista.sel[[i]]<- which(d1$promo_redemption>br[i-1] & d1$promo_redemption<(100-br[i-1]))
  names(lista.sel)[i]<-paste0(br[i-1],"-",100-br[i-1])
}
RFsel<-data.frame(intervalli=names(lista.sel),X.VarExp1=0,X.VarExp2=0, N=0)
RFsel

for(i in 1:length(lista.sel)){
  d2<-d1[lista.sel[[i]],]
  set.seed(10)
  quota.train=9/10
  train<-sample(1:nrow(d2), floor(nrow(d2)*quota.train))
  test<-!(1:nrow(d2) %in% train)
  
  outcome1<-d2[,"promo_redemption"] #outcome1
  outcome2<-logit(d2[,"promo_redemption"]) #outcome2
  pred.matrix<-d2[,variabili]
  set.seed(10)
  rf1<-randomForest(pred.matrix[train,], outcome1[train], ntree=1000, importance=TRUE)
  rf2<-randomForest(pred.matrix[train,], outcome2[train], ntree=1000, importance=TRUE)
  
  y<-d2[train,"promo_redemption"]
  pre1<-rf1$predicted
  pre2<-antilogit(rf2$predicted)
  
  RFsel[i,2]<-X.VarExp(y=y, yhat=pre1) #variance explained
  RFsel[i,3]<-X.VarExp(y=y, yhat=pre2) #variance explained not logit
  RFsel[i,4]<-nrow(d2[train,])
}

RFsel
##########################################
##########################################



#Estraggo le variabili rilevanti per la previsione
plot.var<-function(rf){
  ord.var<-order(rf$importance[,1], decreasing=TRUE)
  var.rf<-data.frame(rf$importance[ord.var,])
  return(var.rf)
  plot(var.rf[,1], type="b", pch=19, col="blue", ylab="%incMSE")
  text(var.rf[,1],label=row.names(var.rf),cex=0.7)
}

plot.var(rf1)
plot.var(rf3)



#Confronto le previsioni della foresta con i dati di test
actualVSfitted<-function(test, rf, col=col){
  variabili<-unlist(dimnames(rf$importance)[1])
  d[test,variabili]
  predicted.test<-predict(rf, d[test,variabili])
  actual.test<-d[test,"promo_redemption"]
  plot(actual.test, predicted.test, xlim=c(0,100), ylim=c(0,100), pch=20, col=col)
  abline(0, 1, col="black")
}

actualVSfitted(test1, rf1, col="green")
actualVSfitted(test1, rf3, col="blue")


#####################################
#applicato rf1 e rf2 a tutto i settori separatamente.
#scopo valutare se viene classificato bene lo stesso
#####################################

tabella<-data.frame(
  id=d$promo_id[train1], 
  settore=d$cliente_settore[train1], 
  settore2=d$settore2[train1], 
  actual=d$promo_redemption[train1], 
  pred=rf11$predicted
)

rf1
X.VarExp(y=tabella$actual,yhat=tabella$pred)
X.VarRes(y=tabella$actual,yhat=tabella$pred)
X.VarExp2(tabella[,c("actual","pred")])
X.VarRes2(tabella[,c("actual","pred")])


aggrega<-function(tabella, varAggr){
  settori<-unique(tabella[,varAggr])
  a<-data.frame(settore=unique(tabella[,varAggr]), 
                Xvar=0,
                M.actual=0,
                M.pred=0,
                n=0)
  for(i in 1:length(settori)){
    casi<-which(tabella[,varAggr]==settori[i])
    a[i,2]<-X.VarExp(y=tabella$actual[casi],yhat=tabella$pred[casi])
    a[i,3]<-mean(tabella$actual[casi])
    a[i,4]<-mean(tabella$pred[casi])
    a[i,5]<-length(casi)
  }
  return(a)
}

#per slides
png("images/Performance RF per settore.png", width=1280, height=700)
par(par.negative)
xm<-X.VarExp(y=tabella$actual,yhat=tabella$pred)
tabella$settore2[which(tabella$settore2=="TRAVEL")]<-"ALTRO"
a<-aggrega(tabella, "settore2")
a[,1]<-factor(a[,1])
levels(a[,1])<-c("Agency","Altro","Entert","Financ&Insur","FMCG","Retail","Services","Travel")
a[a[,2]<0,2]<-NA
bp<-barplot(a[,2], border="white", col="white",names.arg=NULL, ylim=c(0,75), las=2, cex.names=0.8)
text(bp, 0, round(a[,2], 1),cex=2,pos=3, col="black")
abline(h=xm, lty="dotted", lwd=3)
colori<-rep("black",nrow(bp))
text(bp, 15, a[,1], cex=2,srt=90, col=colori)
box()
dev.off()

plot(actual~pred, data=tabella, col=(2+(settore2!="FMCG")), pch=16)
abline(0,1)

plot(actual~pred, data=tabella, col=(2+(settore2!="TRAVEL")), pch=16)
abline(0,1)
#####################################
#####################################

#####################################
#RANDOM FOREST X SETTORE
#####################################
#faccio le random forest classificate per settore

d.train<-d[train1,]
#cartaceo$promo_id
mylist.names <- unique(d.train$cliente_settore)
mylist <- vector("list", length(mylist.names))
names(mylist) <- mylist.names
rf.mylist<-mylist
for(i in 1:length(mylist)){
  mylist[[i]]<-d.train[which(d.train$cliente_settore==mylist.names[i]),]
}

rf.mylist
for(i in 1:length(rf.mylist)){  
  rf.mylist[[i]]<-randomForest(mylist[[i]][,variabili], 
                               logit(mylist[[i]][,"promo_redemption"]),
                               ntree=1000, 
                               importance=TRUE)
}



settRF1<-data.frame(promo_settore=unique(d.train$cliente_settore), X.Varexp=0, N=0)
settRF1
for(i in 1:length(rf.mylist)){
  pred.settore<-rf.mylist[[i]]$predicted
  pred.settore<-antilogit(pred.settore)
  
  yhat<-pred.settore
  y<-mylist[[i]]$promo_redemption
  settRF1[i,2]<-X.VarExp(y=y, yhat=yhat)
  settRF1[i,3]<-nrow(mylist[[i]])
}
settRF1
#####################################
#####################################

################################################
# TEST per valutare bontà della rf
################################################

predicted.test<-predict(rf00, d.checkpoint[test0,variabili])
actual<-d.checkpoint[test0,"promo_redemption"]
X.VarExp(y=actual, yhat=predicted.test)

predicted.test<-predict(rf1, d[test1,variabili])
actual<-d[test1,"promo_redemption"]
X.VarExp(y=actual, yhat=predicted.test)


predicted.test<-predict(rf1, d.checkpoint[zeri,variabili])
actual<-d.checkpoint[zeri,"promo_redemption"]
X.VarExp(y=actual, yhat=predicted.test)
         
predicted.test<-predict(rf11, d[test1,covariate])
actual<-d[test1,"promo_redemption"]
X.VarExp(y=actual, yhat=predicted.test)
         
################################################
# Medie condizionate su predicted
################################################

#Calcolo le medie condizionate a valori della redemption prevista

br<-seq(0,100,by=5)
MeanCond<-data.frame(intervalli=as.character(rep("",length(br)-1)),media_pred=0, Nhat=0,media_actual=0, Nact=0)
MeanCond
for(i in 1:(length(br)-1)){
  pred.int<-which(tabella$pred>br[i] & tabella$pred<=(br[i+1]))
  act.int<-which(tabella$actual>br[i] & tabella$actual<=(br[i+1]))
  
  MeanCond[i,2]<-mean(tabella[pred.int,"pred"])
  MeanCond[i,3]<-length(pred.int)
  MeanCond[i,4]<-mean(tabella[act.int,"actual"])
  MeanCond[i,5]<-length(act.int)
  MeanCond[i,1]<-paste0(br[i],"-",br[i+1])
}
MeanCond
plot(MeanCond$media_pred, MeanCond$media_actual, xlim=c(0,100), ylim=c(0,100), pch=16, col="red", cex=1.7)
points(tabella$pred, tabella$actual, cex=0.3, col="black")
abline(0, 1, col="green")
for(i in br)abline(v=i)

hist(MeanCond$Nhat,breaks=10)

#distribuzioni entro intervallo
pred.int<-which(tabella$pred>br[1] & tabella$pred<=(br[3]))
hist(tabella$pred[pred.int], breaks=10, main="0-10")

pred.int<-which(tabella$pred>br[15] & tabella$pred<=(br[17]))
hist(tabella$pred[pred.int], breaks=10, main="70-80")


################################################
################################################

################################################
# ANALISI DEI RESIDUI su rf11
################################################
names(tabella)
plot(tabella$pred,tabella$actual, col="red", pch=16, xlim=c(0,100), ylim=c(0,100), cex=0.7)
abline(0,1)

residui<-tabella$actual-tabella$pred

summary(residui)
hist(residui, density=1)

################################################
################################################

################################################
#   OUTLIERS
################################################
rf11<-randomForest(pred.matrix[train1,], outcome1[train1], ntree=1000, importance=TRUE, proximity=T)

names(rf1)
str(rf1$proximity)

# Classical MDS

dist.matrix <- rf11$proximity
fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim
library(MASS)
fit <- isoMDS(dist.matrix, k=2)
fit # view results
#eigenvalue
autovalori<-sort(fit$eig/sum(fit$eig), decreasing=T)
autovalori
plot(autovalori[1:100])


# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(jitter(x), jitter(y), xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS",  pch=1, col=tabella$settore2, cex=1)



################################################
################################################
#salvo le rf finora fatte
#dir.create("rf")
save(rf1, file="rf/rf1")
save(rf2, file="rf/rf2")
save(rf3, file="rf/rf3")
save(rf0, file="rf/rf0")
save(rf00, file="rf/rf00")
save(rf11, file="rf/rf11")
####---####---####---####---####---####---###
####---####---####---####---####---####---###
#PRIMA DI PROCEDERE ALLE STIME DELLA FORESTA QUANTILE SALVO TUTTI GLI OGGETTI UTILI DELLA SESSIONE
save(d, file="work/d")
save(d, file="work/d.zeri")
tt<-list(train1=train1,
         test1=test1,
         train0=train0,
         test0=test0, 
         zeri=zeri,
         nozeri=nozeri,
         variabili=variabili,
         covariate=covariate)
save(tt, file="work/tt")
save(tabella, file="work/tabella")
         
###########################################################         
load("rf/rf1")
load("rf/rf11")
load("work/d")
load("work/tt")
load("work/tabella")
         
train1<-tt$train1
test1<-tt$test1
variabili<-tt$variabili
covariate<-tt$covariate
         
#####################################
#  QUANTILE REGRESSION FOREST
#####################################
# BAck end regressione quantile
# Per tenere separate le cose, riassegno d al dataframe db.qrf
# (sto replicando troppe volte il db, si pu? rendere pi? efficiente)
# e calcolo la foresta quantile

#cliente_sottosettore ha troppi livelli e quindi la escludo
covariate<-variabili[variabili!=5]

db.qrf<-d[train1,covariate]
db.test<-d[test1,covariate]
outcome.qrf<-d$promo_redemption[train1]
outcome.test<-d$promo_redemption[test1]
#Limite delle quantregForest
#Can not handle categorical predictors with more than 32 categories.

qrf <- quantregForest(x=db.qrf, y=outcome.qrf) #Stimo i quantili sui dati di training
# NB. La foresta casuale e la foresta "quantile" devono essere costruite sullo stesso set di variabili

#salvo la foresta quantile
save(qrf,file="rf/qrf")
         
load("rf/qrf")

#stimo tutte le quantità di interesse per le analisi sia per il train che per il test

crea_tutte_le_quantita<-function(data, covariate,qrf,rf11,R=1, dati_costruzione_foresta=F){
      #stimo i centili per tutti i dati di test
      centili<-seq(from = .00, to = .99, by= .01)
      prc<- data.frame(predict(qrf, newdata= data[,covariate], centili))
      if(dati_costruzione_foresta==T) media.rf<-rf11$predicted
      else media.rf<-predict(rf11, newdata=data[,covariate])
      mediana<-prc[,which(dimnames(prc)[[2]]=="quantile..0.5")]
      q1<-prc[,which(dimnames(prc)[[2]]=="quantile..0.25")]
      q3<-prc[,which(dimnames(prc)[[2]]=="quantile..0.75")]
      iqr<-q3-q1
      lci95<-c(predict(qrf, newdata=data[,covariate],0.025))
      uci95<-c(predict(qrf, newdata=data[,covariate],0.975))
      risk10<-prc[,which(dimnames(prc)[[2]]=="quantile..0.9")]
      risk20<-prc[,which(dimnames(prc)[[2]]=="quantile..0.8")]
      
      #ricostruisco dalla funzione di ripartizione la densità e su questa calcolo media e varianza
      hit<-seq(R/2,100-R/2, by=R)
      #funzione di densità ("matrice di densità")
      prc.d<-data.frame(t(
        apply(prc,1,function(x,r){  
        dentro<-function(a,eps=r/2)
                (length(which(x>=a-eps & x<a+eps)))  
        int<-data.frame(seq(r/2,100-r/2, by=r))
        dx<- apply(int,1,dentro)
        return(dx)},r=R)))
      names(prc.d)<-hit
      #misura di asimmetria della curva
      #skewness
      skewness<-function(x, m=NULL, s=NULL){
        if(is.null(m))  m<-mean(x)
        if(is.null(s))  s<-sd(x)
        m3<-mean((x-m)^3)
        sk<-m3/(s^3)
        return(sk)
}
#valori positivi di skewness= asimmetria negativa
#valori positivi di skewness= asimmetria positiva
media.qrf<-apply(prc.d,1,function(x) mean(rep(hit, times=x)))
stdev<-apply(prc.d,1,function(x) sd(rep(hit, times=x)))
skew<-apply(prc.d,1,function(x) skewness(rep(hit, times=x)))

promo<-cbind(data, prc, prc.d, media.rf,media.qrf,stdev,mediana, q1,q3,iqr,lci95,uci95,skew,risk10,risk20) 
return(promo)
}

data1.2<-d[which(tt$test1),]
data1.2$train<-0

data1.1<-d[tt$train1,]
data1.1$train<-1

data2<-d.checkpoint[zeri,]
data2$train<-NA

promo1.1<-crea_tutte_le_quantita(data=data1.1,
                               covariate=tt$covariate,
                               qrf=qrf,
                               rf11=rf11,
                               R=1,
                               dati_costruzione_foresta=T)
promo1.2<-crea_tutte_le_quantita(data=data1.2,
                                 covariate=tt$covariate,
                                 qrf=qrf,
                                 rf11=rf11,
                                 R=1)
promo2<-crea_tutte_le_quantita(data=data2,
                               covariate=tt$covariate,
                               qrf=qrf,
                               rf11=rf11,
                               R=1)

promo<-rbind(promo1.1, promo1.2,promo2)

save(promo, file="dati/promo")
         
load("dati/promo")
#controllo sulle media e varianza
plot(media.rf, media.qrf)
abline(0,1,col="red")

plot(stdev,media.rf-media.qrf)
         
plot(stdev,d$promo_redemption-media.qrf)


#controllo di coerenza tra rf11 e qrf
media.diff<-promo$media.rf-media.qrf
summary(media.diff)
hist(media.diff, breaks=20, prob=T)
curve(dnorm(x,mean=mean(media.diff), sd=sd(media.diff)), add=T)


#TEST di correttezza sul file promo
y<-promo[which(promo$train==1),"promo_redemption"]
yhat<-promo[which(promo$train==1),"media.rf"]
X.VarExp(y=y,yhat=yhat)
y<-promo[which(promo$train==0),"promo_redemption"]
yhat<-promo[which(promo$train==0),"media.rf"]
X.VarExp(y=y,yhat=yhat)
y<-promo[which(is.na(promo$train)),"promo_redemption"]
yhat<-promo[which(is.na(promo$train)),"media.rf"]
X.VarExp(y=y,yhat=yhat)


########################
#ulteriori prove sulla rf11 dopo aver letto i papers
######################################
#   Random Forest rf11 NO cliente_sottosettore
######################################
outcome1<-d[,"promo_redemption"] #outcome1
names(d)
covariate<-variabili[variabili!=5]
pred.matrix1<-d[,covariate]
set.seed(10)
rf12<-randomForest(pred.matrix1[train1,],
                   outcome1[train1],
                   ntree=500,
                   nodesize=10,
                   #mtry=p/3
                   importance=F,
                   proximity=F)
rf12


#NOTA PER IL FUTURO 
#Questa combinazione garantisce max varianza spiegata
# 46.51% primo giro 46.41% secondo giro, contro un 45.98 di rf11
#qui le specifiche
# rf12<-randomForest(pred.matrix1[train1,],
#                    outcome1[train1],
#                    ntree=500,
#                    nodesize=10,
#                    #mtry=p/3
#                    importance=F,
#                    proximity=F)