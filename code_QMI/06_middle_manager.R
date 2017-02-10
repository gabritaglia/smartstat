par.default <- par(no.readonly=TRUE)
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="black")
load("dati/promo")
load("work/tt")
load("rf/rf11")


promo$skew<--promo$skew #ho bisogno che la skewness segua il rishio, ovvero alta se distribuzione rishiosa
sup<-quantile(promo$buoni_qta_emessa,0.90)*3
scelta<-which(promo$buoni_qta_emessa>sup)
promo1<-promo[-scelta,]


camp<-which(promo$promo_id==790)

promo$promo_id[camp]
##############################################
#   distribuzione della redemption
##############################################
x<-seq(0.5,99.5, by=1)
dens<-as.numeric(promo1[camp,144:243])
quant<-as.numeric(promo1[camp,44:143])
X<-rep(x,times=dens)
hist(X, breaks=20, prob=T, xlab="Redemption", ylab="Densità", main=paste0("Distribuzione della redemption promo_id ",promo$promo_id[camp]))
abline(v=promo1$q1[camp], col="red")
abline(v=promo1$q3[camp], col="red")
abline(v=promo1$mediana[camp], col="blue", lwd=2)
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults
lines(density(X, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

#per slides
png("images/distribuzione_esempio.png", width=800, height=600)
par(par.negative)
x<-seq(0.5,99.5, by=1)
dens<-as.numeric(promo1[camp,144:243])
X<-rep(x,times=dens)
hist(X, breaks=10, prob=T, xlab="Redemption", ylab="Probabilità", border="black", xlim=c(0,100), main="")
abline(v=promo1$mediana[camp], col="white", lwd=2)
lines(density(X), col="white", lwd=3) # add a density estimate with defaults
text(promo1$mediana[camp]+5,0.005, labels="Me", col="white")
box()
dev.off()


##############################################
#   Redemption-Risk plot
##############################################
png(paste0("images/",titolo,".png"), width=1280,height=800)
titolo="Promo anni 2012-2014 tutti settori"
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013,2014)),]
RRplot(data=promo2, namex="mediana",namey="quantile..0.8", camp=1:nrow(promo2),
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=TRUE, layer="settore2",
       r=0.4,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=30)
dev.off()
titolo="Promo anni 2012-2014 tutti settori (con incertezza)"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013,2014)),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=TRUE, layer="settore2",
       r=0.54,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=T, incn=10)
dev.off()
titolo="Promo anni 2012-2014 tutti settori (strat anno)"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013,2014)),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=TRUE, layer="buoni_inizio_validita_aa",
       r=0.4,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=30)
dev.off()

titolo="Promo 20 campagne anno 2013"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2013)),]
promo2<-promo2[sample(nrow(promo2),20),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=TRUE, layer="settore2",
       r=0.5,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=T,range=0.1,
       incertezza=F, incn=10)
dev.off()
titolo="Promo ENTERTAINMENT anni 2012-2013"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013)
                     & promo1$settore2=="ENTERTAINMENT"),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=T,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.8,
       legenda=TRUE, layer="buoni_inizio_validita_aa",
       r=0.5,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=10)
dev.off()
titolo="Promo ENTERTAINMENT anni 2012-2013 - buoni_qta_emessa"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013)
                     & promo1$settore2=="ENTERTAINMENT"),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=T,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.8,
       legenda=TRUE, layer="buoni_inizio_validita_aa",
       r=NULL,quota="mean",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=10)
dev.off()
titolo="Promo FMCG anni 2012-2013"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013)
                     & promo1$settore2=="FMCG"),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=T,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.8,
       legenda=TRUE, layer="buoni_inizio_validita_aa",
       r=0.5,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=10)
dev.off()
titolo="Promo FMCG anni 2012-2013 - buoni_qta_emessa"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013)
                     & promo1$settore2=="FMCG"),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=T,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.8,
       legenda=TRUE, layer="buoni_inizio_validita_aa",
       r=NULL,quota="mean",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=10)
dev.off()
titolo="Promo anno 2013 - buoni_qta_emessa"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2013)),]
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.8,
       legenda=TRUE, layer="settore2",
       r=NULL,quota="mean",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=F,range=0.1,
       incertezza=F, incn=10)
dev.off()


png("images/Promo ENTERTAINMENT anno 2013 (sample 50).png", width=1280,height=800)
promo2<-promo1[which(promo1$settore2=="ENTERTAINMENT" & promo1$buoni_inizio_validita_aa==2013),]
promo2<-promo2[sample(1:nrow(promo2),30),]
RRplot(data=promo2,
       namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F, text.lab="promo_id", pos=4, cex=1,cex.etichetta=0.7,
       legenda=F, layer="buoni_inizio_validita_aa",
       r=0.5, v=F, h=F,
       main="Promo ENTERTAINMENT anno 2013 (sample 30)", CI=T)
dev.off()

#incertezza
png("images/Promo anno 2013.png", width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa==2013),]
promo2<-promo2[sample(1:nrow(promo2),10),]
RRplot(data=promo2,
       namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=T, text.lab="promo_id", pos=4, cex=1,cex.etichetta=0.7,
       legenda=F, layer="settore2",
       r=0.5, v=F, h=F,
       main="Promo ENTERTAINMENT anno 2013 (sample 30)", CI=F,
       incertezza=T)
dev.off()




png("images/median-skew.png", width=1280,height=800)
RRplot(promo1,"mediana","skew", camp, legend=F, pos=4, main="median-skew")
dev.off()
png("images/median-risk20.png", width=1280,height=800)
RRplot(promo,"mediana","risk20", camp, legend=F, pos=4,main="median-risk20")
dev.off()
png("images/median-risk10.png", width=1280,height=800)
RRplot(promo,"mediana","risk10", camp, legend=F, pos=4, main="median-risk10")
dev.off()
png("images/media.rf-skew.png", width=1280,height=800)
RRplot(promo,"media.rf","skew", camp, legend=F, pos=4,main="media.rf-skew")
dev.off()


png("images/media.rf-skew_all1.png", width=1280,height=800)
RRplot(promo1,"mediana","skew", camp=1:nrow(promo1), text=F,r=0.6, main="media.rf-skew_all1",legend=T, layer="settore2")
dev.off()
png("images/media.rf-skew_all2.png", width=1280,height=800)
RRplot(promo1,"mediana","skew", camp=1:nrow(promo1), text=F,r=NULL, main="media.rf-skew_all2", legend=T, layer="settore2")
dev.off()

png("images/media.rf-skew_year.png", width=1280,height=800)
RRplot(promo1,"mediana","skew", camp=1:nrow(promo1), text=F, layer="buoni_inizio_validita_aa", r=0.6,main="media.rf-skew_year")
dev.off()


png("images/median-skew_2013.png", width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa==2013),]
RRplot(promo2,"mediana","skew", camp=1:nrow(promo2), text=F, r=0.6, main="median-skew_2013",layer="settore2")
dev.off()


png("images/median-skew_ENTERTAINMENT.png", width=1280,height=800)
promo2<-promo1[which(promo1$settore2=="ENTERTAINMENT"),]
RRplot(promo2,"mediana","skew", camp=1:nrow(promo2), text=F, r=0.6, layer="buoni_inizio_validita_aa", main="median-skew_ENTERTAINMENT")
dev.off()


png("images/median-skew_ENTERTAINMENT2.png", width=1280,height=800)
promo2<-promo1[which(promo1$settore2=="ENTERTAINMENT"),]
RRplot(promo2,"mediana","skew", camp=1:nrow(promo2), text=F, r=0.6, layer="buoni_inizio_validita_aa", v="mean", h="mean", main="median-skew_ENTERTAINMENT2")
dev.off()

#TOP MANAGER
titolo="median-risk10_AGGREGATE_settore2"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-aggregate(.~settore2,data=promo1, mean)
promo2<-promo2[,-which(names(promo2)=="buoni_qta_emessa")]
b<-aggregate(buoni_qta_emessa~settore2,data=promo1, sum)
promo2<-merge(promo2,b,by="settore2")
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="settore2",pos=2,cex.legenda=1,cex.etichetta=1,
       legenda=T, layer="settore2",
       r=1,quota="mean",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=FALSE,range=0.2,
       incertezza=T, incn=30)
dev.off()

titolo="median-risk10_AGGREGATE_buoni_inizio_validita_aa anni 2011-2014"
png(paste0("images/",titolo,".png"), width=1280,height=800)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa%in%c(2011,2012,2013,2014)),]
promo2<-aggregate(.~buoni_inizio_validita_aa,data=promo2, median)
promo2<-promo2[,-which(names(promo2)=="buoni_qta_emessa")]
b<-aggregate(buoni_qta_emessa~buoni_inizio_validita_aa,data=promo1, sum)
promo2<-merge(promo2,b,by="buoni_inizio_validita_aa")
RRplot(data=promo2, namex="mediana",namey="risk10", camp=1:nrow(promo2),
       etichetta=F,text.lab="buoni_inizio_validita_aa",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=T, layer="buoni_inizio_validita_aa",
       r=NULL,quota="min",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=titolo, CI=T,range=0.2,
       incertezza=F, incn=30)
dev.off()

##################################à#
#plto redemption-anni
png("images/alice1.png", width=1280, height=800)
par(par.negative)
promo1<-promo[which(promo$settore2=="ENTERTAINMENT"
                    & promo$buoni_inizio_validita_aa %in% c(2012,2013)),]
#per slides
library(plotrix)
camp<-1
amici<-which(promo1$settore==promo1$settore[camp])
data<-promo1[c(camp,amici),]
layer=data$buoni_inizio_validita_aa
indice<-order(data$mediana)
y1<-data$mediana
y<-data$mediana[camp]
plot(sort(indice),y1[indice], ylim=c(0,100), ylab="Redemption",pch=NA, col=layer, cex=0.4, xaxt="n", xlab="")
lim<-par("usr")
rect(lim[1],lim[3] , lim[2], mean(y1), border =NULL, col = "greenyellow",lty=3)
rect(lim[1], median(y1), lim[2], quantile(y1,0.75), border =NULL, col = "orange",lty=3)
rect(lim[1], quantile(y1,0.75), lim[2], lim[4], border =NULL, col = "orangered2",lty=3)  
points(sort(indice),y1[indice],pch=16, col=layer, cex=1.5)
abline(h=mean(y1), lty="dotted", lwd=2)
range=0.2
ui<-promo1[camp, paste0("quantile..",0.5+range/2)]
li<-promo1[camp,paste0("quantile..",0.5-range/2)]
x<-which(indice==camp)
plotCI(x,y,ui=ui, li=li,cex=2,col=layer, lwd=2,scol="white",add=T)
#symbols(x,y, circles=0.2, add=TRUE, inches=FALSE, bg=layer)
testo.legenda=unique(layer)
cex.legenda=2
legend("bottomright", legend=testo.legenda, fill=testo.legenda, ncol=1, bty="n", xjust=1, yjust=1, cex=cex.legenda)
dev.off()

png("images/alice2.png", width=1280, height=800)
par(par.negative)
promo1<-promo[which(promo$settore2=="ENTERTAINMENT"
                    & promo$buoni_inizio_validita_aa %in% c(2012,2013)),]
#per slides
library(plotrix)
camp<-1
amici<-which(promo1$settore==promo1$settore[camp])
data<-promo1[c(camp,amici),]
layer=data$buoni_inizio_validita_aa
indice<-order(data$mediana)
y1<-data$mediana
y<-data$mediana[camp]
plot(sort(indice),y1[indice], ylim=c(0,100), ylab="Redemption",pch=NA, col=layer, cex=0.4, xaxt="n", xlab="")
lim<-par("usr")
# rect(lim[1],lim[3] , lim[2], mean(y1), border =NULL, col = "greenyellow",lty=3)
# rect(lim[1], median(y1), lim[2], quantile(y1,0.75), border =NULL, col = "orange",lty=3)
# rect(lim[1], quantile(y1,0.75), lim[2], lim[4], border =NULL, col = "orangered2",lty=3)  
points(sort(indice),y1[indice],pch=16, col=layer, cex=1.5)
abline(h=mean(y1), lty="dotted", lwd=2)
range=0.2
ui<-promo1[camp, paste0("quantile..",0.5+range/2)]
li<-promo1[camp,paste0("quantile..",0.5-range/2)]
x<-which(indice==camp)
plotCI(x,y,ui=ui, li=li,cex=2,col=layer, lwd=2,scol="white",add=T)
#symbols(x,y, circles=0.2, add=TRUE, inches=FALSE, bg=layer)
testo.legenda=unique(layer)
cex.legenda=2
legend("bottomright", legend=testo.legenda, fill=testo.legenda, ncol=1, bty="n", xjust=1, yjust=1, cex=cex.legenda)
dev.off()

png("images/alice3.png", width=1280, height=800)
par(par.negative)
mycols<-c("orange","red","green3","blue","cyan","magenta","yellow","gray") 
palette(mycols)
promo1<-promo[which(promo$buoni_inizio_validita_aa %in% c(2014)),]
#per slides
library(plotrix)
camp<-1
amici<-which(promo1$buoni_inizio_validita_aa==promo1$buoni_inizio_validita_aa[camp])
data<-promo1[c(camp,amici),]
layer=data$settore2
indice<-order(data$mediana)
y1<-data$mediana
y<-data$mediana[camp]
plot(sort(indice),y1[indice], ylim=c(0,100), ylab="Redemption",pch=NA, col=layer, cex=0.4, xaxt="n", xlab="")
lim<-par("usr")
# rect(lim[1],lim[3] , lim[2], mean(y1), border =NULL, col = "greenyellow",lty=3)
# rect(lim[1], median(y1), lim[2], quantile(y1,0.75), border =NULL, col = "orange",lty=3)
# rect(lim[1], quantile(y1,0.75), lim[2], lim[4], border =NULL, col = "orangered2",lty=3)  
points(sort(indice),y1[indice],pch=16, col=layer, cex=1.5)
abline(h=mean(y1), lty="dotted", lwd=2)
range=0.2
ui<-promo1[camp, paste0("quantile..",0.5+range/2)]
li<-promo1[camp,paste0("quantile..",0.5-range/2)]
x<-which(indice==camp)
plotCI(x,y,ui=ui, li=li,cex=2,col=layer, lwd=2,scol="white",add=T, pch=NA)
#symbols(x,y, circles=0.2, add=TRUE, inches=FALSE, bg=layer)
testo.legenda=unique(layer)
cex.legenda=2
legend("bottomright", legend=testo.legenda, fill=testo.legenda, ncol=1, bty="n", xjust=1, yjust=1, cex=cex.legenda)
dev.off()
# 


png("images/alice5.png", width=1280, height=800)
par(par.negative)
par(fg="white")
mycols<-c("orange","red","green3","blue","cyan","magenta","yellow","gray") 
palette(mycols)
promo1<-promo[which(promo$settore2=="ENTERTAINMENT"
                    & promo$buoni_inizio_validita_aa %in% c(2012,2013)),]
#per slides
camp<-which(promo1$promo_id==5837)
amici<-which(which(promo1$settore2==promo1$settore2[camp])!=camp)
data<-promo1[]
layer=data$buoni_inizio_validita_aa
indice<-order(data$buoni_inizio_validita)
y1<-data$mediana
y<-data$mediana[camp]
plot(sort(indice),y1[indice], ylim=c(0,100), ylab="Redemption",pch=NA, col=layer, cex=0.4, xaxt="n", xlab="Date")
lim<-par("usr")
# rect(lim[1],lim[3] , lim[2], mean(y1), border =NULL, col = "greenyellow",lty=3)
# rect(lim[1], median(y1), lim[2], quantile(y1,0.75), border =NULL, col = "orange",lty=3)
# rect(lim[1], quantile(y1,0.75), lim[2], lim[4], border =NULL, col = "orangered2",lty=3)  
points(sort(indice),y1[indice],pch=16, col=layer, cex=1.5)
abline(h=mean(y1), lty="dotted", lwd=2)
range=0.2
ui<-promo1[camp, paste0("quantile..",0.5+range/2)]
li<-promo1[camp,paste0("quantile..",0.5-range/2)]
x<-which(indice==camp)
library(plotrix)
plotCI(x,y,ui=ui, li=li,cex=2,col=layer, lwd=2,scol="white",add=T, pch=NA)
#symbols(x,y, circles=0.2, add=TRUE, inches=FALSE, bg=layer)
testo.legenda=unique(layer)
cex.legenda=2
legend("bottomright", legend=testo.legenda, fill=testo.legenda, ncol=1, bty="n", xjust=1, yjust=1, cex=cex.legenda)
dev.off()





#NUOVA MISURA DI RISCHIO
#P(X>beta | X> alpha) dove alpha è una soglia preimpostata (esempio 60%)
#pongo la soglia 
alpha=80
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2012,2013,2014)),]
at<-as.numeric(names(promo2)[144:243])
up<-at[at>alpha]
sum.up<-rowSums(promo2[, as.character(up)])
sum.tot<-rowSums(promo2[, as.character(at)])
promo2$risk<-sum.up/sum.tot


titolo="Promo ENTERTAINMENT anni 2012-2013 Top"
png(paste0("images/",titolo,".png"), width=1280,height=900, bg="black")
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="black")
par(par.negative)
promo2<-promo1[which(promo1$settore2=="ENTERTAINMENT"&
                     promo1$buoni_inizio_validita_aa %in%c(2012,2013)),]
RRplot(data=promo2, namex="mediana",namey="risk", camp=1:nrow(promo2),
       ylimiti="custom",alpha=70,cex.lab=2,
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.5,
       legenda=T, layer="buoni_inizio_validita_aa",
       r=0.4,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=NULL, CI=F,range=0.1,
       incertezza=F, incn=30)
dev.off()
titolo="Promo anno 2014 Top"
png(paste0("images/",titolo,".png"), width=1280,height=900, bg="black")
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="black")
par(par.negative)
promo2<-promo1[which(promo1$buoni_inizio_validita_aa %in%c(2014)),]
RRplot(data=promo2, namex="mediana",namey="risk", camp=1:nrow(promo2),
       ylimiti="custom",alpha=70,
       etichetta=F,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=0.5,
       legenda=T, layer="settore2",
       r=0.4,quota="max",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=NULL, CI=F,range=0.1,
       incertezza=F, incn=30)
dev.off()
titolo="Promo ENTERTAINMENT anno 2014 qta_buoni_emessa Top"
png(paste0("images/",titolo,".png"), width=1280,height=900, bg="black")
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="black")
par(par.negative)
promo2<-promo1[which(promo1$settore2=="ENTERTAINMENT"&
                       promo1$buoni_inizio_validita_aa %in%c(2014)),]
RRplot(data=promo2, namex="mediana",namey="risk", camp=1:nrow(promo2),
       ylimiti="custom",alpha=70,
       etichetta=T,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
       legenda=T, layer="settore2",
       r=NULL,quota="mean",
       h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
       h=FALSE,v=FALSE,
       main=NULL, CI=F,range=0.1,
       incertezza=F, incn=30)
dev.off()



#grafico similarità campagne

