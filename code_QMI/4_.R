load("dati/promo")
names(promo)

Perc<-seq(from = .00, to = .99, by= .01)
plot(1:100, Perc, type="n", xlab="Redemption", ylab="Freq Cum")
for(i in 1:nrow(promo)){
  redemption<-promo[i,43:142]
  lines(redemption, Perc,col=i)
}

#controllo che le stime stiano dentro gli intervalli di confidenza al 95%
n<-1724
m<-promo$promo_redemption[1:n]
l<-promo$lci95[1:n]
u<-promo$uci95[1:n]
q1<-promo$q1[1:n]
q3<-promo$q3[1:n]

ordine<-order(m)

plot(m[ordine],col="blue", cex=0.5, ylim=c(0,100))
lines(l[ordine],col="red", cex=1)
lines(u[ordine],col="green", cex=1)

length(m[!which(m>l & m<u)])
length(m[!which(m>q1 & m<q3)])

lines(q1[ordine],col="red4", cex=1)
lines(q3[ordine],col="green4", cex=1)

#ampiezza degli intervalli di confidenza
summary(u-l)
cor(m,u-l)
plot(m,u-l, cex=0.2) #la foresta sembra prevedere peggio le redemption basse
lm(u-l~ m)
#standard deviation
s<-promo$stdev
summary(s)
cor(m,s)
plot(m,s, cex=0.2) #la foresta sembra prevedere peggio le redemption basse
lm(s ~ m)

#ampiezza del range interquartile
summary(q3-q1)
cor(m,q3-q1)
plot(m,q3-q1, cex=0.2) #la foresta sembra prevedere peggio le redemption basse





decili<-seq(53,142,by=10)
decili
names(promo)[decili]


Fx<-promo[1:12,43:142]
x11()
par(mfrow=c(3,4))
for(i in 1:nrow(Fx)){
  plot(as.numeric(Fx[i,]),Perc, xlim=c(0,100), ylim=c(0,1), type="n")
  lines(as.numeric(Fx[i,]),Perc)
  abline(a=0,b=0.01,col="red")
  }

fx<-promo[,144:243]
hit<-as.numeric(names(promo[,144:243]))
x11()
par(mfrow=c(3,4))
for(i in 1:ncol(fx)){
  hist(rep(hit,times=fx[i,i]))
}

par(mfrow=c(1,1))


ci<-promo[,c("quantile..0.25","quantile..0.75")]

ci<-cbind(ci,m)
ci<-ci[order(ci$m),]

out<-!which(m>ci[,1] & m<ci[,2])
plot(m, promo$promo_redemption, cex=0.5, col=out+1)
plot(m, cex=0.5, col=out+1)
points(ci[,1])
cor(promo[,decili])
######################################
#MAPPA DI KOHONEN
######################################
data<-promo[,decili]
dim(data)
data<-apply(data,2,as.numeric)
data <- data/rowSums(data)

set.seed(98)
library(kohonen)
mappa <- som(data=data, grid = somgrid(1, 20, "hexagonal"))
table(mappa$unit.classif)
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

######################################
#MAPPA DI KOHONEN
######################################
data<-promo[which(promo$train==1),seq(51,143,by=10)]
dim(data)
data<-apply(data,2,as.numeric)
data <- data/rowSums(data)
set.seed(98)
library(kohonen)
mappa <- som(data=data, grid = somgrid(10, 10, "hexagonal"))

plot(mappa,"codes")
table(mappa$unit.classif)
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
#associo a ciascuna azienda il cluster pi? vicino a lei (1 dei 100)
#che ha contribuito a generare
cluster <- data.frame(cluster=as.numeric(map(mappa,data)$unit.classif))
pts <- data.frame(mappa$grid$pts)
pts$cluster <- 1:nrow(pts)
#qui creo le coordinate per poter plottare le aziende
unione <- merge(cluster,pts,by="cluster")
unione[,2] <- jitter(unione[,2],1)
unione[,3] <- jitter(unione[,3],1)
#qui ci metto i puntini fatti come puntini rossi
plot(mappa, palette.name=coolBlueHotRed)
points(unione[,2:3],col="purple",pch=20,cex=0.5)

q<-summary(promo$buoni_qta_emessa)[c(2,5)]
qta_classe<-rep("media",nrow(promo))
length(qta_classe)
qta_classe[promo$buoni_qta_emessa<q[1]]<-"bassa"
qta_classe[promo$buoni_qta_emessa>q[2]]<-"alta"
qta_classe<-as.numeric(factor(qta_classe))
qta_classe

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
} 
palette(c("red", "blue", "purple"))
plot(mappa,palette.name=coolBlueHotRed)
points(unione[,2:3],col=qta_classe,pch=20,cex=0.8)


######################################

#qui invece ho tagliato la mappa in 3 parti
som.hc <- cutree(hclust(dist(mappa$codes)), 4)
add.cluster.boundaries(mappa, som.hc)



