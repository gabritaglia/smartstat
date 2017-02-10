###############################################
#   Redemption-Risk plot con incertezza
##############################################
# RRplot<-function(data, namex,namey, camp=1:now(data), text=T,text.lab="promo_id", legend=T, layer=NULL,pos=4,cex=1, r=NULL, v="median", h="median", main=NULL){
x<-data[camp, namex ]
y<--data[camp, namey]

x2<-data[camp, "quantile..0.55"]
x1<-data[camp, "quantile..0.45"]

#puntini in 45-55
p<-as.numeric(data[camp,89:99])   #quantile 0.45-0.55
rip<-data[100, 189:199]
p1<-rep(p,rip*10)
y1<-rep(y, times=length(p1))
p1<-jitter(p1,2)
y1<-jitter(y1,2)
plot(x, y,type="n",xlim=c(0,100),ylim=c(min(data[,namey]), max(data[,namey])))
points(p1,y1, pch=1, bg="white", col="red", cex=0.5)
points(x,y, pch=4, bg="blue", cex=1.5, lwd=2)
abline(v=Mex, col="black", lwd=2, lty=3)
abline(h=Mey, col="black", lwd=2, lty=3)

#puntini in 0-100
p<-as.numeric(data[camp,44:143])   #quantile 0-100
rip<-data[100, 144:243]
p1<-rep(p,rip*100)
y1<-rep(y, times=length(p1))
p1<-jitter(p1,10)
y1<-jitter(y1,2)
plot(x, y,type="n",xlim=c(0,100),ylim=c(min(data[,namey]), max(data[,namey])))
points(p1,y1, pch=1, bg="white", col="red", cex=0.5)
points(x,y, pch=4, bg="blue", cex=1.5, lwd=2)
abline(v=Mex, col="black", lwd=2, lty=3)
abline(h=Mey, col="black", lwd=2, lty=3)

#rettangolo (brutto)
MM<-as.numeric(data[camp,c(89,99)])
plot(x, y,type="n",xlim=c(0,100),ylim=c(min(data[,namey]), max(data[,namey])))
points(p1,y1, pch=1, bg="white", col="red", cex=0.5)
points(x,y, pch=4, bg="blue", cex=1.5, lwd=2)
Rett=matrix(c(MM[2]-MM[1], 1), nrow=length(camp))
symbols(x,y, rectangles=Rett, add=T)
abline(v=Mex, col="black", lwd=2, lty=3)
abline(h=Mey, col="black", lwd=2, lty=3)


#resample
MM<-as.numeric(data[camp,c(89,99)])
p<-seq(MM[1], MM[2],by=0.01) 

p<-rep(as.numeric(data[camp,89:99]),times=100)
plot(p)
p1<-jitter(p,100) 
plot(p1)
rip<-as.numeric(data[100, 189:199])
w<-rep(rip, times=100)/sum(rep(rip, times=100))
p2<-sample(p1,size=200, replace=T, prob=w)
plot(p2)
y1<-rep(y, times=length(p2))
y1<-jitter(y1,2)
plot(x, y,type="n",xlim=c(0,100),ylim=c(min(data[,namey]), max(data[,namey])))
points(p2,y1, pch=1, bg="white", col="red", cex=0.5)
points(x,y, pch=4, bg="blue", cex=1.5, lwd=2)
Rett=matrix(c(MM[2]-MM[1], 1), nrow=length(camp))
symbols(x,y, rectangles=Rett, add=T)
abline(v=Mex, col="black", lwd=2, lty=3)
abline(h=Mey, col="black", lwd=2, lty=3)

plot(x, y,type="n",
     xlab=paste0("Redemption (", namex, ")"), 
     ylab=paste0("Rischio (", namey,")"), 
     xlim=c(0,100),
     ylim=c(min(data[,namey]), max(data[,namey])),
     main=main)
Mex<-median(data[,namex])
Mey<-median(data[,namey])  
abline(v=Mex, col="black", lwd=2)
abline(h=Mey, col="black", lwd=2)

symbols(x,y, squares=c(x1-x2), add=TRUE, inches=FALSE, col="red")
points(x,y, add=TRUE, pch=16, col="red")




if(v=="median")Mex<-median(data[,namex])
if(v=="mean")Mex<-mean(data[,namex])
if(h=="median")Mey<-median(data[,namey])
if(h=="mean")Mey<-mean(data[,namey])
if(length(r)==1)r<-rep(r, nrow(data))
if(is.null(r))r<-data$buoni_qta_emessa[camp]/max(data$buoni_qta_emessa)
if(!is.null(layer)) {layer=data[,layer]
                     legenda=unique(layer)}
plot(x, y,type="n",
     xlab=paste0("Redemption (", namex, ")"), 
     ylab=paste0("Rischio (", namey,")"), 
     xlim=c(0,100),
     ylim=c(min(data[,namey]), max(data[,namey])),
     main=main)
abline(v=Mex, col="black", lwd=2)
abline(h=Mey, col="black", lwd=2)
symbols(x,y, circles=r, add=TRUE, inches=FALSE, bg=layer)
if(text==T) text(x,y,data[camp, text.lab], pos=pos)
if(legend==T)
  legend("bottomright", legend=legenda, fill=1:length(legenda), ncol=1, bty="n", xjust=1, yjust=1, cex=cex)
}

library(ggmap)

stat_density2d(data=cultura.coords,aes(x=x,y=y,fill=..level..,alpha=..level..),h=.02,size =0.003,bins=16,geom='polygon')

library("MASS")
data(geyser)
m <- ggplot(geyser, aes(x = duration, y = waiting)) +
  geom_point() + xlim(0.5, 6) + ylim(40, 110)
m + geom_density2d()

m <- ggplot(promo_spat, aes(x = mediana, y = -skew)) +
  geom_point()


ggplot(eom_density2d(data=promo_spat,aes(x=mediana,y=skew,),h=.02,size =0.003,bins=16)
       +scale_fill_gradient(low="green",high="red")
       +scale_alpha(range=c(0.00,0.9),guide=TRUE)
)




ggmap(  stat_density2d(data=cultura.coords,aes(x=x,y=y,fill=..level..,alpha=..level..),h=.02,size =0.003,bins=16,geom='polygon')+
          #geom_path(data=area,aes(x=x,y=y),colour="blue",size=1)+
          #xlab("")+ylab("")+ggtitle(titolo)+
          #xlab("")+ylab("")+
          theme(plot.title = element_text(size=20))+
          scale_fill_gradient(low="green",high="red")+
          scale_alpha(range=c(0.00,0.9),guide=TRUE)+
          tema.format.ggplot()
)