##############################################
#   Redemption-Risk plot
##############################################
RRplot<-function(data, namex,namey, camp=1:nrow(data),
                 ylimiti="standard",alpha=NULL,
                 cex.lab=1,
                 etichetta=TRUE,text.lab="promo_id",pos=4,cex.legenda=1,cex.etichetta=1,
                 legenda=TRUE, layer=NULL,
                 r=NULL,quota="max",
                 h1=FALSE,h2=FALSE,h3=FALSE,v1=FALSE,v2=FALSE,v3=FALSE,
                 h=FALSE,v=FALSE,
                 main=NULL, CI=FALSE,range=0.1,
                 incertezza=FALSE, incn=30,Xmediana=FALSE){
  require(quantregForest)
  require(plotrix)
  if(namey=="risk"){
    #P(X> alpha) dove alpha è una soglia preimpostata (esempio 60%)
    at<-as.numeric(names(data)[144:243])
    up<-at[at>alpha]
    sum.up<-rowSums(promo2[, as.character(up)])
    sum.tot<-rowSums(promo2[, as.character(at)])
    data$risk<-sum.up/sum.tot
  }  
    x<-data[camp, namex ]
  y<-data[camp, namey]
  
  if(length(r)==1)r<-rep(r, nrow(data[camp,]))
  if(is.null(r)){
    if(quota=="max")  r<-data[camp,"buoni_qta_emessa"]/max(data[,"buoni_qta_emessa"])
    if(quota=="mean") r<-data[camp,"buoni_qta_emessa"]/mean(data[,"buoni_qta_emessa"])
    if(quota=="min")  r<-data[camp,"buoni_qta_emessa"]/min(data[,"buoni_qta_emessa"])
    if(quota=="sum")  r<-data[camp,"buoni_qta_emessa"]/sum(data[,"buoni_qta_emessa"])}
  if(!is.null(layer)) {layer=data[,layer]
                       testo.legenda=unique(layer)}
  if(range>1 | range<0){stop("0 < range <1 ")}
  x2<-data[camp, paste0("quantile..",0.5+range/2)]
  x1<-data[camp,paste0("quantile..",0.5-range/2)]
  if(namey=="skew" | ylimiti=="custom")ylim=c(min(data[,namey]), max(data[,namey]))
  else ylim=c(0,100)
  if(namey=="risk") ylab=paste0("Rischio (Redemption > ", alpha, ")")
  else paste0("Rischio (", namey,")")
  plot(x, y,type="n",
       xlab=paste0("Redemption (", namex, ")"), 
       ylab=ylab, 
       xlim=c(0,100),
       ylim=ylim,
       main=main,fg="white", cex.lab=cex.lab)
  Mex<-median(data[,namex])
  Mey<-median(data[,namey])
  q1x<-quantile(data[,namex],0.25)
  q3x<-quantile(data[,namex],0.75)
  q1y<-quantile(data[,namey],0.25)
  q3y<-quantile(data[,namey],0.75)
  lim<-par("usr")
  rect(lim[1], lim[3], lim[2], lim[4], border =NULL, col = "greenyellow",lty=3)
  rect(lim[1], lim[3], q1x, q1y, border =NULL, col = "greenyellow",lty=3)
  rect(Mex, lim[3], lim[2], Mey, border =NULL, col = "khaki1",lty=3)
  rect(lim[1], Mey, q3x, lim[4], border =NULL, col = "khaki1",lty=3)  
  rect(Mex,Mey,lim[2],lim[4], border =NULL, col = "orange",lty=3)
  rect(q3x, q3y, lim[2], lim[4], border =NULL, col = "orangered2",lty=3)
  if(h==FALSE){h1=F;h2=F;h3=F}
  if(v==FALSE){v1=F;v2=F;v3=F}
  if(v1==T) abline(v=q1x, col="green", lwd=2, lty=3)
  if(v2==T)abline(v=Mex, col="black", lwd=2, lty=3)  
  if(v3==T) abline(v=q3x, col="red", lwd=2, lty=3)
  if(h1==T) abline(h=q1y, col="green", lwd=2, lty=3)
  if(h2==T) abline(h=Mey, col="black", lwd=2, lty=3)
  if(h3==T) abline(h=q3y, col="red", lwd=2, lty=3)
  if(incertezza==F) symbols(x,y, circles=r, add=TRUE, inches=FALSE, bg=layer)
  if(CI==T) plotCI(x,y,ui=x2, li=x1, pch=NA,err="x",cex=1,col="red", lwd=2,scol="black",add=T)
  if(etichetta==T) text(x,y,data[camp, text.lab], pos=pos, cex=cex.etichetta)
  if(!is.null(layer) & legenda==T){
    if(namey=="risk")
      legend("topleft", legend=testo.legenda, fill=testo.legenda, ncol=1, bty="n", xjust=1, yjust=1, cex=cex.legenda, col="black")
  if(incertezza==T) { for(i in 1:length(x1)){
    incx<-sample(seq(x1[i],x2[i], by=0.01),incn, replace=T)
    incy<-jitter(rep(y[i],times=incn),0.7)
   points(incx,incy, pch=16, cex=1, col=layer[i])
  }
  points(x,y,pch=16, cex=1, col=layer)
  if(Xmediana==T) points(x,y,pch=4,cex=3, col="black",lwd=3)
  }
}
}

