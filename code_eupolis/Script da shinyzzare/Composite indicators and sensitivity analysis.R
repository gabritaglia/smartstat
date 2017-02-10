# Composite indicators and sensitivity analysis

require(lattice)
require(MASS)

norm.pesi<-function(l.pesi)
{
  l.pesi.n<-vector("list", length=n.lev-1)
  for(h in 1:(n.lev-1))
  {
    pesi<-l.pesi[[h]]
    l.pesi.n[[h]]<-as.matrix(pesi%*%solve(diag(colSums(pesi), nrow=ncol(pesi), ncol=ncol(pesi))))
  }
  l.pesi.n
}

synth<-function(ind, pesi)
{
comps<-ind%*%pesi
comps
}

hier<-function(ind,pesi,n.lev)
  {
  inds<-vector("list", n.lev)
  inds[[1]]<-ind
    for(h in 1:(n.lev-1))
      {
        inds[[h+1]]<-signif(synth(inds[[h]], pesi[[h]]),3)
      }
    inds
  }


check<-function(n.lev, l.pesi)
{
  chk<-1
  for(i in n.lev-1:2)
  {
    chk<-chk*(nrow(l.pesi[[i]])==ncol(l.pesi[[i-1]]))
  }
  chk
}

pos<-function(out, n.lev)
{
  rnk<-vector("list", n.lev)
  for(h in 1:n.lev)
  {
    rnk[[h]]<-apply(out[[h]], 2, function(x) n.unita-rank(x, ties.method="first")+1)
  }
rnk  
}

ordin<-function(out, n.lev)
{
  ord<-vector("list", n.lev)
  for(h in 1:n.lev)
  {
    ord[[h]]<-apply(out[[h]], 2, function(x) order(x, decreasing=TRUE))
  }
  ord  
}

Aggregazione<-function(ind, l.pesi.n, n.lev)
{
  pts<-hier(ind, l.pesi.n, n.lev) 
  rango<-pos(pts, n.lev) 
  ord<-ordin(pts, n.lev) 

  pts.dfr<-vector("list", n.lev)
  for(h in 1:n.lev)
  {
    pts.dfr[[h]]<-data.frame(nomi.unita,pts[[h]])
  }

  rnk.dfr<-vector("list", n.lev)
  for(h in 1:n.lev)
  {
    rnk.dfr[[h]]<-data.frame(nomi.unita,rango[[h]])
  }

  ord.dfr<-vector("list", n.lev)
  for(h in 1:n.lev)
  {
    ord.dfr[[h]]<-as.data.frame(matrix(nomi.unita[ord[[h]]], nrow=n.unita, byrow=FALSE))
  }
  
  output<-list(punteggi=pts, punteggi.dfr=pts.dfr, rnk=rango, rank.dfr=rnk.dfr, classifica=ord, classifica.dfr=ord.dfr)
  nom<-vector(length=n.lev)
  for(h in 1:n.lev)
  {
    nom[h]<-paste("Livello", as.character(h), sep="")
  }
names(output$punteggi)<-nom
names(output$rnk)<-nom
names(output$classifica)<-nom
names(output$punteggi.dfr)<-nom
names(output$rank.dfr)<-nom
names(output$classifica.dfr)<-nom

output
}

salva.file<-function(Aggr)
{
  for(h in 1:n.lev)
  {
    write.table(Aggr$punteggi.dfr[[h]], paste("Punteggi livello ", as.character(h), ".csv"), dec=",", sep = ";", row.names=FALSE, col.names=TRUE)
    write.table(Aggr$rank.dfr[[h]], paste("Posizione livello ", as.character(h), ".csv"), dec=",", sep = ";", row.names=FALSE, col.names=TRUE)
    write.table(Aggr$classifica.dfr[[h]], paste("Classifica livello ", as.character(h), ".csv"), dec=",", sep = ";", row.names=FALSE, col.names=TRUE)
  }
}

pesi.com<-function(l.pesi.norm)
{
  pesi.compl<-diag(rep(1,n.ind))
  for(h in 1:(n.lev-1))
  {
    pesi.compl<-pesi.compl%*%l.pesi.norm[[h]]
  }
  pesi.compl<-as.data.frame(cbind(seq(1:n.ind), pesi.compl))
  colnames(pesi.compl)<-c("Indicatore", "Peso")
  pesi.compl
}

sintesi.variabili<-function(Aggr)
{
  sintesi.var<-vector("list", n.lev)
  nomi<-NULL
  for(h in 1:n.lev)
  {
    sintesi.var[[h]]=apply(Aggr$punteggi[[h]], 2, summary)
    nomi<-c(nomi,paste("Livello.",as.character(h), sep=""))
  }
  names(sintesi.var)<-nomi
  sintesi.var
}

sintesi.us.pts<-function(Aggr, unita.statistica)
{  
  profilo.pts<-vector("list", n.lev) #Profilo in termini di punteggi sulle variabili/dimensioni
  nomi<-NULL
  for(h in 1:n.lev)
  {
    profilo.pts[[h]]<-Aggr$punteggi.dfr[[h]][Aggr$punteggi.dfr[[h]]$nomi.unita==unita.statistica,]
    nomi<-c(nomi,paste("Livello.",as.character(h), sep=""))
  }
  sintesi.unita.pts<-profilo.pts
  names(sintesi.unita.pts)<-nomi
  sintesi.unita.pts    
}

sintesi.us.rnk<-function(Aggr, unita.statistica)
{  
  profilo.rnk<-vector("list", n.lev) #Profilo in termini di rank sulle variabili/dimensioni
  nomi<-NULL
  for(h in 1:n.lev)
  {
    profilo.rnk[[h]]<-Aggr$rank.dfr[[h]][Aggr$rank.dfr[[h]]$nomi.unita==unita.statistica,]
    nomi<-c(nomi,paste("Livello.",as.character(h), sep=""))
  }
  sintesi.unita.rnk<-profilo.rnk 
  names(sintesi.unita.rnk)<-nomi
  sintesi.unita.rnk    
}

#FUNZIONI GRAFICHE

parallele<-function(Livello, unita.statistica)
{
  windows()
  par(mfrow=c(2,2))
  parcoord(Aggr$punteggi[[Livello]], col = "blue", lty = 1, var.label = TRUE, main=paste("Punteggio - Livello", Livello))
  parcoord(Aggr$rnk[[Livello]], col = "blue", lty = 1, var.label = TRUE,  main=paste("Classifica - Livello", Livello))
  
  colori<-rep("blue", n.unita)
  colori[which(nomi.unita==unita.statistica)]="red"
  spess<-rep(1, n.unita)
  spess[which(nomi.unita==unita.statistica)]=2
  
  parcoord(Aggr$punteggi[[Livello]], col = colori, lty = 1, var.label = TRUE, lwd=spess, main=paste("Punteggio - Livello", Livello))
  parcoord(Aggr$rnk[[Livello]], col = colori, lty = 1, var.label = TRUE, lwd=spess, main=paste("Classifica - Livello", Livello))
}

Grafico.var<-function(Aggr, Livello, Variabile, unita.statistica)
{
  windows()
  par(mfrow=c(1,2))
  colori<-rep("blue", n.unita)
  colori[which(nomi.unita==unita.statistica)]="red"
  spess<-rep(1, n.unita)
  spess[which(nomi.unita==unita.statistica)]=2
  plot(cbind(0,Aggr$punteggi[[Livello]][,Variabile]), col="blue", pch="-", xlab="Punteggio", ylab="", xaxt="n", cex=1.5*spess)
  plot(cbind(0,Aggr$punteggi[[Livello]][,Variabile]), col=colori, pch="-", xlab="Punteggio", ylab="", xaxt="n", cex=1.5*spess)
}

Grafico.coppie<-function(Aggr, Livello, unita.statistica)
{
  windows()
  colori<-rep("blue", n.unita)
  colori[which(nomi.unita==unita.statistica)]="red"
  pairs(Aggr$punteggi[[Livello]], col=colori, pch=19, main=paste("Variabili livello",Livello))
}

Grafico.due<-function(Aggr, Livello.1, Livello.2, Variabile.1, Variabile.2, unita.statistica)
{
  windows()
  xx<-Aggr$punteggi[[Livello.1]][,Variabile.1]
  yy<-Aggr$punteggi[[Livello.2]][,Variabile.2]
  colori<-rep("blue", n.unita)
  colori[which(nomi.unita==unita.statistica)]="red"
  plot(xx,yy, col=colori, pch=19, xlab=paste("Livello", Livello.1, "-", "Variabile", Variabile.1), ylab=paste("Livello", Livello.2, "-", "Variabile", Variabile.2))
}


#Funzioni simulazioni

simulazione.1<-function(unita.statistica, ind.1, min.1, max.1, passi)
{
  valori<-seq(from=min.1, to=max.1, length.out=passi)
  sim.pts<-vector(mode="numeric", length=passi)
  sim.rnk<-vector(mode="numeric", length=passi)
  ind.sim<-ind
  us<-which(nomi.unita==unita.statistica)
  for(h in 1:passi)
  {
    ind.sim[us, ind.1]<-valori[h]
    sim<-Aggregazione(ind.sim, l.pesi.norm, n.lev)
    sim.pts[h]<-sim$punteggi[[n.lev]][us,1]
    sim.rnk[h]<-sim$rnk[[n.lev]][us,1]
  }
  
  sim.ris<-data.frame(valori,sim.pts, sim.rnk)
  colnames(sim.ris)<-c("Indicatore", "Punteggio", "Classifica")
  sim.ris
}

simulazione.2<-function(unita.statistica, ind.1, ind.2, min.1, max.1, min.2, max.2, passi)
{
  valori.1<-seq(from=min.1, to=max.1, length.out=passi)
  valori.2<-seq(from=min.2, to=max.2, length.out=passi) 
  valori<-expand.grid(va1=valori.1, va2=valori.2)
  sim2.pts<-matrix(0,nrow=nrow(valori), ncol=ncol(valori))
  sim2.rnk<-matrix(0,nrow=nrow(valori), ncol=ncol(valori))
  ind.sim2<-ind
  us<-which(nomi.unita==unita.statistica)
  for(h in 1:nrow(valori))
  {
    ind.sim2[us, c(ind.1, ind.2)]<-as.matrix(valori[h,1:2])
    sim2<-Aggregazione(ind.sim2, l.pesi.norm, n.lev)
    valori$z[h]<-sim2$punteggi[[n.lev]][us,1]
    valori$w[h]<-sim2$rnk[[n.lev]][us,1]
    sim2.pts[h]<-sim2$punteggi[[n.lev]][us,1]
    sim2.rnk[h]<-sim2$rnk[[n.lev]][us,1]
  }
  
  sim2.ris<-valori
  colnames(sim2.ris)<-c("x", "y", "z", "w")
  sim2.ris
}  

simulazione.3<-function(unita.statistica, indicatori, passi)
{
  valori.sim3<-matrix(nrow=passi, ncol=length(indicatori))
  for(h in 1:length(indicatori))
  {
    valori.sim3[,h]<-seq(from=min(ind[,indicatori[h]]), to = max(ind[,indicatori[h]]), length.out=passi) 
  }
  sim3.pts<-vector(length=nrow(valori.sim3))
  sim3.rnk<-vector(length=nrow(valori.sim3))
  ind.sim3<-ind
  us<-which(nomi.unita==unita.statistica)
  for(h in 1:nrow(valori.sim3))
  {
    ind.sim3[us, indicatori]<-as.matrix(valori.sim3[h,])
    sim3<-Aggregazione(ind.sim3, l.pesi.norm, n.lev)
    sim3.pts[h]<-sim3$punteggi[[n.lev]][us,1]
    sim3.rnk[h]<-sim3$rnk[[n.lev]][us,1]
  }
  sim3.ris<-data.frame(seq(1:passi),sim3.pts, sim3.rnk)
  colnames(sim3.ris)<-c("Indicatore", "Punteggio", "Classifica")
  sim3.ris
}

#----------

#Caricamento dati e definizione variabili globali

setwd("C:\\Users\\Marco Fattore\\Dropbox\\Attività - Consulenza\\Cons - Eupolis")
dati<-read.csv("Var.csv", sep=";", dec=",", header = TRUE)
pesi.1<-as.matrix(read.csv("Pesi1.csv", sep=";", dec=",", header = FALSE))
pesi.2<-as.matrix(read.csv("Pesi2.csv", sep=";", dec=",", header = FALSE))
pesi.3<-as.matrix(read.csv("Pesi3.csv", sep=";", dec=",", header = FALSE))
pesi.4<-as.matrix(read.csv("Pesi4.csv", sep=";", dec=",", header = FALSE))

nomi.unita<<-dati[,1]          #Nomi delle unità statistiche
nomi.ind<<-colnames(dati[,-1]) #Nomi degli indicatori elementari
ind<<-as.matrix(dati[,-1])    #Indicatori elementari
n.unita<<-nrow(ind)            #Numero di unità statistiche
n.ind<<-ncol(ind)              #Numero di indicatori elementari
n.lev<<-sum(c(sum(pesi.1)!=0,sum(pesi.2)!=0, sum(pesi.3)!=0, sum(pesi.4)!=0))+1  #Numero di livelli nella gerarchia aggregativa
l.pesi<<-list(pesi.1, pesi.2, pesi.3, pesi.4) #Mette i pesi in una lista. Ogni livello in una componente della lista
l.pesi.norm<-norm.pesi(l.pesi) #Nomrmlizzazione pesi
l.pesi.norm<<-l.pesi.norm


#check(n.lev, l.pesi)  #verifica se i vettori dei pesi e il numero di livelli sono coerenti

#Analisi base

Aggr<-Aggregazione(ind, l.pesi.norm, n.lev) #Calcolo punteggi e classifiche ai vari livelli di aggregazione
salva.file(Aggr)

sintesi.var<-sintesi.variabili(Aggr)

unita.statistica<-"a4"
sintesi.us.pts(Aggr, unita.statistica)
sintesi.us.rnk(Aggr, unita.statistica)

pesi.complessivi<-pesi.com(l.pesi.norm)
barplot(as.vector(pesi.complessivi$Peso), names.arg=seq(1:n.ind), col="blue", main="Pesi complessivi degli indicatori")

#Grafici analisi base
unita.statistica<-"a2"
parallele(1, unita.statistica)
Grafico.var(Aggr, 3, 1, unita.statistica)
Grafico.coppie(Aggr, 2, unita.statistica)
Grafico.due(Aggr, 1, 3, 2, 1, unita.statistica)

#SIMULAZIONE

#Simulazione 1. Singolo indicatore elementare
unita.statistica<-"a4"
simul.1<-simulazione.1(unita.statistica, 1, 0, 10, 20) 
plot(simul.1$Indicatore, simul.1$Punteggio, pch=19, col="blue", xlab="Valore indicatore", ylab="Punteggio aggregato", main=paste("Unità statistica:", unita.statistica))
plot(simul.1$Indicatore, simul.1$Classifica, pch=19, col="blue", xlab="Valore indicatore", ylab="Posizione in classifica", ylim=c(1,n.unita), main=paste("Unità statistica:", unita.statistica))
punteggio1.max<-max(simul.1$Punteggio)
posizione1.max<-max(simul.1$Classifica)
punteggio1.max
posizione1.max

#Simulazione 2. Due indicatori elementari per volta

simul.2<-simulazione.2(unita.statistica, 1, 3, 0, 10, 0, 10, 5) 
levelplot(z~x*y, simul.2, contour=TRUE, labels=TRUE, cuts=10, xlab="Indicatore 1", ylab="Indicatore 2", main="Punteggio aggregato")
levelplot(w~x*y, simul.2, contour=TRUE, labels=TRUE, cuts=10, xlab="Indicatore 1", ylab="Indicatore 2", main="Posizione in classifica")
punteggio2.max<-max(simul.2$z)
posizione2.max<-max(simul.2$w)
punteggio2.max
posizione2.max

#Simulazione 3 - k indicatori congiuntamente

unita.statistica<-"a4"
simul.3<-simulazione.3(unita.statistica, c(1,3,4),50) 
plot(simul.3$Indicatore, simul.3$Punteggio, pch=19, col="blue", xlab="Passi", ylab="Punteggio aggregato")
plot(simul.3$Indicatore, simul.3$Classifica, pch=19, col="blue", xlab="Passi", ylab="Posizione in classifica", ylim=c(1,n.unita))
punteggio3.max<-max(simul.3$Punteggio)
posizione3.max<-max(simul.3$Classifica)
punteggio3.max
posizione3.max


