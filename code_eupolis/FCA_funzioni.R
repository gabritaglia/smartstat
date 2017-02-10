#Questo script implementa la ricerca dei concetti in una Formal Concept Analysis

#notazione
#G oggetti -> celle territoriali
#M attributi -> ontologie
#I relazioni -> tabella di occorrenze

numbers<-data.frame(rbind(one=    c(0,0,1,0,1),
                          two=    c(0,1,0,1,0),
                          three=  c(0,0,1,1,0),
                          four=   c(1,1,0,0,1),
                          five=   c(0,0,1,1,0),
                          six=    c(1,1,0,0,0),
                          seven=  c(0,0,1,1,0),
                          eight=  c(1,1,0,0,0),
                          nine=   c(1,0,1,0,1),
                          ten=    c(1,1,0,0,0)
))
names(numbers)<-c("composite","even","odd","prime","square")
numbers

#definizione funzione primo
#la fz primo ha come imput un set di oggetti (attributi) e restituisce tutti gli attributi (oggetti) condivisi da quegli oggetti (attributi)
#in altri termini A belongs to G; B belongs to M
# A'=B      # B'=A
#in questo caso si fa solo primo(A)=B
primoG<-function(currSubsetG,I){
  M<-1:ncol(I)
  currSubset_primoG<-M[which(apply(data.frame(I[currSubsetG,]),2,prod)==1)]
  return(currSubset_primoG)
}
primoM<-function(currSubsetM,I){
  G<-1:nrow(I)
  currSubset_primoM<-G[which(apply(data.frame(I[,currSubsetM]),1,prod)==1)]
  return(currSubset_primoM)
}
#definizione funzione secondo
#la fz secondo ha come imput un insieme di attributi (oggetti) e restituisce invece  tutti attributi (oggetti) che condividono gli oggetti (attributi) condivisi dagli attributi (oggetti)
#in altri termini A belongs to G; B belongs to M
# A''=(A')'=B'=A        # B''=(B')'=A'=B
#in questo caso si fa solo secondo(A)=primo(B)=A
secondoG<-function(currSubsetG,I){
  G<-1:nrow(I)
  currSubset_secondoG<-G[which(apply(data.frame(I[,primoG(currSubsetG,I)]),1,prod)==1)]
  return(currSubset_secondoG)
}
secondoM<-function(currSubsetM,I){
  M<-1:ncol(I)
  currSubset_secondoM<-M[which(apply(data.frame(I[primoM(currSubsetM,I),]),2,prod)==1)]
  return(currSubset_secondoM)
}



#esempi


primoM(c("odd", "prime"), numbers)

I<-numbers
########----   ALGORITMO 2 ----####### FUNZIONA!!
#BEGIN
FCA<-function(I){
    G<-1:nrow(I);M<-1:ncol(I)
    int<-list()
    cond<-c()
    int[[1]]<-M
    a<-expand.grid(rep(list(0:1), ncol(I))) #ricordarsi di cambiarlo a seconda
    a<-apply(a,2,as.logical)
    for(g in G){
          print(g)
          for (i in 1:nrow(a)){
              Y<-M[a[i,]]#oggetti che hanno quegli attributi
              primog<-primoG(g, I) #oggetti di g
              inters<-Y[Y %in%primog] #oggetti che hanno quegli attributi che ha anche g
              if(!any(unlist(lapply(int, function(x) identical(inters,x))))){#c'è un vettore uguale? se si non copiare, se no aggiungilo
              int<-c(int, list(inters))
              }
          }
    }
    ext<-list()
    for (m in 1:length(int)){ ext[[m]]<-primoM(int[[m]],I)} #calcoli preliminari
    concetti<-list(intent=int, extent=ext)
return(concetti)
}
################## FINE ALGORITMO 2 #####################################


HasseDiagram<-function(Z, profili, peso=rep(.04, nrow(Z)), colori="gray", main=NULL, cex=1) #Visualizza il diagramma di Hasse dei profili
{ library(parsec)
  #peso deve essere un vettore numerico i cui names devono essere i profili
  #colori deve essere un vettore numerico i cui names devono essere i profili
  C<-incidence2cover(Z)
  vertici<-vertices(C, shape="square")
  plot(-vertici, pty="m",pch=19, xlab="", ylab="", xaxt="n", yaxt="n",col="white", bty="n", fg="black", main=main)
  drawedges(C, -vertici, col="black")
  ifelse (peso != rep(.04, nrow(vertici)), peso<-peso[rownames(vertici)], rep(.04, nrow(vertici) )) #ordino i pesi secondo l'ordine dei vertici
  ifelse(colori!="gray", colori<-colori[rownames(vertici)], "gray") #ordino i colori secondo l'ordine dei vertici
  symbols(-vertici, circles=peso, add=TRUE, inches=FALSE, bg=colori)
  text(-vertici, paste0(rownames(profili$profiles),"\n n=",prf$freq), cex=cex) 
}

pop2prof2<-function (concetti, I,labtype = c("profiles", "progressive")) 
{ 
  #creazione della matrice da dare in pasto a var2pop2
  D<-matrix(0, ncol=ncol(I), nrow=length(concetti$int))
  for (m in 1:length(concetti$int)) D[m,concetti$int[[m]]]<-1
  D<-data.frame(D)
  n.ext<-c()
  for (m in 1:length(concetti$ext)) n.ext[m]<-length(concetti$ext[[m]])
  D.exp <- D[rep(row.names(D), n.ext), 1:ncol(D)]
  
  freq <- apply(D.exp, 1, function(x) paste(x, collapse = "-"))
  freq <- table(freq)
  profiles <- strsplit(names(freq), "-")
  profiles <- lapply(profiles, as.numeric)
  profiles <- matrix(unlist(profiles), ncol = ncol(D.exp), byrow = TRUE, 
                     dimnames = list(names(freq), names(D.exp)))
  profiles <- as.data.frame(profiles)
  m <- nrow(profiles)
  if (labtype[1] == "progressive") {
    labels <- sprintf(paste("P%0", ceiling(log(m, 10)), "i", 
                            sep = ""), 1:m)
    rownames(profiles) <- names(freq) <- labels
  }
  profiles2<-apply(profiles,2,as.logical)
  row.names(profiles)<-apply(profiles2,1,function(x) paste(names(I)[x], collapse="-"))
  names(freq)<-row.names(profiles)
  res <- list(profiles = profiles, freq = freq)
  class(res) <- "wprof"
  return(res)
}



FCADiagram<-function(concetti, I, cex=0.7){
  library(parsec)
  prf<-pop2prof2(concetti, I=I)                    
  Z<-getzeta(prf)     #Costruisco la matrice Z del poset
  HasseDiagram(Z, prf, cex=cex)
}




