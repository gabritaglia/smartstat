#Questo script implementa la ricerca dei concetti in una Formal Concept Analysis

#notazione
#G oggetti -> celle territoriali
#M attributi -> ontologie
#I relazioni -> tabella di occorrenze

#creazione di una tabella di esempio
animals<-data.frame(rbind(bat=          c(0,1,0,0,1,1,0,1,0), 
                          eagle=        c(0,1,1,0,1,1,0,0,0),
                          monkey=       c(0,0,0,1,1,0,0,1,0),
                          parrotfish=   c(1,0,1,0,1,0,1,0,0),
                          penguin=      c(0,0,1,0,1,1,1,0,0),
                          shark=        c(1,0,0,0,1,0,1,0,0),
                          lanternfish=  c(1,0,0,0,1,0,1,0,1)#,
                          #tutto=       c(1,1,1,1,1,1,1,1,1)#,
                          #nulla=        c(0,0,0,0,0,0,0,0,0)
                    ))
is(animals)
names(animals)<-c("breathes_in_water","can_fly","has_beak", "has_hands","has_skeleton","has_wings","lives_in_water","is_viviparous","produces_light")
animals

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
primoG(c("parrotfish","shark"),animals)
primoM(c("has_beak","lives_in_water"), animals)

secondoG(c("parrotfish", "shark"),animals)
secondoM(c("has_beak", "lives_in_water"),animals)

primoM(c("odd", "prime"), numbers)

int<-list()
G<-1:nrow(I);M<-1:ncol(I)

########----   ALGORITMO 1 ----#######
if(FALSE){
#BEGIN
int[[+1]]<-list(oggetti=primoM(M,I),attributi=M) #int={(M',M)}
i<-0
currSubset<-c(max(G))       #currSubset=max{g belonging to G}
nextObj<-c(max(G))          #nextObj=max{g belonging to G}
while(length(currSubset)<length(G)){ #while currSubset!=G
      currSubset_secondo<-secondoG(currSubset, I)
      currSubset_primo<-primoG(currSubset,I)
      if(!any(currSubset_secondo[!currSubset_secondo%in%currSubset]) | 
        max(currSubset_secondo[!currSubset_secondo%in%currSubset])<nextObj) #there is no g belonging to currSubset''\currSubset such that g<nextObj
          {
                addint<-list()
                addint[[+1]]<-list(oggetti=currSubset_secondo, attributi=currSubset_primo) #C=C union {currSubset'', currSubset'}
                int<-c(int, addint) 
                nextObj<-max(G[!G%in%currSubset_secondo]) #nextObj=max{g belonging G\currSubset''}
                currSubset<-currSubset_secondo #currSubset=currSubset''
            }
        else{
                nextObj<-max(G[!G%in%currSubset][G[!G%in%currSubset]<max(currSubset)]) #max{g belonging to G\currSubset such that g<max(currSubset)}
            }       
    currSubset<-c(currSubset, nextObj) #currSubset= currSubset union {nextObj}                        
    currSubset<-currSubset[-which(nextObj<currSubset)] #currSubset=currSubset\{g belonging to currSubset such that nextIbj<g}
    i<-i+1;print(i)
}
#
}

setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB") 
load("pol1")
I<-data.frame(apply(!!pol1@data,2,as.numeric)[,c("tradizione","arte","storia","cultura","religioso","rurale","sport")]) #primo set di ontologie
I.freq<-pol1@data[,c("tradizione","arte","storia","cultura","religioso","rurale","sport")]
#I<-animals[1:4, 2:5]
#I<-numbers
########----   ALGORITMO 2 ----####### FUNZIONA!!
#BEGIN
G<-1:nrow(I);M<-1:ncol(I)
int<-list()
cond<-c()
int[[1]]<-M
a<-expand.grid(0:1,0:1,0:1,
               0:1,0:1,0:1,
               0:1) #ricordarsi di cambiarlo a seconda
a<-apply(a,2,as.logical)

for(g in G){
  print(g)
  for (i in 1:nrow(a) ){
      Y<-M[a[i,]]#oggetti che hanno quegli attributi
      primog<-primoG(g, I) #oggetti di g
      inters<-Y[Y %in%primog] #oggetti che hanno quegli attributi che ha anche g
      
      if(!any(unlist(lapply(int, function(x) identical(inters,x))))){#c'è un vettore uguale? se si non copiare, se no aggiungilo
        int<-c(int, list(inters))
      }
      #print(i)  
    }
}
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB")
#save(int,  file="Intent.ontologie2")
################## FINE ALGORITMO 2 #####################################
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB")
load("Intent.ontologie2")
##########################
#escludo alcuni concetti non interessanti
########################
ext<-list();for (m in 1:length(int)){ ext[[m]]<-primoM(int[[m]],I)} #calcoli preliminari
n.ext<-c();for (m in 1:length(ext)) n.ext[m]<-length(ext[[m]]) #calcoli preliminari

int<-int[-2] #tolgo il concetto vuoto
int<-int[-which(n.ext>1000)] #escludo i concetti troppo diffusi
int<-int[-which(n.ext<35)] #escludo i concetti troppo diffusi
#e li salvo a parte come originalità
originalita<-int[which(n.ext<35)]
str(int);str(I)
#ottengo gli extent dagli intent(int)
ext<-list()
for (m in 1:length(int)){
  ext[[m]]<-primoM(int[[m]],I)
}
str(ext)

names(I)<-c("tra","art","sto","cul","rel","rur","spo")

#creazione della matrice da dare in pasto a var2pop2
D<-matrix(0, ncol=ncol(I), nrow=length(int))
for (m in 1:length(int)) D[m,int[[m]]]<-1
D<-data.frame(D)
n.ext<-c()
for (m in 1:length(ext)) n.ext[m]<-length(ext[[m]])
D.exp <- D[rep(row.names(D), n.ext), 1:ncol(D)]


#metto sul poset
library(parsec)
HasseDiagram<-function(Z, profili, peso=rep(.04, nrow(Z)), colori="gray", main=NULL, cex=1) #Visualizza il diagramma di Hasse dei profili
{
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

pop2prof2<-function (y, I,labtype = c("profiles", "progressive")) 
{ 
  freq <- apply(y, 1, function(x) paste(x, collapse = "-"))
  freq <- table(freq)
  profiles <- strsplit(names(freq), "-")
  profiles <- lapply(profiles, as.numeric)
  profiles <- matrix(unlist(profiles), ncol = ncol(y), byrow = TRUE, 
                     dimnames = list(names(freq), names(y)))
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

prf<-pop2prof2(D.exp, I=I)

#                    
Z<-getzeta(prf)     #Costruisco la matrice Z del poset

HasseDiagram(Z, prf, cex=1)
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB/Eupolis Slides incontro 28 ottobre")
png("HasseDiagram-concetti.png",width=1024, height=512,cex=2)
HasseDiagram(Z, prf,cex=1.5)
dev.off()

#oggetti rilevanti
str(I) #matrice celle x ontologie (dicotomica)
str(I.freq) #matrice celle x ontologie (frequenze)
str(int) #lista degli intent dei concetti
str(ext) #lista degli extent dei concetti
str(prf) #lista matrice celle x ontologie (dicotomica) e vettore frequenze profili
#########################################
#creazione dei pesi di concetto
#########################################
# ogni cella ottiene un punteggio per ciascun concetto calcolato come segue:
# per ogni attributo del concetto si calcola n(a)-Me(a)
# dove n(a) è la frequenza dell'attributo per cella
# Me(a) è la mediana della frequenza dell'attributo per le celle che ce l'hanno (che sono tutte quelle dell'extent dell'intent)
#escludo il concetto vuoto

#esempio
j=31
ext[[j]]
int[[j]]
concetto<-I.freq[ext[[j]],int[[j]]];t(concetto)
apply(as.matrix(data.frame(concetto)),2,median)
a<-apply(as.matrix(data.frame(concetto)),1, function(x)x-apply(as.matrix(data.frame(concetto)),2,median));a
apply(a,2,sum) #intensità di concetto



intensitaC<-data.frame(matrix(NA,ncol=length(int), nrow=nrow(I.freq))) 
row.names(intensitaC)=1:nrow(I)
names(intensitaC)<-names(prf[[2]])
  
for(j in c(1:length(int))){ 
  b<-data.frame(row.names=1:nrow(I))
  concetto<-I.freq[ext[[j]],int[[j]]];concetto
  a<-matrix(apply(as.matrix(data.frame(concetto)),1, function(x)x-apply(as.matrix(data.frame(concetto)),2,median)),nrow=length(int[[j]]))
  #a[a<0]<-0
  a<-apply(a,2,sum);
  names(a)<-row.names(concetto)
  b<-merge(b,a,by=0, all=T) #merge by row.names
  b$Row.names<-as.numeric(b$Row.names)
  b<-b[order(b$Row.names),]
  intensitaC[,j]<-b[,2]
  print(j)
}


setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB/Eupolis Slides incontro 28 ottobre")
save(intensitaC, file="intensitaConcetti")

###############################################
#PER MILANO
###############################################
I.mi<-data.frame(apply(!!pol1.mi@data,2,as.numeric)[,c("tradizione","arte","storia","cultura","religioso","rurale","sport")]) #primo set di ontologie
I.mi.freq<-pol1.mi@data[,c("tradizione","arte","storia","cultura","religioso","rurale","sport")]
intensitaC.mi<-data.frame(matrix(NA,ncol=length(int), nrow=nrow(I.mi.freq))) 
row.names(intensitaC.mi)=1:nrow(I.mi)
names(intensitaC.mi)<-names(prf[[2]])

for(j in c(1:length(int))){ 
  b<-data.frame(row.names=1:nrow(I.mi))
  concetto<-I.mi.freq[ext[[j]],int[[j]]];concetto
  a<-matrix(apply(as.matrix(data.frame(concetto)),1, function(x)x-apply(as.matrix(data.frame(concetto)),2,median)),nrow=length(int[[j]]))
  #a[a<0]<-0
  a<-apply(a,2,sum);
  names(a)<-row.names(concetto)
  b<-merge(b,a,by=0, all=T) #merge by row.names
  b$Row.names<-as.numeric(b$Row.names)
  b<-b[order(b$Row.names),]
  intensitaC.mi[,j]<-b[,2]
  print(j)
}

dir.create("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB/Eupolis Slides incontro 28 ottobre.mi")
setwd("/media/tagliabue/DATI/Dropbox/SMARTSTAT/EUPOLIS/OpenData_MIMB/Eupolis Slides incontro 28 ottobre.mi")
save(intensitaC.mi, file="intensitaConcetti.mi")

