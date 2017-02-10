data<-"2015-04-26.csv"





setwd("/home/gabriele/TWTR/DATI/H3G/tweets")
files<-list.files()
riepilogo<-as.data.frame.matrix(matrix(ncol=2, nrow=length(files)))
names(riepilogo)<-c("data", "numero_tweets_scaricati")

for (i in 1:length(files)){
  load(files[i])
  
  riepilogo[i,1]<-substr(files[i],1,10);
  riepilogo[i,2]<-nrow(megafile);
}


dati<-read.csv2("C:/Users/gabriele/Desktop/Riepiologo_tweets_scaricati_2014-05-19_2015_04_26.csv")

barplot(dati$numero_tweets_scaricati)
summary(dati$numero_tweets_scaricati)
