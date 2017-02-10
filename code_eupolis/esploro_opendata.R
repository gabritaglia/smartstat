data<-read.csv("dati.lombardia.elenco.dataset.csv", header=T)

names(data)


table(data$CATEGORIA)

table(data$CATEGORIA, data$TIPO)
getwd()
sino<-read.table("sino.txt", header=T)

data<-cbind(data, sino)

table(sino)
table(data$sino, data$TIPO)

data$NOME[which(data$TIPO =="Grafico" & data$sino==1)]
data$NOME[which(data$TIPO =="Mappa" & data$sino==1)]
data$NOME[which(data$TIPO =="Vista filtrata" & data$sino==1)]



data$sino[which(data$TIPO =="Grafico" )]<-0
data$sino[which(data$TIPO =="Mappa" )]<-0
table(data$sino, data$TIPO)

table(data$CATEGORIA, data$sino)

write.csv(data, "OpenData2.csv")
data2<-read.csv("OpenData2.csv", header=T, sep=";")


data<-data2
table(data$sino)

  table(data$new_cat, data$sino)


data$sino[which(data$new_cat == "confini e suolo" & data$sino==0)]<-1


write.csv(data, "OpenData.csv")
data<-read.csv("OpenData.csv")

data.int<-data[which(data$sino==1),]
table( data$TIPO)

df<-as.data.frame(table(data$CATEGORIA))
fix(df)

table(data$sino)

dff<-as.data.frame(table(factor(data1$new_cat)))

data$NOME[which(data$new_cat=="montagna e paesaggio")]
data$NOME[which(data$new_cat=="confini e suolo")]
