#CAMPIONAMENTO
NUM_TUTTI<-300000 #numerosità campione generale
NUM_CARRELLO_ACQUISTO<-100000


# NOME_FILE_ID<-"tab_pag1_id.csv"
# NOME_FILE_ID<-"tab_pag2_id.csv"
NOME_FILE_ID<-"tab_pag3_id.csv"
# NOME_FILE_ID<-"tab_pag4_id.csv"

# "tab_pag1_id.csv"# 1 semestre 2013
# "tab_pag2_id.csv"# 2 semestre 2013
# "tab_pag3_id.csv"# 1 semestre 2014
# "tab_pag4_id.csv"# 2 semestre 2014


#come codifica degli oggetti ho usato questo:
# oggetto_2013.1 1 semestre 2013
# oggetto_2014.2  2 semestre 2013
# oggetto_2013.1 1 semestre 2014
# oggetto_2014.2 2 semestre 2014

#CAMPIONAMENTO ID TUTTI
library(dplyr)
getwd()
list.files()

id_2014.1<-read.csv(NOME_FILE_ID, stringsAsFactors=F)
names(id_2014.1)<-c("id","count")
str(id_2014.1)

unici_id_2014.1<-id_2014.1 %>% distinct(id)
sum(id_2014.1$count)

load("prodotti_cookies_carrello_compra")
names(prodotti)


# prodotti2013.1<-prodotti[prodotti$day>=as.Date("2013-01-01") & prodotti$day<as.Date("2013-07-01") ,]
# prodotti2014.2<-prodotti[prodotti$day>=as.Date("2013-07-01") & prodotti$day<as.Date("2013-12-31") ,]
prodotti2014.1<-prodotti[prodotti$day>=as.Date("2014-01-01") & prodotti$day<as.Date("2014-07-01") ,]
# prodotti2014.2<-prodotti[prodotti$day>=as.Date("2014-07-01") & prodotti$day<as.Date("2014-12-31")  ,]


sample2014.1<-id_2014.1[sample(1:nrow(id_2014.1),NUM_TUTTI),"id"]

save(sample2014.1, file="id_campione/id_campione_2014_1sem") #<--- MODIFICARE


sum(prodotti2014.1$id%in%sample2014.1)
nrow(prodotti2014.1)/nrow(id_2014.1)
sum(prodotti2014.1$id%in%sample2014.1)/length(sample2014.1)


#CAMPIONAMENTO ID CARRELLO E COMPRATORI
sample2014.1_Carrello_Acquisto<-prodotti2014.1[sample(1:nrow(prodotti2014.1),NUM_CARRELLO_ACQUISTO),"id"]
save(sample2014.1_Carrello_Acquisto, file="id_campione/id_campione_2014_1sem_carrello_acquisto") #<--- MODIFICARE