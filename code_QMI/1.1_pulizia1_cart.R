list.files()

d<-read.csv2("dati originali/Dati 19 dicembre 2014.csv")
names(d)
#divido il cartaceo dal cartaceo
which(d$promo_tipologia=="Cartaceo")
d<-d[which(d$promo_tipologia=="Cartaceo"),]


names(d)
sum(is.na(d$promo_distribuzione_biglietti))
summary(d$cliente_settore)
d$promo_id[which(d$cliente_settore=="")]
summary(d$cliente_sottosettore)
d$promo_id[which(d$cliente_sottosettore=="")]


levels(factor(d$buoni_inizio_validita))

which(d$buoni_inizio_validita=="")
which(d$buoni_inizio_validita_aa=="")

summary(d$buoni_dati_promo_definitivi)
sum(d$buoni_dati_promo_definitivi==0)

table(d$buoni_dati_promo_definitivi, d$buoni_inizio_validita_aa)
table(d$buoni_inizio_validita_aa)

check<-function(x){
  print(x)
  print(summary(d[,x]))
  #print(table(d[,x]))
  print(sum(is.na(d[,x])))
}

names(d)
for(i in names(d)[75:82])
  check(i)

table(d$set_anomalo, d$buoni_inizio_validita_aa)


#-- Rimozione dei campi inutili
#tolgo promo_tipologia (che per questo subset è sempre Cartaceo)
d<-subset(d, select=-c(promo_tipologia))
#tolgo promo_nome
d<-subset(d, select=-c(promo_nome))
#tolgo campi totalmente vuoti
vuoti<-which(apply(d,2,function(x)all(is.na(x))))
names(d)[vuoti]
d<-d[,-c(vuoti)] 
names(d)
#tolgo da promo_data_inizio a promo_durata
d<-subset(d, select=-c(promo_data_inizio,
                       promo_data_inizio_aa,
                       promo_data_inizio_mm,
                       promo_data_inizio_gg,
                       promo_data_fine,
                       promo_data_fine_aa,
                       promo_data_fine_mm,
                       promo_data_fine_gg,
                       promo_durata))
names(d)
#tolgo cliente_nome
d$cliente_nome<-factor(d$cliente_nome)
t_cliente<-table(d$cliente_nome)
#creo una variabile cliente tanto medio poco fedele a QMI
s<-summary(as.numeric(t_cliente))
names_bottom<-names(t_cliente)[which(t_cliente<=s[2])]
names_medium<-names(t_cliente)[which(t_cliente>s[2]&t_cliente<s[5])]
names_top<-names(t_cliente)[which(t_cliente>=s[5])]
d$cliente_qta_promo_class<-""
d$cliente_qta_promo_class[which(d$cliente_nome %in% names_bottom)]<-"cliente_poche_promo"
d$cliente_qta_promo_class[which(d$cliente_nome %in% names_medium)]<-"cliente_medie_promo"
d$cliente_qta_promo_class[which(d$cliente_nome %in% names_top)]<-"cliente_tante_promo"
table(d$cliente_qta_promo_class)
d<-subset(d, select=-c(cliente_nome))

#tolgo promo_presenza_feste
d<-subset(d, select=-c(promo_presenza_feste)) #0 su tutte le righe
#tolgo da buoni_inizio_validita a buoni_fine_validita_gg
d<-subset(d, select=-c( #buoni_inizio_validita,
                        #buoni_inizio_validita_aa,
                        buoni_inizio_validita_mm,
                        buoni_inizio_validita_gg,
                        #buoni_fine_validita,
                        buoni_fine_validita_aa,
                        buoni_fine_validita_mm,
                        buoni_fine_validita_gg
          ))
names(d)
#tolgo buoni_film_titolo
d<-subset(d, select=-c(buoni_film_titolo))
#tolgo buoni_film_id_tmdb
d<-subset(d, select=-c(buoni_film_id_tmdb))
#tolgo buoni_qta_redenta
# d<-subset(d, select=-c(buoni_qta_redenta))
names(d)

#tolgo da codici_inizio a id_printathome_promo (variabili print@home)
d<-subset(d, select=-c( codici_inizio,
                        codici_fine,
                        codici_validita,
                        codici_inseriti,
                        codici_stampati,
                        codici_redemption_digitale,
                        codici_conversione,  
                        codici_carnet,
                        codici_numero_utilizzi,
                        id_printathome_promo))

#SULLE RIGHE
# #escludo le promozioni del set_anomalo #sul Cartaceo non ce ne sono
# d<-d[-which(d$set_anomalo==1),]
d<-subset(d, select=-c(set_anomalo))

#modifico la classe della colonna ove necessario
names(d)
d.1<-d
#to factor
col_factor<-c( "promo_id",
               "promo_meccanica",
               "promo_distribuzione_biglietti",
               "cliente_idcrm",              
               "cliente_settore",              
               "cliente_sottosettore",
               "buoni_dati_promo_definitivi",
               "buoni_stagione_primavera",
               "buoni_stagione_estate",
               "buoni_stagione_autunno",
               "buoni_stagione_inverno",
               "buoni_feste_capodanno",
               "buoni_feste_epifania",
               "buoni_feste_sanvalentino",
               "buoni_feste_venticinqueaprile",
               "buoni_feste_primomaggio",
               "buoni_feste_pasqua",
               "buoni_feste_duegiugno",
               "buoni_feste_ferragosto",
               "buoni_feste_primonovembre",
               "buoni_feste_ottodicembre",
               "buoni_feste_natale",
               "buoni_feste_santostefano",
               "buoni_feste_ultimodellanno",
               "buoni_per_tutta_la_durata",
               "buoni_redemption_o_forfait",
               "buoni_intero_o_sconto",
               "buoni_tipologia",
               "buoni_film_genere1",
               "buoni_film_genere2",
               "buoni_film_genere3",
               "buoni_3d",
               "buoni_valido_lun",
               "buoni_valido_mar",
               "buoni_valido_mer",
               "buoni_valido_gio",
               "buoni_valido_ven",
               "buoni_valido_sab",
               "buoni_valido_dom",
               "cliente_qta_promo_class"
)

#to numeric
col_numeric<-c("buoni_durata",
               "buoni_qta_emessa",
               "buoni_qta_redenta",
               "promo_redemption",
               "buoni_inizio_validita_aa")

#to date
col_date<-c("buoni_inizio_validita",
            "buoni_fine_validita")

d[,col_factor]  <- lapply(d[,col_factor] , factor)
d[,col_numeric] <- lapply(d[,col_numeric] , as.numeric)
d[,col_date]    <- lapply(d[,col_date] , as.character)
dates<-d[,col_date]
for (j in 1:ncol(dates)){
    a<-1:nrow(dates)
    class(a)<-"Date"
    for(i in 1:length(a)){
      a[i]<-as.Date(dates[i,j], "%d/%m/%Y")}
    dates[,j]<-a
}
d[,col_date]<-dates

cartaceo<-d
save(cartaceo, file="dati/cartaceo")

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
#controllo incoerenze variabili
apply(d,2,function(x)sum(is.na(x)))

names(d)
#promo_tipologia
table(d$promo_tipologia) #ok
table(d$promo_meccanica) #ok
table(d$promo_distribuzione_biglietti) #ok

length(which(as.character(d$promo_meccanica)!=as.character(d$promo_distribuzione_biglietti))) #i due campi sono diversi
#idcrm
length(unique(d$cliente_idcrm))
sort(table(d$cliente_idcrm), decreasing=T) #ok
sort(table(d$cliente_idcrm)/nrow(d),decreasing=T) #il cliente più grosso conta il 5.7%

table(d$cliente_settore) #ok
table(d$cliente_sottosettore) #ok
which(d$buoni_durata<0)
plot(table(d$buoni_durata)) #ok #valutare possibilità di considerare categorie di durata
table(d$buoni_dati_promo_definitivi)
#le variabili binarie sono tali
apply(d[,13:30], 2, function(x) all(x%in%c(0,1))) #ok

table(d$buoni_redemption_o_forfait) #ok
table(d$buoni_intero_o_sconto) #ok

table(d$buoni_tipologia) #ok
table(d$buoni_film_genere1) #ok
table(d$buoni_film_genere2) #ok
table(d$buoni_film_genere3) #ok

apply(d[,37:44], 2, function(x) all(x%in%c(0,1))) #ok

which(d$buoni_qta_emessa<0) #ok
which(d$promo_redemption>100) #ok

