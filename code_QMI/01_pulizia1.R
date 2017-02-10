list.files()

d<-read.csv2("Dati 19 dicembre 2014.csv")
names(d)
#-- Rimozione dei campi inutili
#tolgo promo_nome
d<-subset(d, select=-c(promo_nome))
#tolgo campi totalmente vuoti
vuoti<-which(apply(d,2,function(x)all(is.na(x))))
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
d<-subset(d, select=-c(cliente_nome))
#tolgo promo_presenza_feste
d<-subset(d, select=-c(promo_presenza_feste)) #0 su tutte le righe
#tolgo da buoni_inizio_validita a buoni_fine_validita_gg
d<-subset(d, select=-c(#buoni_inizio_validita,
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
d<-subset(d, select=-c(buoni_qta_redenta))
names(d)

#tolgo da codici_inizio a id_printathome_promo (variabili Print@Home)
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
#escludo le promozioni del set_anomalo
d<-d[-which(d$set_anomalo==1),]
d<-subset(d, select=-c(set_anomalo))

#modifico la classe della colonna ove necessario
names(d)
d.1<-d
#to factor
col_factor<-c( "promo_id",
              "promo_tipologia",
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
              "buoni_valido_dom"              
             )

#to numeric
col_numeric<-c("buoni_durata",
               "buoni_qta_emessa",
               "promo_redemption"
               )
#to date
col_date<-c("buoni_inizio_validita",
            "buoni_fine_validita")

d[,col_factor]  <- lapply(d[,col_factor] , factor)
d[,col_numeric] <- lapply(d[,col_numeric] , as.numeric)
d[,col_date]    <- lapply(d[,col_date] , as.character)
#d[,col_date]    <- lapply(as.character(d[1,col_date]) , as.Date, format="%d/%m%/%Y")


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
sort(table(d$cliente_idcrm)) #ok
sort(table(d$cliente_idcrm)/nrow(d)) #il cliente più grosso conta il 5.7%

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

#divido tra cartaceo e print@home
cartaceo<-d[which(d$promo_tipologia=="Cartaceo"),]
printhome<-d[which(d$promo_tipologia=="Print@home"),]


#salvataggio dei dati
save(cartaceo, file="cartaceo")
save(printhome, file="printhome")