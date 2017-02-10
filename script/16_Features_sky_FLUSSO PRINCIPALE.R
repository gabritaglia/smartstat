library(magrittr)
library(dplyr)
library(data.table)
library(stringr)
library(tm)
#dati di input
dir_input="Sample_stagioni/GenFeb2013"
dir_output="Sample_stagioni/GenFeb2013"
dir_os="Sample_stagioni"
file_visite<-"visite_stagione_GenFeb2013"
file_piattaforma<-"piattaforma_stagione_GenFeb2013"
file_pagine<-"pagine_stagione_GenFeb2013"
file_prodotti<-"prodotti_stagione_GenFeb2013"
file_log<-"log_stagione_GenFeb2013"


##---------------------------------------------------------------------
load(paste0(dir_input,"/",file_visite))
v<-visite_stagione_GenFeb #<-----MODIFICARE


# cookies<- v %>% 
# 	distinct(visitorID)  %>%
# 	select(visitorID)

cookies<-data.frame(unique(v$visitorID))
names(cookies)<-"visitorID"


cookies$visitorID<-as.character(cookies$visitorID)

ore_prelavoro<-c("06","07","08")
ore_mattina<-c("09","10","11","12")
ore_pranzo<-c("13","14")
ore_pome<-c("15","16","17")
ore_postlavoro<-c("18","19","20")
ore_sera<-c("21","22","23","00")
ore_notte<-c("01","02","03","04","05")
#fascia_oraria
v$fascia_oraria<-""
v$fascia_oraria[which(v$ora %in% ore_prelavoro)]<-"0_prelavoro"
v$fascia_oraria[which(v$ora %in% ore_mattina)]<-"1_morning"
v$fascia_oraria[which(v$ora %in% ore_pranzo)]<-"2_lunch"
v$fascia_oraria[which(v$ora %in% ore_pome)]<-"3_afternoon"
v$fascia_oraria[which(v$ora %in% ore_postlavoro)]<-"4_postlavoro"
v$fascia_oraria[which(v$ora %in% ore_sera)]<-"5_evening"
v$fascia_oraria[which(v$ora %in% ore_notte)]<-"6_night"

rm(ore_prelavoro,ore_mattina, ore_pranzo, ore_pome, ore_postlavoro, ore_sera, ore_notte)
#n_visite per giorno-ora
giorno<-v$day
levels(giorno)<-c("5Friday","1Monday","6Saturday","7Sunday","4Thursday","2Tuesday","3Wednesday")
ora<-factor(v$ora)
levels(ora)<-c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
giorno.ora<-paste(giorno, ora)
v$giorno.ora<-giorno.ora

v$visitorID<-as.character(v$visitorID)
a <- data.table(v$giorno.ora,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)]<-0
t<-data.frame(t)
t<-t  %>%
	add_rownames(var = "visitorID")
cookies<-left_join( cookies, t, by = "visitorID")  
rm(giorno, ora, giorno.ora)


#n_visite
t<-table(as.character(v$visitorID))
t<-data.frame(t)
names(t)<-c("visitorID", "n_visite")
t$visitorID<-as.character(t$visitorID)
cookies<-left_join( cookies, t, by = "visitorID")  
cookies$n_visite<-as.integer(cookies$n_visite)
rm(t)
#p_visite_day
v$visitorID<-as.character(v$visitorID)
a <- data.table(v$day,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)]<-0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_Lun",
						"p_visite_Mar",
						"p_visite_Mer",
						"p_visite_Gio",
						"p_visite_Ven",
						"p_visite_Sab",
						"p_visite_Dom")
t<-t  %>%
	add_rownames(var = "visitorID")
cookies<-left_join( cookies, t, by = "visitorID")  
rm(t,a)
#p_visite_we
a <- data.table(v$we,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_wd",
						"p_visite_we")
t<-t  %>%
	add_rownames(var = "visitorID")
cookies<-left_join( cookies, t, by = "visitorID")  
rm(t,a)
#p_visite_matt/pra/pome/sera/notte
a <- data.table(v$fascia_oraria,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_prelav",
						"p_visite_morn",
						"p_visite_lunc",
						"p_visite_afte",
						"p_visite_postlav",
						"p_visite_even",
						"p_visite_nigh")
t<-t  %>%
	add_rownames(var = "visitorID")
cookies<-left_join( cookies, t, by = "visitorID")  
rm(t,a)
#p_visite_fascia_we/wd
v[v$we==1 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_we"]<-"0_prelavoro_we"
v[v$we==1 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_we"
v[v$we==1 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_we"
v[v$we==1 & v$fascia_oraria=="2_lunch", "fascia_oraria_we"]<-"1_lunch_we"
v[v$we==1 & v$fascia_oraria=="3_afternoon", "fascia_oraria_we"]<-"3_afternoon_we"
v[v$we==1 & v$fascia_oraria=="4_postlavoro", "fascia_oraria_we"]<-"4_postlavoro_we"
v[v$we==1 & v$fascia_oraria=="5_evening", "fascia_oraria_we"]<-"5_evening_we"
v[v$we==1 & v$fascia_oraria=="6_night", "fascia_oraria_we"]<-"6_night_we"
v[v$we==0 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_we"]<-"0_prelavoro_wd"
v[v$we==0 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_wd"
v[v$we==0 & v$fascia_oraria=="2_lunch", "fascia_oraria_we"]<-"1_lunch_wd"
v[v$we==0 & v$fascia_oraria=="3_afternoon", "fascia_oraria_we"]<-"3_afternoon_wd"
v[v$we==0 & v$fascia_oraria=="4_postlavoro", "fascia_oraria_we"]<-"4_postlavoro_wd"
v[v$we==0 & v$fascia_oraria=="5_evening", "fascia_oraria_we"]<-"5_evening_wd"
v[v$we==0 & v$fascia_oraria=="6_night", "fascia_oraria_we"]<-"6_night_wd"
a <- data.table(v$fascia_oraria_we,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_prelav_wd",
						"p_visite_prelav_we",
						"p_visite_morn_wd",
						"p_visite_morn_we",
						"p_visite_lunc_wd",
						"p_visite_lunc_we",
						"p_visite_afte_wd",
						"p_visite_afte_we",
						"p_visite_postlav_wd",
						"p_visite_postlav_we",
						"p_visite_even_wd",
						"p_visite_even_we",
						"p_visite_nigh_wd",
						"p_visite_nigh_we")
t<-t  %>%
	add_rownames(var = "visitorID")
t$visitorID<-as.character(t$visitorID)
# cookies<-left_join( cookies, t, by = "visitorID") #inspiegabile motivo non andava
cookies<-merge( cookies, t, by = "visitorID")  
rm(t,a)



#p_visite_fascia_lunven_sab_dom
v[v$day=="Saturday" & v$fascia_oraria=="0_prelavoro","fascia_oraria_wdSabDom"]<-"0_prelavoro_sab"
v[v$day=="Saturday" & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_sab"
v[v$day=="Saturday" & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_sab"
v[v$day=="Saturday" & v$fascia_oraria=="2_lunch", "fascia_oraria_wdSabDom"]<-"1_lunch_sab"
v[v$day=="Saturday" & v$fascia_oraria=="3_afternoon", "fascia_oraria_wdSabDom"]<-"3_afternoon_sab"
v[v$day=="Saturday" & v$fascia_oraria=="4_postlavoro", "fascia_oraria_wdSabDom"]<-"4_postlavoro_sab"
v[v$day=="Saturday" & v$fascia_oraria=="5_evening", "fascia_oraria_wdSabDom"]<-"5_evening_sab"
v[v$day=="Saturday" & v$fascia_oraria=="6_night", "fascia_oraria_wdSabDom"]<-"6_night_sab"
v[v$day=="Sunday"   & v$fascia_oraria=="0_prelavoro","fascia_oraria_wdSabDom"]<-"0_prelavoro_dom"
v[v$day=="Sunday" & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_dom"
v[v$day=="Sunday" & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_dom"
v[v$day=="Sunday" & v$fascia_oraria=="2_lunch", "fascia_oraria_wdSabDom"]<-"1_lunch_dom"
v[v$day=="Sunday" & v$fascia_oraria=="3_afternoon", "fascia_oraria_wdSabDom"]<-"3_afternoon_dom"
v[v$day=="Sunday" & v$fascia_oraria=="4_postlavoro", "fascia_oraria_wdSabDom"]<-"4_postlavoro_dom"
v[v$day=="Sunday" & v$fascia_oraria=="5_evening", "fascia_oraria_wdSabDom"]<-"5_evening_dom"
v[v$day=="Sunday" & v$fascia_oraria=="6_night", "fascia_oraria_wdSabDom"]<-"6_night_som"
v[v$we==0 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_wdSabDom"]<-"0_prelavoro_lunven"
v[v$we==0 & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_lunven"
v[v$we==0 & v$fascia_oraria=="2_lunch", "fascia_oraria_wdSabDom"]<-"1_lunch_lunven"
v[v$we==0 & v$fascia_oraria=="3_afternoon", "fascia_oraria_wdSabDom"]<-"3_afternoon_lunven"
v[v$we==0 & v$fascia_oraria=="4_postlavoro", "fascia_oraria_wdSabDom"]<-"4_postlavoro_lunven"
v[v$we==0 & v$fascia_oraria=="5_evening", "fascia_oraria_wdSabDom"]<-"5_evening_lunven"
v[v$we==0 & v$fascia_oraria=="6_night", "fascia_oraria_wdSabDom"]<-"6_night_lunven"
a <- data.table(v$fascia_oraria_wdSabDom,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_prelav_dom",
						"p_visite_prelav_sab",
						"p_visite_prelav_lunven",
						"p_visite_morn_dom",
						"p_visite_morn_sab",
						"p_visite_morn_lunven",
						"p_visite_lunc_dom",
						"p_visite_lunc_sab",
						"p_visite_lunc_lunven",
						"p_visite_afte_dom",
						"p_visite_afte_sab",
						"p_visite_afte_lunven",
						"p_visite_postlav_dom",
						"p_visite_postlav_sab",
						"p_visite_postlav_lunven",          
						"p_visite_even_dom",
						"p_visite_even_sab",
						"p_visite_even_lunven",
						"p_visite_nigh_dom",
						"p_visite_nigh_sab",
						"p_visite_nigh_lunven")
t<-t  %>%
	add_rownames(var = "visitorID")
t$visitorID<-as.character(t$visitorID)
# cookies<-left_join( cookies, t, by = "visitorID") #inspiegabile motivo non andava
cookies<-merge( cookies, t, by = "visitorID")  
rm(t,a)


#quanti giorni n_days_visite
t<- v %>% 
	distinct(visitorID, giorno)  %>%
	select(visitorID, giorno) %>%
	group_by(visitorID) %>%
	summarise(
		n_days_visite = n_distinct(giorno),
		first_day=min(as.Date(as.character(giorno))),
		last_day=max(as.Date(as.character(giorno))),
		int_days_life=last_day-first_day+1,
		p_days_visiteSUlife=n_days_visite/int_days_life
	)
#trovo l'intervallo temporale globale di osservazione in giorni
intervallo_giorni<-as.numeric(max(as.Date(as.character(v$giorno)))-min(as.Date(as.character(v$giorno))))
t$p_days_visiteSUperiodo<-t$n_days_visite/intervallo_giorni
cookies<-inner_join( cookies, t, by = "visitorID")  

rm(t, intervallo_giorni)
#rimarrebbe da sfruttare la variabile last_visit ma è categoriale e non saprei adesso come fare ad associarla al singolo cookie

save(file=paste0(dir_output,"/cookies_checkpoint_visite"), cookies)
rm(v)

# load(file=paste0(dir_output,"/cookies_checkpoint_visite"))
##------------------------------------------------------------------------------------
##--- PIATTAFORMA_SAMPLE
load(paste0(dir_input,"/",file_piattaforma))
p<-piattaforma_stagione_GenFeb
# p<-p[,-1]
os<-read.csv2(paste0(dir_os,"/","os2.csv"), stringsAsFactor=F)
p$os<-as.character(p$os)
p<- left_join(p,os,by="os")
p<-p[,!names(p)%in%c("piattaforma.x")]
names(p)[names(p)=="piattaforma.y"]<-"piattaforma"
levels(p$piattaforma)[1]<-"nonspec" #rinomino ALTRO in non specificato
p$visitorID<-as.character(p$visitorID)
p_merge<-unique(p[,c("visitorID", "piattaforma","old","vendor", "browser")])
# u<-unique(p_merge[,c("visitorID", "piattaforma","old","vendor", "browser")])
#Qui sorge il problema della non unicità del browser e os (e delle variabili conseguenti) al visitorID, sono pochi casi (88 massimo) che potrebbero essere errori. 
#Verificare che siano effettivamente errori
#Per ovviare questa faccenda ora io elimino d'ufficio i duplicati
dup<-duplicated(p_merge$visitorID)
p_merge<-p_merge[!dup,]
#variabile mobile dummy (i nonspec sono considerati d'ufficio pc)
p_merge$mobile<-0
p_merge$mobile[p_merge$piattaforma=="mobile"]<-1
p_merge$vendor.mobile<-paste0(p_merge$vendor,".",p_merge$mobile)

cookies<-left_join(cookies, p_merge, by="visitorID")
rm(os,p_merge, dup)
rm(p)
cookies$mobile.vendor<-factor(paste0(cookies$piattaforma,".",cookies$vendor))

save(file=paste0(dir_output,"/cookies_checkpoint_piattaforma"), cookies)

# load(file=paste0(dir_output,"/cookies_checkpoint_piattaforma"))
##------------------------------------------------------------------------------------
##--- PAGINE_SAMPLE
load(paste0(dir_input,"/",file_pagine))
pg<-pagine_sample
rm(pagine_sample)
names(pg)<-c("hour", "visitorID", "entry_pages", "pages", "channels", "page_views")
pg$visitorID<-as.character(pg$visitorID)
#operazioni fatte una tantum con system locale ENGLISH
pg$hour<-as.character(pg$hour)
pg$ts<- strptime(pg$hour,"%b %d, %Y, Hour %H")
pg$giorno <- format(pg$ts,"%Y-%m-%d")
#numero di pagine viste dal cookie nel periodo
t<-table(pg$visitorID)
t<-data.frame(t)
names(t)<-c("visitorID","n_pagine_viste")
t$visitorID<-as.character(t$visitorID)
cookies<-left_join(cookies, t, by="visitorID")

save(file=paste0(dir_output,"/cookies_checkpoint_pagine_1"), cookies)

#numero di pagine viste dal cookie al giorno min max mean me q1 q3
a <- data.table(pg$giorno,pg$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-a %>% 
	group_by(V2) %>%
	summarize(min_pages_giorno=mean(N),   #forse un po' lento 
						q1_pages_giorno=quantile(N, 0.25),
						mean_pages_giorno=mean(N),
						median_pages_giorno=quantile(N, 0.5),
						q3_pages_giorno=quantile(N, 0.75),
						max_pages_giorno=max(N))
t<-as.data.frame(t)
names(t)[1]<-"visitorID"
t$visitorID<-as.character(t$visitorID)
cookies<-left_join(cookies, t, by="visitorID")


#numero di entry page per cookies
pgt <- data.table(pg)
pgt<-pgt[, list(n_entry_pages = length(unique("entry_pages"))), by = c("visitorID", "entry_pages")]
t<-table(pgt$visitorID)
t<-as.data.frame(t)
names(t)<-c("visitorID","n_entry_pages_distinte")
t$visitorID<-as.character(t$visitorID)
cookies<-left_join(cookies,t,by="visitorID")
rm(pgt,t)

save(file=paste0(dir_output,"/cookies_checkpoint_pagine_2"), cookies)


#INIZIO A GENERARE LE DUE TABELLE SEPARATE URL E entryURL


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#richiamo lo script che genera la funzioni per lavorare sugli URL
source("16_Parole_per_Features_sky.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#richiamo lo script che genera la lista delle parole
source("16_Parole_per_Features_sky.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# load("sample2.1/cookies")
# load("sample/pagine_sample")
# pg<-pagine_sample %>% filter(Pages %in% pagine_sample$Pages[1:100])
# names(pg)[2:4]<-c("visitorID", "entry_pages", "pages")


#ATTACCO IL PROFILO DELLE PAGES AI COOKIES
#carico il file con tutti gli URL delle PAGES semantizzate
URL<-read.csv2(file=paste0(dir_output,"/URL_PAGES_semantizzate_2014"))

#aggrego la tabella pagine rispetto alla chiave id pages e aggiungo la frequenza (pg1)
pg1<-	pg %>%
			group_by(visitorID, pages) %>% 
			select(visitorID, pages) %>%  
			summarise(n_pages=n())
pg1<-data.frame(pg1)

pg1<-left_join(pg1, URL, by="pages") #contiene id pages-n_pages-profilo_dell_url

for(j in 4:ncol(pg1))
pg1[,j]<-pg1[,j]*pg1$n_pages #moltiplico il profilo_url per il numero di volte che quel cookies ha visto la pagina
pg1[is.na(pg1)]<-0 #metto a zero tutti gli NA
PrimaCol<-which(names(pg1)=="pages_AreaAssistenza")
t1<-aggregate(pg1[PrimaCol:ncol(pg1)], by=list(factor(pg1$visitorID)), sum)
names(t1)[1]<-"visitorID"
t1$visitorID<-as.character(t1$visitorID)
t1[is.na(t1)]<-0

cookies<-left_join(cookies,t1, by="visitorID")
#fine pages


#ATTACCO IL PROFILO DELLE ENTRYPAGES AI COOKIES
#carico il file con tutti gli URL delle PAGES semantizzate
entryURL<-read.csv2(file=paste0(dir_output,"/URL_ENTRYPAGES_semantizzate_2014"))

#aggrego la tabella pagine rispetto alla chiave id entry_pages e aggiungo la frequenza (pg2)
pg2<-	pg %>%
	group_by(visitorID, entry_pages) %>% 
	select(visitorID, entry_pages) %>%  
	summarise(n_entry_pages=n())
pg2<-data.frame(pg2)

pg2<-left_join(pg2, entryURL, by="entry_pages") #contiene id pages-n_pages-profilo_dell_url

PrimaCol2<-which(names(pg2)=="entry_AreaAssistenza")
for(j in PrimaCol2:ncol(pg2))
	pg2[,j]<-pg2[,j]*pg2$n_entry_pages #moltiplico il profilo_url per il numero di volte che quel cookies ha visto la entry page
pg2[is.na(pg2)]<-0 #metto a zero tutti gli NA
t<-aggregate(pg2[PrimaCol2:ncol(pg2)], by=list(factor(pg2$visitorID)), sum)
names(t2)[1]<-"visitorID"
t2$visitorID<-as.character(t2$visitorID)
t2[is.na(t2)]<-0

cookies<-left_join(cookies,t2, by="visitorID")
#fine entry pages


save(file=paste0(dir_output,"/cookies_checkpoint_pagine_4"), cookies)

#adesso torno a lavorare sulla tabella cookies

var_entry_Area<-names(cookies)[sapply(names(cookies), grepl, pattern="entry_Area")]          
#calcolo numero totale di entry pages
cookies$n_entry_pages_totali<-apply(cookies[,var_entry_Area],1,sum)
cookies$n_entry_pages_totali[cookies$n_entry_pages_totali==0]<-1


save(file=paste0(dir_output,"/cookies_checkpoint_pagine_5"), cookies)
#load(file=paste0(dir_output,"/cookies_checkpoint_pagine_5"))

#devo normalizzare! 
#pages
var_pages<-names(cookies)[sapply(names(cookies), grepl, pattern="pages_")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="entry")] #tolgo n_entry_pages_totali
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="giorno")] #tolgo min_ q1_ mean_ median_ q3_ max_ pages_giorno
var_pages_commerciale<-var_pages[sapply(var_pages, grepl, pattern="commerciale")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="commerciale")]
var_pages_Area<-var_pages[sapply(var_pages, grepl, pattern="Area")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="Area")]
var_pages_video<-var_pages[sapply(var_pages, grepl, pattern="video")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="video")]

var_pages_commerciale
var_pages_Area
var_pages_video
var_pages

#normalizzo le pages_Area
summary(cookies[,var_pages_Area])
somma<-apply(cookies[,var_pages_Area],1,sum)
for (i in var_pages_Area){
	cookieS[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}
#normalizzo le pages_commerciale
summary(cookies[,var_pages_commerciale])
somma<-apply(cookies[,var_pages_commerciale],1,sum)
for (i in var_pages_commerciale){
	cookies[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}
#normalizzo le pages e video
summary(cookies[,var_pages])
summary(cookies[,var_pages_video])
somma<-apply(cookies[,var_pages],1,sum)
cookies[,var_pages_video]<-cookies[,var_pages_video]/somma
cookies[is.na(cookies[,var_pages_video]),var_pages_video]<-0
for (i in var_pages){
	cookies[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}

#entry
var_entry<-names(cookies)[sapply(names(cookies), grepl, pattern="entry_")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="pages")] #tolgo n_entry_pages_totali
var_entry_commerciale<-var_entry[sapply(var_entry, grepl, pattern="commerciale")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="commerciale")]
var_entry_Area<-var_entry[sapply(var_entry, grepl, pattern="Area")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="Area")]
var_entry_video<-var_entry[sapply(var_entry, grepl, pattern="video")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="video")]

#normalizzo le entry_Area
summary(cookies[,var_entry_Area])
somma<-apply(cookies[,var_entry_Area],1,sum)
for (i in var_entry_Area){
	cookies[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}
#normalizzo le entry_commerciale
summary(cookies[,var_entry_commerciale])
somma<-apply(cookies[,var_entry_commerciale],1,sum)
for (i in var_entry_commerciale){
	cookies[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}
#normalizzo le entry
summary(cookies[,var_entry])
summary(cookies[,var_entry_video])
somma<-apply(cookies[,var_entry],1,sum)
cookies[,var_entry_video]<-cookies[,var_entry_video]/somma
cookies[is.na(cookies[,var_pages_video]),var_pages_video]<-0
for (i in var_entry){
	cookies[,i]<-cookies[,i]/somma
	cookies[is.na(cookies[,i]),i]<-0
}

cookies$n_entry_pages_totali[is.na(cookies$n_entry_pages_totali)]<-1

rm(var_entry,var_entry_commerciale,var_pages_commerciale,var_entry, var_entry_Area)
rm(var_pages,var_pages_commerciale,var_pages_commerciale,var_pages, var_pages_Area)
rm(pg, pg1)
save(file=paste0(dir_output,"/cookies_checkpoint_pagine_6"), cookies)

rm(t, URL,entryURL,pages, entry_pages, lista.parole)


##------------------------------------------------------------------------------------
##--- PRODOTTI_SAMPLE
load(paste0(dir_input,"/",file_prodotti))
pd<-prodotti_sample
# pd<-pd[,-1]
rm(prodotti_sample)
#operazioni fatte una tantum con system locale ENGLISH
pd$hour<-as.character(pd$hour)
pd$ts<- strptime(pd$hour,"%b %d, %Y, Hour %H")
pd$giorno <- format(pd$ts,"%Y-%m-%d")

#compra
compratoreID<-as.character((pd$visitorID[!is.na(pd$purchaseID)]))
cookies[,"compra"]<-0
cookies[cookies$visitorID%in%compratoreID,"compra"]<-1
#carrello
summary(pd$products)
# length(which(pd$products!=""))
carrelloID<-as.character((pd$visitorID[pd$products!=""]))
cookies[,"carrello"]<-0
cookies[cookies$visitorID%in%carrelloID,"carrello"]<-1

#vede_promo
# summary(pd$promo)
# length(which(pd$promo!=""))
vede_promoID<-as.character((pd$visitorID[pd$promo!=""]))
cookies[,"vede_promo"]<-0
cookies[cookies$visitorID%in%vede_promoID,"vede_promo"]<-1

#n_pacchetti_carrello
products<-as.character(unique(pd$products))
pacchetti <- strsplit(products, split="\\+")
n_pacchetti<-as.numeric(as.character(sapply(pacchetti, length)))

qualepacchetto<-function(nome){
	yes<-lapply(pacchetti, function(table) match(x=nome,table, nomatch=F)>0)
	yes<-as.numeric(yes)
	return(yes)
}
products<-data.frame(cbind(products, n_pacchetti))
unique(unlist(pacchetti))
products$PacchettoSkyTV<-qualepacchetto("SkyTV")
products$PacchettoSkyGo<-qualepacchetto("SkyGo")
products$PacchettoMySkyHD<-qualepacchetto("MySkyHD")
products$PacchettoParabola<-qualepacchetto("Parabola")
products$PacchettoFamiglia<-qualepacchetto("SkyFamiglia")
products$PacchettoCalcio<-qualepacchetto("Calcio")
products$PacchettoHD<-qualepacchetto("HD")
products$PacchettoSkyHD<-qualepacchetto("SkyHD")
products$PacchettoSport<-qualepacchetto("Sport")
products$PacchettoCinema<-qualepacchetto("Cinema")
products$PacchettoSky3D<-qualepacchetto("Sky3D")

pd<-merge(pd,products, by="products")
#sul carrello
pd_subset<-pd[which(pd$visitorID%in%carrelloID),]
names(pd_subset)
PrimaCol<-which(sapply(names(pd_subset), grepl, pattern="n_pacchetti"))
pd_subset[,PrimaCol:ncol(pd_subset)]<-apply(pd_subset[,PrimaCol:ncol(pd_subset)],2,function(x)as.numeric(as.character(x)))
t<-aggregate(pd_subset[,PrimaCol:ncol(pd_subset)], by=list(pd_subset$visitorID), max)
names(t)[1]<-c("visitorID")
cookies<-merge(cookies, t, by="visitorID", all.x=T)
t<-aggregate(pd_subset[,PrimaCol:ncol(pd_subset)], by=list(pd_subset$visitorID), sum)
names(t)[1]<-c("visitorID")
names(t)[2:ncol(t)]<-paste0("tot_inserimenti_",names(t)[2:ncol(t)])
cookies<-merge(cookies, t, by="visitorID", all.x=T)

cookies[is.na(cookies$n_pacchetti),"n_pacchetti"]<-0
cookies[is.na(cookies$PacchettoSkyTV),"PacchettoSkyTV"]<-0
cookies[is.na(cookies$PacchettoSkyGo),"PacchettoSkyGo"]<-0
cookies[is.na(cookies$PacchettoMySkyHD),"PacchettoMySkyHD"]<-0
cookies[is.na(cookies$PacchettoParabola),"PacchettoParabola"]<-0
cookies[is.na(cookies$PacchettoFamiglia),"PacchettoFamiglia"]<-0
cookies[is.na(cookies$PacchettoCalcio),"PacchettoCalcio"]<-0
cookies[is.na(cookies$PacchettoHD),"PacchettoHD"]<-0
cookies[is.na(cookies$PacchettoSkyHD),"PacchettoSkyHD"]<-0
cookies[is.na(cookies$PacchettoSport),"PacchettoSport"]<-0
cookies[is.na(cookies$PacchettoCinema),"PacchettoCinema"]<-0
cookies[is.na(cookies$PacchettoSky3D),"PacchettoSky3D"]<-0

cookies[is.na(cookies$tot_inserimenti_n_pacchetti),"tot_inserimenti_n_pacchetti"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoSkyTV),"tot_inserimenti_PacchettoSkyTV"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoSkyGo),"tot_inserimenti_PacchettoSkyGo"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoMySkyHD),"tot_inserimenti_PacchettoMySkyHD"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoParabola),"tot_inserimenti_PacchettoParabola"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoFamiglia),"tot_inserimenti_PacchettoFamiglia"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoCalcio),"tot_inserimenti_PacchettoCalcio"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoHD),"tot_inserimenti_PacchettoHD"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoSkyHD),"tot_inserimenti_PacchettoSkyHD"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoSport),"tot_inserimenti_PacchettoSport"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoCinema),"tot_inserimenti_PacchettoCinema"]<-0
cookies[is.na(cookies$tot_inserimenti_PacchettoSky3D),"tot_inserimenti_PacchettoSky3D"]<-0


#sul carrello
pd_subset<-pd[which(pd$visitorID%in%compratoreID & !is.na(pd$purchaseID)),] #compratori nella visita che hanno acquistato
names(pd_subset)
PrimaCol<-which(sapply(names(pd_subset), grepl, pattern="n_pacchetti"))
pd_subset[,PrimaCol:ncol(pd_subset)]<-apply(pd_subset[,PrimaCol:ncol(pd_subset)],2,function(x)as.numeric(as.character(x)))
t<-aggregate(pd_subset[,PrimaCol:ncol(pd_subset)], by=list(pd_subset$visitorID), max)
names(t)[1]<-c("visitorID")
names(t)[2:ncol(t)]<-paste0("acquisto_",names(t)[2:ncol(t)])
cookies<-merge(cookies, t, by="visitorID", all.x=T)
t<-aggregate(pd_subset[,PrimaCol:ncol(pd_subset)], by=list(pd_subset$visitorID), sum)
names(t)[1]<-c("visitorID")
names(t)[2:ncol(t)]<-paste0("tot_acquisti_",names(t)[2:ncol(t)])
cookies<-merge(cookies, t, by="visitorID", all.x=T)

cookies[is.na(cookies$acquisto_n_pacchetti),"acquisto_n_pacchetti"]<-0
cookies[is.na(cookies$acquisto_PacchettoSkyTV),"acquisto_PacchettoSkyTV"]<-0
cookies[is.na(cookies$acquisto_PacchettoSkyGo),"acquisto_PacchettoSkyGo"]<-0
cookies[is.na(cookies$acquisto_PacchettoMySkyHD),"acquisto_PacchettoMySkyHD"]<-0
cookies[is.na(cookies$acquisto_PacchettoParabola),"acquisto_PacchettoParabola"]<-0
cookies[is.na(cookies$acquisto_PacchettoFamiglia),"acquisto_PacchettoFamiglia"]<-0
cookies[is.na(cookies$acquisto_PacchettoCalcio),"acquisto_PacchettoCalcio"]<-0
cookies[is.na(cookies$acquisto_PacchettoHD),"acquisto_PacchettoHD"]<-0
cookies[is.na(cookies$acquisto_PacchettoSkyHD),"acquisto_PacchettoSkyHD"]<-0
cookies[is.na(cookies$acquisto_PacchettoSport),"acquisto_PacchettoSport"]<-0
cookies[is.na(cookies$acquisto_PacchettoCinema),"acquisto_PacchettoCinema"]<-0
cookies[is.na(cookies$acquisto_PacchettoSky3D),"acquisto_PacchettoSky3D"]<-0

cookies[is.na(cookies$tot_acquisti_n_pacchetti),"tot_acquisti_n_pacchetti"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoSkyTV),"tot_acquisti_PacchettoSkyTV"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoSkyGo),"tot_acquisti_PacchettoSkyGo"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoMySkyHD),"tot_acquisti_PacchettoMySkyHD"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoParabola),"tot_acquisti_PacchettoParabola"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoFamiglia),"tot_acquisti_PacchettoFamiglia"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoCalcio),"tot_acquisti_PacchettoCalcio"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoHD),"tot_acquisti_PacchettoHD"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoSkyHD),"tot_acquisti_PacchettoSkyHD"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoSport),"tot_acquisti_PacchettoSport"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoCinema),"tot_acquisti_PacchettoCinema"]<-0
cookies[is.na(cookies$tot_acquisti_PacchettoSky3D),"tot_acquisti_PacchettoSky3D"]<-0



#variabile comportamento
cookies$soggetto<-"naviga"
cookies$soggetto[cookies$carrello==1]<-"carrello"
cookies$soggetto[cookies$compra==1]<-"compra"

cookies$soggetto_calcio<-"naviga"
cookies$soggetto_calcio[cookies$carrello==1 & cookies$PacchettoCalcio==1]<-"carrello"
cookies$soggetto_calcio[cookies$compra==1 & cookies$PacchettoCalcio==1]<-"compra"

cookies$soggetto_cinema<-"naviga"
cookies$soggetto_cinema[cookies$carrello==1 & cookies$PacchettoCinema==1]<-"carrello"
cookies$soggetto_cinema[cookies$compra==1 & cookies$PacchettoCinema==1]<-"compra"

cookies$soggetto_sport<-"naviga"
cookies$soggetto_sport[cookies$carrello==1 & cookies$PacchettoSport==1]<-"carrello"
cookies$soggetto_sport[cookies$compra==1 & cookies$PacchettoSport==1]<-"compra"

cookies$soggetto_famiglia<-"naviga"
cookies$soggetto_famiglia[cookies$carrello==1 & cookies$PacchettoFamiglia==1]<-"carrello"
cookies$soggetto_famiglia[cookies$compra==1 & cookies$PacchettoFamiglia==1]<-"compra"

cookies$soggetto_HD<-"naviga"
cookies$soggetto_HD[cookies$carrello==1 & cookies$PacchettoHD==1]<-"carrello"
cookies$soggetto_HD[cookies$compra==1 & cookies$PacchettoHD==1]<-"compra"



#days_before_purchase
pd$giorno<-as.Date(pd$giorno)
a<-pd[!is.na(pd$purchaseID),c("visitorID", "giorno","purchaseID", "products")]
purchase_day<-aggregate(a$giorno,by=list(a$visitorID), min)
names(purchase_day)<-c("visitorID", "purchase_day")
cookies<-merge(cookies, purchase_day, by="visitorID", all.x=T)
cookies$purchase_day<-as.Date(cookies$purchase_day)
cookies$days_before_purchase<-as.numeric(cookies$purchase_day-cookies$first_day)+1
#days_before_basket
pd$giorno<-as.Date(pd$giorno)
a<-pd[!is.na(pd$products),c("visitorID", "giorno","purchaseID", "products")]
basket_day<-aggregate(a$giorno,by=list(a$visitorID), min)
names(basket_day)<-c("visitorID", "basket_day")
cookies<-merge(cookies, basket_day, by="visitorID", all.x=T)
cookies$basket_day<-as.Date(cookies$basket_day)
cookies$days_before_basket<-as.numeric(cookies$basket_day-cookies$first_day)+1
#days_basket_purchase
cookies$days_basket_purchase<-as.numeric(cookies$purchase_day-cookies$basket_day)+1


rm(pd, pd_subset,t, carrelloID, vede_promoID, n_pacchetti, pacchetti, products, PacchettoCalcio, PacchettoCinema, PacchettoSport, purchase_day, basket_day,a)
save(file=paste0(dir_output,"/cookies"), cookies)

save(file=paste0(dir_output,"/cookies_checkpoint_prodotti"), cookies)
##------------------------------------------------------------------------------------
##--- LOG_SAMPLE
load(paste0(dir_input,"/",file_log))
log<-log_sample
rm(log_sample)
log<-log[,-1]

length(which(log$log!=""))
logID<-as.character((log$visitorID[which(log$log!="")]))
cookies[,"log"]<-0
cookies[cookies$visitorID%in%logID,"log"]<-1

save(file=paste0(dir_output,"/cookies"), cookies)
rm(log, logID)

##------------------------------------------------------------------------------------