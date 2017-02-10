library(magrittr)
library(dplyr)
library(data.table)
library(stringr)
library(tm)
#dati di input
dir_input="sample2"
dir_output="sample2.1"
file_visite<-"visite_sample"
file_piattaforma<-"piattaforma_sample"
file_pagine<-"pagine_sample"
file_prodotti<-"prodotti_sample"
file_log<-"log_sample"


##---------------------------------------------------------------------
load(paste0(dir_input,"/",file_visite))
v<-visite_sample
# v<-v[,-1]
rm(visite_sample)

cookies<- v %>% 
  distinct(visitorID)  %>%
  select(visitorID)

cookies$visitorID<-as.character(cookies$visitorID)

ore_prelavoro<-c(6,7,8)
ore_mattina<-c(10,11,12)
ore_pranzo<-c(13,14)
ore_pome<-c(15,16,17)
ore_postlavoro<-c(18,19,20)
ore_sera<-c(21,22,23,0)
ore_notte<-c(1,2,3,4,5)
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
            "p_visite_nigh"
)
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



#p_visite_fascia_wd_sab_dom
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
v[v$we==0 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_wdSabDom"]<-"0_prelavoro_wd"
v[v$we==0 & v$fascia_oraria=="1_morning", "fascia_oraria_wdSabDom"]<-"1_morning_wd"
v[v$we==0 & v$fascia_oraria=="2_lunch", "fascia_oraria_wdSabDom"]<-"1_lunch_wd"
v[v$we==0 & v$fascia_oraria=="3_afternoon", "fascia_oraria_wdSabDom"]<-"3_afternoon_wd"
v[v$we==0 & v$fascia_oraria=="4_postlavoro", "fascia_oraria_wdSabDom"]<-"4_postlavoro_wd"
v[v$we==0 & v$fascia_oraria=="5_evening", "fascia_oraria_wdSabDom"]<-"5_evening_wd"
v[v$we==0 & v$fascia_oraria=="6_night", "fascia_oraria_wdSabDom"]<-"6_night_wd"
a <- data.table(v$fascia_oraria_wdSabDom,v$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
names(t)<-c("p_visite_prelav_dom",
            "p_visite_prelav_sab",
            "p_visite_prelav_wd",
            "p_visite_morn_dom",
            "p_visite_morn_sab",
            "p_visite_morn_wd",
            "p_visite_lunc_dom",
            "p_visite_lunc_sab",
            "p_visite_lunc_wd",
            "p_visite_afte_dom",
            "p_visite_afte_sab",
            "p_visite_afte_wd",
            "p_visite_postlav_dom",
            "p_visite_postlav_sab",
            "p_visite_postlav_wd",          
            "p_visite_even_dom",
            "p_visite_even_sab",
            "p_visite_even_wd",
            "p_visite_nigh_dom",
            "p_visite_nigh_sab",
            "p_visite_nigh_wd")
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
rm(v)
save(file=paste0(dir_output,"/cookies"), cookies)


##------------------------------------------------------------------------------------
##--- PIATTAFORMA_SAMPLE
load(paste0(dir_input,"/",file_piattaforma))
p<-piattaforma_sample
# p<-p[,-1]
rm(piattaforma_sample)
os<-read.csv2("os2.csv")
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
cookies<-left_join(cookies, p_merge, by="visitorID")
rm(os,p_merge, dup)
rm(p)
save(file=paste0(dir_output,"/cookies"), cookies)



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


#numero di pagine viste dal cookie al giorno min max mean me q1 q3
a <- data.table(pg$giorno,pg$visitorID)
a <- a[,.N,by=list(V1,V2)]
t<-a %>%
    group_by(V2) %>%
    summarize(min_pages_giorno=mean(N),
              q1_pages_giorno=quantile(N, 0.25),
              mean_pages_giorno=mean(N),
              median_pages_giorno=quantile(N, 0.5),
              q3_pages_giorno=quantile(N, 0.75),
              max_pages_giorno=max(N))
t<-as.data.frame(t)
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
#lavoro sulle PAGES
pages<-pg %>% 
  distinct(pages)  %>%
  select(pages) 
# t<-table(pg$pages)
pages<-pages[pages!=""]
# t<-sort(t, decreasing=T)
# pages_SKY<-data.frame(cbind(pages=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2)))
# write.csv2(pages_SKY, file="output/pages_SKY.csv", row.names=F)
# rm(t, pages_SKY)
#frequenza parole entro area
ricavaParole<-function(pages,paroleNO=NULL){
  require(stringr)
  require(tm)
  url <- strsplit(pages,"http%3A//|http://")
  #   url[[1]]
  url <- sapply(url,function(x)x[2])
  #   url[[1]]
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  #   url1[[1]] #per ogni url la stringa contenente le parole
  #   url1[[1]]
  parole<-unlist(strsplit(url1," "))
  #   parole
  parole<-parole[!parole%in%c(stopwords("italian"),"htm","html","vid","sky","it","shtml","php","pls","","www",letters,NA, "sh", "sht",paroleNO)]
  parole
  return(parole)
}
#PAGES SECONDO NUOVO APPROCCIO
url <- strsplit(pages,"http%3A//|http://")
# url[[1]]
url <- sapply(url,function(x)x[2])
# url[[1]]
url <- strsplit(url,"/")
# url[[1]]
URL<-as.data.frame(pages)
URL[,"l1"]<- sapply(url,function(x)x[1])
URL[,"l2"]<- sapply(url,function(x)x[2])
#commerciale
URL[(URL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | (URL$l1=="www.sky.it" & URL$l2%in%c("prodotti-sky","acquista","abbonarsi")),"area"]<-"pages_commerciale"
#cont_clienti
URL[(URL$l1%in%c("guidatv.sky.it","skygo.sky.it","skyid.sky.it","hotclub.sky.it")),"area"]<-"pages_cont_clienti"
#assist_clienti
URL[(URL$l1=="www.sky.it" & URL$l2%in%c("assistenza-e-supporto","area-clienti")),"area"]<-"pages_assist"
#news
URL[(URL$l1%in%c("tg24.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("news"))
    ,"area"]<-"pages_news"
#meteo
URL[(URL$l1%in%c("meteo.sky.it"))
    ,"area"]<-"pages_meteo"
#sport
URL[(URL$l1%in%c("sport.sky.it","fantascudetto.sky.it","liveblog.sport.sky.it",
                 "fantascudetto.com","fantascudetto.net","fantacampioni.sky.it",
                 "charitystars.sport.sky.it", "fantamondiale.sky.it", "fantagp.sky.it",
                 "liveblog.sport.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("sport"))
    ,"area"]<-"pages_sport"
#intrattenimento
URL[(URL$l1%in%c("arte.sky.it","mag.sky.it","skyatlantic.sky.it","skyuno.sky.it",
                 "cinema.sky.it","xfactor.sky.it","masterchef.sky.it",
                 "hellskitchen.sky.it","juniormasterchef.sky.it",
                 "theapprentice.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("mag","skyatlantic","skyuno","cinema","xfactor","masterchef","hellskitchen","theapprentice"))
    ,"area"]<-"pages_intrattenimento"
#contenuti_generico
URL[(URL$l1%in%c("oroscopo.sky.it","forum.sky.it"))
    ,"area"]<-"pages_contenuti_generico"
#home
URL[ (URL$l1%in%c("sky.it") & is.na(URL$l2),"area"]<-"pages_home"
#non_definito
URL[is.na(URL$area),"area"]<-"pages_nondef"

#creazione delle relative features:
#elenco features:
url2character<-function(pages){
  require(stringr)
  require(tm)
  url <- strsplit(pages,"http%3A//|http://")
  url <- sapply(url,function(x)x[2])
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  return(url1) #per ogni url la stringa contenente le parole
}

URL$pages<-as.character(URL$pages)
URL$url_stringa<-url2character(URL$pages)
creaFeature<-function(area,pattern,data=URL){
  pattern<-c(pattern)
  yes<-sapply(pattern, grepl, data$url_stringa, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  a<-rep(0,nrow(data))
  a[data$area%in%area & yes]<-1
  return(a)
}
#contenuto video
URL[,"pages_video"]<-0
URL[URL$l1%in%c("video.sky.it"), "pages_video"]<-1
#features di area
a <- data.table(URL$area,URL$pages)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)]<-0
t<-as.table(t)
t<-as.data.frame.matrix(t)
t<-t  %>%
  add_rownames(var = "pages")
URL<-left_join( URL, t, by = "pages")  
rm(t,a)

#area commerciale
URL$pages_acquisto_cinema<-creaFeature("pages_commerciale","cinema")
URL$pages_acquisto_calcio<-creaFeature("pages_commerciale","calcio")
URL$pages_acquisto_soloperte<-creaFeature("pages_commerciale","soloperte")
#area assistenza e clienti
URL$pages_visione_sport<-creaFeature("pages_cont_clienti",c("sport","calcio"))
URL$pages_visione_cinema<-creaFeature("pages_cont_clienti",c("cinema","film","commedia","drammatico","azione","thriller"))
URL$pages_visione_serie<-creaFeature("pages_cont_clienti",c("serie","film"))
#area news
URL$pages_cronaca<-creaFeature("pages_news","cronaca")
URL$pages_mondo<-creaFeature("pages_news","mondo")
URL$pages_politica<-creaFeature("pages_news","politica")
URL$pages_economia<-creaFeature("pages_news","economia")
URL$pages_spettacolo<-creaFeature("pages_news","spettacolo")
#area meteo
URL$pages_previsioni_we<-creaFeature("pages_meteo",c("sab","dom"))
URL$pages_previsioniNord<-creaFeature("pages_meteo",c("piemonte","aosta","lombardia","veneto","friuli","trentino","adige","emilia","liguria"))
URL$pages_previsioniCentro<-creaFeature("pages_meteo",c("toscana","umbria","lazio","marche","friuli","trentino","adige","emilia"))
URL$pages_previsioniSud<-creaFeature("pages_meteo",c("campania","basilicata","puglia","molise","calabria","sicilia","sardegna"))
URL$pages_previsioni_villeggiatura<-creaFeature("pages_meteo",c("montagna","mare"))
#area sport
URL$pages_calcio<-creaFeature("pages_sport",c("calcio","italiano","calciomercato","mondiale","estero","champions","gol","fantascudetto","inter", "milan","juventus","real"))
URL$pages_formula1<-creaFeature("pages_sport",c("formula","motori"))
URL$pages_motogp<-creaFeature("pages_sport",c("motogp","motori","moto"))
URL$pages_ciclismo<-creaFeature("pages_sport",c("ciclismo","tour","france"))
URL$pages_basket<-creaFeature("pages_sport",c("nba"))
URL$pages_fanta<-creaFeature("pages_sport",c("fantascudetto","fantamondiale","fantacampioni","fantagp")) 
#area intrattenimento
URL$pages_programmitv<-creaFeature("pages_intrattenimento",c("masterchef","xfactor","serie","hellskitchen","juniormasterchef"))
URL$pages_cinema<-creaFeature("pages_intrattenimento",c("cinema","film", "trailer"))
URL$pages_gossip<-creaFeature("pages_intrattenimento",c("gossip","sexy","style","life","calendario"))
#area contenuti generico
URL$pages_forum_calcio<-creaFeature("pages_contenuti_generico",c("milan","calciomercato","calcio","juventus","inter", "champions"))
URL$pages_oroscopo<-creaFeature("pages_contenuti_generico",c("oroscopo"))
URL$pages_problemi_tecnici<-creaFeature("pages_contenuti_generico",c("decoder","abbonamento","mysky"))

URL$pages<-as.character(URL$pages)
pg1<-left_join(pg, URL, by="pages")
pg1$visitorID<-factor(pg1$visitorID)

#features di sotto-area
t<-aggregate(pg1[11:ncol(pg1)], by=list(pg1$visitorID), sum)
names(t)[1]<-"visitorID"
t$visitorID<-as.character(t$visitorID)
cookies<-left_join(cookies,t, by="visitorID")

rm(pages, pg1, t, URL, url)
save(file=paste0(dir_output,"/cookies"), cookies)

###################################################################
#lavoro sulle ENTRY_PAGES
entry_pages<-pg %>% 
  distinct(entry_pages)  %>%
  select(entry_pages)
entry_pages<-entry_pages[entry_pages!=""]
#Entry_pages SECONDO NUOVO APPROCCIO
entryurl <- strsplit(entry_pages,"http%3A//|http://")
# entryurl[[1]]
entryurl <- sapply(entryurl,function(x)x[2])
# entryurl[[1]]
entryurl <- strsplit(entryurl,"/")
# entryurl[[1]]
entryURL<-as.data.frame(entry_pages)
entryURL[,"l1"]<- sapply(entryurl,function(x)x[1])
entryURL[,"l2"]<- sapply(entryurl,function(x)x[2])
#commerciale
entryURL[(entryURL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | (entryURL$l1=="www.sky.it" & entryURL$l2%in%c("prodotti-sky","acquista","abbonarsi")),"area"]<-"entry_commerciale"
#cont_clienti
entryURL[(entryURL$l1%in%c("guidatv.sky.it","skygo.sky.it","skyid.sky.it","hotclub.sky.it")),"area"]<-"entry_cont_clienti"
#assist_clienti
entryURL[(entryURL$l1=="www.sky.it" & entryURL$l2%in%c("assistenza-e-supporto","area-clienti")),"area"]<-"entry_assist"
#news
entryURL[(entryURL$l1%in%c("tg24.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("news"))
    ,"area"]<-"entry_news"
#meteo
entryURL[(entryURL$l1%in%c("meteo.sky.it"))
    ,"area"]<-"entry_meteo"
#sport
entryURL[(entryURL$l1%in%c("sport.sky.it","fantascudetto.sky.it","liveblog.sport.sky.it",
                 "fantascudetto.com","fantascudetto.net","fantacampioni.sky.it",
                 "charitystars.sport.sky.it", "fantamondiale.sky.it","fantagp.sky.it",
                 "liveblog.sport.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("sport"))
    ,"area"]<-"entry_sport"
#intrattenimento
entryURL[(entryURL$l1%in%c("arte.sky.it","mag.sky.it","skyatlantic.sky.it","skyuno.sky.it",
                 "cinema.sky.it","xfactor.sky.it","masterchef.sky.it",
                 "hellskitchen.sky.it","juniormasterchef.sky.it",
                 "theapprentice.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("mag","skyatlantic","skyuno","cinema","xfactor","masterchef","hellskitchen","theapprentice"))
    ,"area"]<-"entry_intrattenimento"
#contenuti_generico
entryURL[(entryURL$l1%in%c("oroscopo.sky.it","forum.sky.it"))
    ,"area"]<-"entry_contenuti_generico"
#home
entryURL[ ((entryURL$l1%in%c("www.sky.it") & is.na(entryURL$l2))),"area"]<-"entry_home"
#non_definito
entryURL[is.na(entryURL$area),"area"]<-"entry_nondef"

#creazione delle relative features:
#elenco features:
url2character<-function(pages){
  library(stringr)
  library(tm)
  url <- strsplit(pages,"http%3A//|http://")
  url <- sapply(url,function(x)x[2])
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  return(url1) #per ogni url la stringa contenente le parole
}

entryURL$entry_pages<-as.character(entryURL$entry_pages)
entryURL$url_stringa<-url2character(entryURL$entry_pages)
creaFeature<-function(area,pattern,data=entryURL){
  pattern<-c(pattern)
  yes<-sapply(pattern, grepl, data$url_stringa, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  a<-rep(0,nrow(data))
  a[data$area%in%area & yes]<-1
  return(a)
}
#contenuto video
entryURL[,"entry_video"]<-0
entryURL[entryURL$l1%in%c("video.sky.it"), "entry_video"]<-1
#features di area
a <- data.table(entryURL$area,entryURL$entry_pages)
a <- a[,.N,by=list(V1,V2)]
t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
t[is.na(t)] <- 0
t<-as.table(t)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
t<-t  %>%
  add_rownames(var = "entry_pages")
entryURL<-left_join( entryURL, t, by = "entry_pages")  
rm(t,a)
#area commerciale
entryURL$entry_acquisto_cinema<-creaFeature("entry_commerciale","cinema")
entryURL$entry_acquisto_calcio<-creaFeature("entry_commerciale","calcio")
entryURL$entry_acquisto_soloperte<-creaFeature("entry_commerciale","soloperte")
#area assistenza e clienti
entryURL$entry_visione_sport<-creaFeature("entry_cont_clienti",c("sport","calcio"))
entryURL$entry_visione_cinema<-creaFeature("entry_cont_clienti",c("cinema","film","commedia","drammatico","azione","thriller"))
entryURL$entry_visione_serie<-creaFeature("entry_cont_clienti",c("serie"))
#area news
entryURL$entry_cronaca<-creaFeature("entry_news","cronaca")
entryURL$entry_mondo<-creaFeature("entry_news","mondo")
entryURL$entry_politica<-creaFeature("entry_news","politica")
entryURL$entry_economia<-creaFeature("entry_news","economia")
entryURL$entry_spettacolo<-creaFeature("entry_news","spettacolo")
#area meteo
entryURL$entry_previsioni_we<-creaFeature("entry_meteo",c("sab","dom"))
entryURL$entry_previsioniNord<-creaFeature("entry_meteo",c("piemonte","aosta","lombardia","veneto","friuli","trentino","adige","emilia","liguria"))
entryURL$entry_previsioniCentro<-creaFeature("entry_meteo",c("toscana","umbria","lazio","marche","friuli","trentino","adige","emilia"))
entryURL$entry_previsioniSud<-creaFeature("entry_meteo",c("campania","basilicata","puglia","molise","calabria","sicilia","sardegna"))
entryURL$entry_previsioni_villeggiatura<-creaFeature("entry_meteo",c("montagna","mare"))
#area sport
entryURL$entry_calcio<-creaFeature("entry_sport",c("calcio","italiano","calciomercato","mondiale","estero","champions","gol","inter", "milan","juventus","real"))
entryURL$entry_formula1<-creaFeature("entry_sport",c("formula","motori"))
entryURL$entry_motogp<-creaFeature("entry_sport",c("motogp","motori","moto"))
entryURL$entry_ciclismo<-creaFeature("entry_sport",c("ciclismo","tour","france"))
entryURL$entry_basket<-creaFeature("entry_sport",c("nba"))
entryURL$entry_fanta<-creaFeature("entry_sport",c("fantascudetto","fantamondiale","fantacampioni","fantagp")) 
#area intrattenimento
entryURL$entry_programmitv<-creaFeature("entry_intrattenimento",c("masterchef","xfactor","serie"))
entryURL$entry_cinema<-creaFeature("entry_intrattenimento",c("cinema","film"))
entryURL$entry_gossip<-creaFeature("entry_intrattenimento",c("gossip","sexy","style","life","calendario"))
#area contenuti generico
entryURL$entry_forum_calcio<-creaFeature("entry_contenuti_generico",c("milan","calciomercato","calcio","juventus"))
entryURL$entry_oroscopo<-creaFeature("entry_contenuti_generico",c("oroscopo"))
entryURL$entry_problemi_tecnici<-creaFeature("entry_contenuti_generico",c("decoder","abbonamento","mysky"))

entryURL$entry_pages<-as.character(entryURL$entry_pages)
pg2<-left_join(pg, entryURL, by="entry_pages")
pg2$visitorID<-factor(pg2$visitorID)
#features di sotto-area
t<-aggregate(pg2[11:ncol(pg2)], by=list(pg2$visitorID), sum)
names(t)[1]<-"visitorID"
t$visitorID<-as.character(t$visitorID)
cookies<-left_join(cookies,t, by="visitorID")
rm(pg2,t, entryURL, entry_pages, entryurl)
#numero totale di entry pages
cookies$n_entry_pages_totali<-apply(cookies[,c(  "entry_assist",
                        "entry_cont_clienti",
                        "entry_commerciale",
                        "entry_contenuti_generico",
                        "entry_intrattenimento",
                        "entry_meteo",
                        "entry_news",
                        "entry_home",
                        "entry_sport",
                        "entry_nondef")],1,sum)

rm(pg)
save(file=paste0(dir_output,"/cookies"), cookies)

rm(pg,pg1,pg2,t, url, URL,entryURL, entry_pages, entryurl)


##------------------------------------------------------------------------------------
##--- PRODOTTI_SAMPLE
load(paste0(dir_input,"/",file_prodotti))
pd<-prodotti_sample
pd<-pd[,-1]
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
carrelloID<-as.character((pd$visitorID[which(pd$products!="")]))
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
pd_subset<-pd[which(pd$visitorID%in%carrelloID),]
names(pd_subset)
pd_subset[,9:ncol(pd_subset)]<-apply(pd_subset[,9:ncol(pd_subset)],2,function(x)as.numeric(as.character(x)))
t<-aggregate(pd_subset[,9:ncol(pd_subset)], by=list(pd_subset$visitorID), max)
names(t)[1]<-c("visitorID")
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
