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
ore_mattina<-c(9,10,11,12)
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
p_merge$vendor.mobile<-paste0(p_merge$vendor,".",p_merge$mobile)

cookies<-left_join(cookies, p_merge, by="visitorID")
rm(os,p_merge, dup)
rm(p)
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

# load(file=paste0(dir_output,"/cookies_checkpoint_pagine_2"))
#frequenza parole entro area
ricavaParole<-function(pages,paroleNO=NULL){ #questa funzione serve a estrarre tutte le parole presenti nel vettore character di pagine. Non usata al momento.
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
#######################################################################################################
#FUNZIONI UTILI ALLO SCOPO e valori utili per tutte sia per le pages che le entry pages
#######################################################################################################
generalivelli_e_url_stringa<-function(pages){
  pages<-as.character(pages)
  pages<-pages[!pages==""]
  url <- strsplit(pages,"http%3A//|http://") #tolgo http e https
  url <- sapply(url,function(x)x[2])
  url <- strsplit(url,"/") #tokenizzo l'url
  URL<-as.data.frame(pages)
  URL[,"l1"]<- sapply(url,function(x)x[1]) #trattengo il primo livello
  URL[,"l2"]<- sapply(url,function(x)x[2]) #trattengo il secondo livello
  URL[,"l3"]<- sapply(url,function(x)x[3]) #trattengo il terzo livello
  URL[,"l4"]<- sapply(url,function(x)x[4]) #trattengo il quarto livello
  
  URL$url_stringa<-url2character(URL$pages) #trasfoma l'url nella stringa character con ciascuna parola separata da spazio
  return(URL)
}
url2character<-function(pages){ #trasforma l'url in una stringa character di parole separate da spazio
  pages<-as.character(pages)
  require(stringr)
  require(tm)
  url <- strsplit(pages,"http%3A//|http://")
  url <- sapply(url,function(x)x[2])
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  return(url1) #per ogni url la stringa contenente le parole
}


aggiungi_aree_e_video_a_URL<-function(URL){   #aggiunge la variabile area all'url secondo queste regole
  #commerciale
  URL[	(URL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | 
         (URL$l1=="www.sky.it" & URL$l2%in%c("prodotti-sky","acquista","abbonarsi")),"area"]<-"AreaCommerciale"
  #cont_clienti
  URL[(URL$l1%in%c("guidatv.sky.it","skygo.sky.it","hotclub.sky.it")),"area"]<-"AreaCont_clienti"
  #assist_clienti
  URL[(URL$l1=="www.sky.it" & URL$l2%in%c("assistenza-e-supporto","area-clienti")) |
        (URL$l1%in% c("skyid.sky.it"))	
      ,"area"]<-"AreaAssistenza"
  #news
  URL[(URL$l1%in%c("tg24.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("news"))
      ,"area"]<-"AreaNews"
  #meteo
  URL[(URL$l1%in%c("meteo.sky.it"))
      ,"area"]<-"AreaMeteo"
  #altri_sport
  URL[	(URL$l1%in%c("sport.sky.it","liveblog.sport.sky.it")) |
         (URL$l1=="video.sky.it" & URL$l2%in%c("sport")) 
       ,"area"]<-"AreaSport_altro"
  #calcio
  URL[	((URL$l1%in%c("sport.sky.it") & URL$l2%in% c("sport") & URL$l3%in% c("calcio_italiano","calcio_estero","champions_league")) | 
          (URL$l1%in%c("sport.sky.it") & URL$l2%in% c("sport") & URL$l3%in% c("statistiche") & URL$l4 %in% c("calcio")) |
          (URL$l1%in%c("video.sky.it") & URL$l2%in% c("sport") & URL$l3%in% c("calcio-italiano","highlights-serie-a","highlights-serie-b","champions-league","europa-league","europei-calcio","calcio-estero","calciomercato","bundesliga","fox-sports")) |
          (URL$l1 %in% c("charitystars.sport.sky.it","fantascudetto.sky.it","fantacampioni.sky.it","fantamondiale.sky.it"))
  )
       ,"area"]<-"AreaCalcio"
  #motori
  URL[	((URL$l1%in%c("sport.sky.it") & URL$l2%in% c("sport") & URL$l3%in% c("formula1","motogp")) | 
          (URL$l1%in%c("video.sky.it") & URL$l2%in% c("sport") & URL$l3%in% c("formula1","motogp")) |
          (URL$l1 %in% c("fantagp.sky.it"))
  )
       ,"area"]<-"AreaMotori"
  #cinema
  URL[(URL$l1%in%c("cinema.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("cinema"))
      ,"area"]<-"AreaCinema"
  #serietv
  URL[(URL$l1%in%c("skyatlantic.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("skyatlantic"))
      ,"area"]<-"AreaSerietv"
  #programmitv
  URL[(URL$l1%in%c("skyuno.sky.it","theapprentice.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in% c("skyuno","theapprentice"))
      ,"area"]<-"AreaProgrammitvAltro"
  
  #gastronomia
  URL[(URL$l1%in%c("masterchef.sky.it",
                   "hellskitchen.sky.it","juniormasterchef.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in% c("xfactor","masterchef","hellskitchen"))
      ,"area"]<-"AreaProgrammitvGastronomia"
  #programmitv
  URL[(URL$l1%in%c("xfactor.sky.it","italiasgottalent.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in% c("xfactor", "italiasgottalent"))
      ,"area"]<-"AreaProgrammitvMusica"
  
  #intrattenimento_altro
  URL[(URL$l1%in%c("arte.sky.it","mag.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("mag"))
      ,"area"]<-"AreaIntrattenimento_altro"
  #oroscopo
  URL[(URL$l1%in%c("oroscopo.sky.it","forum.sky.it"))
      ,"area"]<-"AreaOroscopo"
  #forum
  URL[(URL$l1%in%c("forum.sky.it"))
      ,"area"]<-"AreaForum"
  #home
  URL[ (URL$l1%in%c("sky.it") & is.na(URL$l2)),"area"]<-"AreaHome"
  #non_definito
  URL[is.na(URL$area),"area"]<-"AreaResiduale"
  #creo ora una colonna per ogni area
  require(data.table)
  a <- data.table(URL$area,URL$pages)
  a <- a[,.N,by=list(V1,V2)]
  t<-tapply(a$N,list(as.factor(a$V2), as.factor(a$V1)), sum)
  t[is.na(t)]<-0
  t<-as.data.frame.matrix(t)
  t<-t  %>%
    add_rownames(var = "pages")
  t$pages<-as.character(t$pages)
  URL$pages<-as.character(URL$pages)
  URL<-left_join( URL, t, by = "pages")  
  
  #e poi aggiungo anche la colonna video
  
  URL[,"video"]<-0
  URL[URL$l1%in%c("video.sky.it"), "video"]<-1
  return(URL)
}

aggiungi_prefisso_alle_features<-function(URL,prefisso){ #serve a mettere pages_ o entry_ davanti al nome delle features create
  names(URL)[(which(names(URL)=="area")+1):length(names(URL))]<-as.character(sapply(
    names(URL)[(which(names(URL)=="area")+1):length(names(URL))],function(x)paste0(prefisso,x)))
  return(URL)
}

creaFeature<-function(area=NULL,pattern,data=URL,tuttoilresto=FALSE){ #va a imputare 1 all'elemento i.esimo del vettore a se è presente il pattern nella stringa
  pattern<-c(pattern)
  yes<-sapply(pattern, grepl, data$url_stringa, ignore.case=TRUE)
  yes<-as.data.frame(yes)
  yes<-apply(yes,1,any)
  if(tuttoilresto) yes<-!yes
  a<-rep(0,nrow(data))
  if(is.null(area)) a[yes]<-1
  else a[data$area%in%area & yes]<-1
  return(a)
}

aggiungi_features_semantiche_a_URL<-function(URL, lista.parole){
  URL<-URL
  #features semantiche
    #area commerciale
  parole_comm_calcio<-c("calcio",lista.parole$parole.calcio)
  URL$commerciale_calcio<-creaFeature(area="AreaCommerciale",parole_comm_calcio, data=URL)
  
  parole_comm_sport<-c("sport",lista.parole$parole.ciclismo,lista.parole$parole.basket,lista.parole$parole.tennis,lista.parole$parole.altrisport,lista.parole$parole.fantagiochi)
  URL$commerciale_sport<-creaFeature(area="AreaCommerciale",parole_comm_sport, data=URL)
  
  parole_comm_motori<-c("skymotori",lista.parole$parole.motogp, lista.parole$parole.formula1)
  URL$commerciale_motori<-creaFeature(area="AreaCommerciale",parole_comm_motori, data=URL)
  
  parole_comm_skytv<-c("tv",lista.parole$parole.programmitv,lista.parole$parole.gossip)
  URL$commerciale_skytv<-creaFeature(area="AreaCommerciale",parole_comm_skytv, data=URL)
  
  parole_comm_cinema<-c("cinema",lista.parole$parole.cinema)
  URL$commerciale_cinema<-creaFeature(area="AreaCommerciale",parole_comm_cinema, data=URL)
  
  parole_comm_famiglia<-c("famiglia")
  URL$commerciale_famiglia<-creaFeature(area="AreaCommerciale",parole_comm_famiglia, data=URL)
  parole_comm_hd<-c("hd")
  URL$commerciale_hd<-creaFeature(area="AreaCommerciale",parole_comm_hd, data=URL)
  
  parole_comm_tecnologiasky<-c("multivision","link","go","demand","primafila","3d")
  URL$commerciale_tecnologiasky<-creaFeature(area="AreaCommerciale",parole_comm_tecnologiasky, data=URL)
  #area news
  URL$cronaca<-creaFeature(area=NULL,lista.parole$parole.cronaca, data=URL)
  URL$mondo<-creaFeature(area=NULL,lista.parole$parole.mondo, data=URL)
  URL$politica<-creaFeature(area=NULL,lista.parole$parole.politica, data=URL)
  URL$economia<-creaFeature(area=NULL,lista.parole$parole.economia, data=URL)
  #area meteo
  URL$previsioni_we<-creaFeature("AreaMeteo",lista.parole$parole.previsioni_we, data=URL)
  URL$previsioniNord<-creaFeature("AreaMeteo",lista.parole$parole.previsioniNord, data=URL)
  URL$previsioniCentro<-creaFeature("AreaMeteo",lista.parole$parole.previsioniCentro, data=URL)
  URL$previsioniSud<-creaFeature("AreaMeteo",lista.parole$parole.previsioniSud, data=URL)
  URL$previsioni_villeggiatura<-creaFeature("AreaMeteo",lista.parole$parole.previsioni_villeggiatura, data=URL)
  #area sport
  URL$calcio<-creaFeature(area=NULL,lista.parole$parole.calcio, data=URL)
  URL$formula1<-creaFeature(area=NULL,lista.parole$parole.formula1, data=URL)
  URL$motogp<-creaFeature(area=NULL,lista.parole$parole.motogp, data=URL)
  URL$ciclismo<-creaFeature(area=NULL,lista.parole$parole.ciclismo, data=URL)
  URL$basket<-creaFeature(area=NULL,lista.parole$parole.basket, data=URL)
  URL$tennis<-creaFeature(area=NULL,lista.parole$parole.tennis, data=URL)
  URL$altrisport<-creaFeature(area=NULL, lista.parole$parole.altrisport, data=URL)
  URL$fantagiochi<-creaFeature(area=NULL,lista.parole$parole.fantagiochi, data=URL) 
  #area intrattenimento
  URL$programmitv<-creaFeature(area=NULL,lista.parole$parole.programmitv, data=URL)
  URL$cinema<-creaFeature(area=NULL,lista.parole$parole.cinema, data=URL)
  URL$gossip<-creaFeature(area=NULL,lista.parole$parole.gossip, data=URL)
  #area contenuti generico
  URL$problemi_tecnici<-creaFeature("AreaForum",lista.parole$parole.problemi_tecnici, data=URL)
  parole.tutte<-as.character(unlist(lista.parole))
  URL$tuttoilresto<-creaFeature(area=NULL,parole.tutte, data=URL, tuttoilresto=TRUE)
  return(URL)
}

leva_le_features_in_eccesso<-function(URL){
  URL<-URL  %>%
    select(-(l1:area))
  return(URL)
}
#######################################################################################################
#parole che deteminano la features semantica
#######################################################################################################
lista.parole<-list()
lista.parole$parole.cronaca<-c("cronaca","yara", "processo", "allagamenti","scampia","omicidio", "giglio","carcere","camorra","mafia", "bossetti", "naufragio", "esondazione", "nubifragio", "prostituzione","droga", "ndrangheta", "corruzione")
lista.parole$parole.mondo<-c("gaza", "israele","raid", "iraq", "hamas", "obama", "merkel", "nassiriya","afghanistan","libia","clinton", "putin","internazionale", "isrealiani", "isis", "attentato")
lista.parole$parole.politica<-c("politica", "renzi", "berlusconi","ruby", "elezioni", "senato", "repubblica","governo", "bunga", "grillo", "m5s", "riforma","riforme","ministri","sindaco","elettorale","migranti", "minetti", "parlamento")
lista.parole$parole.economia<-c("economia","bce", "fed", "alitalia","esuberi", "sciopero", "spread", "economico", "pil", "dollaro", "inps", "eurozona", "eurogruppo")
lista.parole$parole.previsioni_we <-c("sab","dom")
lista.parole$parole.previsioniNord <-c("piemonte","aosta","lombardia","veneto","friuli","trentino","adige","emilia","liguria")
lista.parole$parole.previsioniCentro <-c("toscana","umbria","lazio","marche","friuli","trentino","adige","emilia")
lista.parole$parole.previsioniSud <-c("campania","basilicata","puglia","molise","calabria","sicilia","sardegna")
lista.parole$parole.previsioni_villeggiatura<-c("montagna","mare")
lista.parole$parole.calcio<-c("calcio","calciomercato","champions","gol","fantascudetto","inter", "milan","juventus","calciatori","juve","Calciomercato","balotelli","bayern","sampdoria","bundesliga","mourinho", "ibrahimovic","totti","atalanta","kaka","goleador","hellas", "milanello", "fifa")
lista.parole$parole.formula1<-c("formula","motori","ferrari","vettel","alonso","silverstone","hamilton","schumacher")
lista.parole$parole.motogp<-c("motogp","motori","moto","motomondiale")
lista.parole$parole.ciclismo<-c("ciclismo","tour","france","tappa")
lista.parole$parole.basket<-c("nba","basket","lakers", "bulls")
lista.parole$parole.tennis<-c("tennis", "wimbledon")
lista.parole$parole.altrisport<-c("olimpiadi","rugby","nuoto","pallanuoto","volley","scherma","golf", "baseball")
lista.parole$parole.fantagiochi<-c("fantascudetto","fantamondiale","fantacampioni","fantagp")
lista.parole$parole.programmitv<-c("skyuno","masterchef","xfactor","hellskitchen","juniormasterchef", "theapprentice", "apprentice")
lista.parole$parole.cinema<-c("cinema","film","commedia","drammatico","thriller","horror", "oscar", "trameFilm")
lista.parole$parole.gossip <-c("mag","gossip","sexy","style","modelle", "spettacolo","festival","concerto","middelton","armani","bullock", "beyonce", "arcuri", "bieber", "playboy")
lista.parole$parole.problemi_tecnici<-c("decoder","abbonamento","mysky","demand", "installazione", "multivision", "contratto", "disdetta", "skygo", "skylink")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#lavoro sulle PAGES
pages<-pg %>% 
  distinct(pages)  %>%
  select(pages) 
pages<-pages[pages!=""]
#genera i livelli dall'url e la stringa character dell'url
URL<-generalivelli_e_url_stringa(pages)
#aggiungo features aree e video
URL<-aggiungi_aree_e_video_a_URL(URL)
#aggiungo features semantiche
URL<-aggiungi_features_semantiche_a_URL(URL, lista.parole)  
#aggiungo la parola pages_ a tutte le features create
URL<-aggiungi_prefisso_alle_features(URL, prefisso="pages_")
#levo le features da l1 a area
URL<-leva_le_features_in_eccesso(URL)

save(file=paste0(dir_output,"/URL_checkpoint"), URL)

pg<-left_join(pg, URL, by="pages")

#lavoro sulle ENTRY_PAGES
entry_pages<-pg %>% 
  distinct(entry_pages)  %>%
  select(entry_pages) 
entry_pages<-entry_pages[entry_pages!=""]
#genera i livelli dall'url e la stringa character dell'url
entryURL<-generalivelli_e_url_stringa(entry_pages)
#aggiungo features aree e video
entryURL<-aggiungi_aree_e_video_a_URL(entryURL)
#aggiungo features semantiche
entryURL<-aggiungi_features_semantiche_a_URL(entryURL, lista.parole)  
#aggiungo la parola pages_ a tutte le features create
entryURL<-aggiungi_prefisso_alle_features(entryURL, prefisso="entry_")
#levo le features da l1 a area
entryURL<-leva_le_features_in_eccesso(entryURL)
#rinomino la prima variabile in entry_pages
names(entryURL)[1]<-"entry_pages"

save(file=paste0(dir_output,"/entryURL_checkpoint"), entryURL)

pg<-left_join(pg, entryURL, by="entry_pages")

PrimaCol<-which(names(pg)=="pages_AreaAssistenza")
t<-aggregate(pg[PrimaCol:ncol(pg)], by=list(factor(pg$visitorID)), sum)
names(t)[1]<-"visitorID"
t$visitorID<-as.character(t$visitorID)
t[is.na(t)]<-0
cookies<-left_join(cookies,t, by="visitorID")
                
var_entry_Area<-names(cookies)[sapply(names(cookies), grepl, pattern="entry_Area")]          
#calcolo numero totale di entry pages
cookies$n_entry_pages_totali<-apply(cookies[,var_entry_Area],1,sum)
cookies$n_entry_pages_totali[cookies$n_entry_pages_totali==0]<-1
                

save(file=paste0(dir_output,"/cookies_checkpoint_pagine_3"), cookies)
#devo normalizzare! 
#pages
var_pages<-names(cookies)[sapply(names(cookies), grepl, pattern="pages_")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="entry")]#tolgo n_entry_pages_totali
var_pages_commerciale<-var_pages[sapply(var_pages, grepl, pattern="commerciale")]
var_pages<-var_pages[!sapply(var_pages, grepl, pattern="commerciale")]
#divido le variabili pages per n_pagine_viste
cookies$n_pagine_viste[cookies$n_pagine_viste==0]<-1
cookies[,var_pages]<-apply(cookies[,var_pages],2,function(x, y=cookies) x/y$n_pagine_viste)
#divido le variabili pages_commerciale per pages_AreaCommerciale
cookies[cookies$pages_AreaCommerciale!=0,var_pages_commerciale]<-apply(cookies[cookies$pages_AreaCommerciale!=0,var_pages_commerciale],2,function(x, y=cookies) x/y$pages_AreaCommerciale[cookies$pages_AreaCommerciale!=0])
#entry
var_entry<-names(cookies)[sapply(names(cookies), grepl, pattern="entry_")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="n_")]#tolgo n_entry_pages_totali
var_entry_commerciale<-var_entry[sapply(var_entry, grepl, pattern="commerciale")]
var_entry<-var_entry[!sapply(var_entry, grepl, pattern="commerciale")]
#divido le variabili entry per n_entry_pages_totali
cookies[,var_entry]<-apply(cookies[,var_entry],2,function(x, y=cookies) x/y$n_entry_pages_totali)
#divido le variabili entry_commerciale per entry_AreaCommerciale
cookies[cookies$entry_AreaCommerciale!=0,var_entry_commerciale]<-apply(cookies[cookies$entry_AreaCommerciale!=0,var_entry_commerciale],2,function(x, y=cookies) x/y$entry_AreaCommerciale[cookies$entry_AreaCommerciale!=0])   
   
rm(var_entry,var_entry_commerciale,var_pages_commerciale,var_pages, var_entry_Area)
rm(pg)
save(file=paste0(dir_output,"/cookies_checkpoint_pagine_4"), cookies)
            
rm(pg,t, URL,entryURL,pages, entry_pages, lista.parole)
                
                
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