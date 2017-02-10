load("sample/cookies")

names(cookies)

cookies$n_entry_pages_totali<-apply(cookies[,c("entry_assist_e_cont_clienti",
                                               "entry_commerciale",
                                               "entry_contenuti_generico",
                                               "entry_intrattenimento",
                                               "entry_meteo",
                                               "entry_news",
                                               "entry_nondef_home",
                                               "entry_sport")],1,sum)

var<-c(2:28,31,32,34,37:111,115,124,126,127)


co<-cookies
co[,var]<-apply(co[,var], 2, as.numeric)

co[,c(41:75)]<-apply(co[,c(41:75)],2, function(x, data=co) {x<-x/data$n_pagine_viste})
co[,c(76:111)]<-apply(co[,c(76:111)],2, function(x, data=co) {x<-x/data$n_entry_pages_totali})
co2<-co[,var]

a<-aggregate(co2, by=list(cookies$soggetto), mean)
row.names(a)<-a[,1]
a<-a[,-1]
a<-t(a)
a<-data.frame(a)

ab<-round(a,4)


boxplot(n_visite~soggetto, data=cookies, outline=F)
boxplot(n_visite~soggetto, data=cookies)
#frequenza dei compratori e carrellisti
names(cookies)

soggetti<-cookies$n_days_visite>=2
sum(soggetti)

tableFattaBene(cookies$soggetto[soggetti])
tableFattaBene(cookies$soggetto_sport[soggetti])
tableFattaBene(cookies$soggetto_calcio[soggetti])
tableFattaBene(cookies$soggetto_cinema[soggetti])
