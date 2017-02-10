#selezione brutale

load("sample2.1/cookies")

sel<-is.na(cookies$pages_AreaAssistenza)
sum(sel)


cookies1<-cookies[!sel,]


summary(cookies1)

names(cookies1)

#sistemazione a manina del campione
comm<-apply(cookies1[,c("pages_commerciale_calcio","pages_commerciale_sport","pages_commerciale_motori","pages_commerciale_skytv","pages_commerciale_cinema","pages_commerciale_famiglia","pages_commerciale_hd","pages_commerciale_tecnologiasky")],1,sum)
comm[1:100]
summary(comm)

a<-apply(cookies1[,c("pages_commerciale_calcio","pages_commerciale_sport","pages_commerciale_motori","pages_commerciale_skytv","pages_commerciale_cinema","pages_commerciale_famiglia","pages_commerciale_hd","pages_commerciale_tecnologiasky")],2,function(x)x/comm)
names(cookies1)

for (i in 260:267)
  cookies1[,i]<-cookies1[,i]/comm


summary(cookies1[,260:267])


b<-cookies[,291:ncol(cookies)]
names(cookies)
names(cookiesAA)


cookies2<-cbind(cookiesAA, b)


names(cookies2)

cookies<-cookies2
save(file="sample2.1/cookies_new", cookies)






cookies$soggetto_tecnologiasky<-"naviga"
cookies$soggetto_tecnologiasky[cookies$carrello==1 & (cookies$PacchettoMySkyHD==1 | cookies$PacchettoHD==1 | cookies$PacchettoSky3D==1 | cookies$PacchettoSkyHD==1 )]<-"carrello"
cookies$soggetto_tecnologiasky[cookies$compra==1 & (cookies$PacchettoMySkyHD==1 | cookies$PacchettoHD==1 | cookies$PacchettoSky3D==1 | cookies$PacchettoSkyHD==1
)]<-"compra"