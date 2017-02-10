################################################################################################
#FUNZIONI UTILI ALLO SCOPO e valori utili per tutte sia per le pages che le entry pages
################################################################################################

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
	URL[  (URL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | 
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
################################################################################################