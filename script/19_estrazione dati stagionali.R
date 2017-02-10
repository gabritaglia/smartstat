#scopo è spilttare le 5 tabelle semestrali per creare dei subset stagionali da mettere in pasto alla funzione delle features.
#Il subset annuale invece verrà fatto come rbind dei subset semestrali

# dir.create("Sample_stagioni")
##########################################
##			ANNO 2013
##########################################
ANNO<-2013
#-------------------------------------------------------
#				PRIMO SEMESTRE
#-------------------------------------------------------
dir_input="Sample_2013_1sem/dati_ordinati"


file_visite<-"sample2013.1_visite"
file_piattaforma<-"sample2013.1_piattaforma"
file_pagine<-"sample2013.1_pagine"
file_prodotti<-"sample2013.1_prodotti"
file_log<-"sample2013.1_log"

#---------------------------------------------------------------
#STAGIONE_A GENNAIO-FEBBRAIO SKY-FASTWEB
data_inizio<-"2013-01-01"
data_fine<-"2013-02-28"
dir_output="Sample_stagioni/GenFeb2013"
suffisso_nome_file<-paste0("_stagione_GenFeb",ANNO)
	#visite
	load(file=paste0(dir_input,"/",file_visite,"_ordinato"))
	condizione<-(visite$giorno>=as.Date(data_inizio) & visite$giorno<=as.Date(data_fine))
	visite_stagione_GenFeb<-visite[condizione,]
	save(visite_stagione_GenFeb,file=paste0(dir_output,"/","visite",suffisso_nome_file))
	#piattaforma
	load(file=paste0(dir_input,"/",file_piattaforma,"_ordinato"))
	condizione<-(piattaforma$giorno>=as.Date(data_inizio) & piattaforma$giorno<=as.Date(data_fine))
	piattaforma_stagione_GenFeb<-piattaforma[condizione,]
	save(piattaforma_stagione_GenFeb,file=paste0(dir_output,"/","piattaforma",suffisso_nome_file))
	#pagine
	load(file=paste0(dir_input,"/",file_pagine,"_ordinato"))
	condizione<-(pagine$giorno>=as.Date(data_inizio) & pagine$giorno<=as.Date(data_fine))
	pagine_stagione_GenFeb<-pagine[condizione,]
	save(pagine_stagione_GenFeb,file=paste0(dir_output,"/","pagine",suffisso_nome_file))					 
	#prodotti
	load(file=paste0(dir_input,"/",file_prodotti,"_ordinato"))
	condizione<-(prodotti$giorno>=as.Date(data_inizio) & prodotti$giorno<=as.Date(data_fine))
	prodotti_stagione_GenFeb<-prodotti[condizione,]
	save(prodotti_stagione_GenFeb,file=paste0(dir_output,"/","prodotti",suffisso_nome_file))
	#log
	load(file=paste0(dir_input,"/",file_log,"_ordinato"))
	condizione<-(log$giorno>=as.Date(data_inizio) & log$giorno<=as.Date(data_fine))
	log_stagione_GenFeb<-log[condizione,]
	save(log_stagione_GenFeb,file=paste0(dir_output,"/","log",suffisso_nome_file))

#FINE STAGIONE_A GENNAIO-FEBBRAIO SKY-FASTWEB
#---------------------------------------------------------------				
						 
				
				
#---------------------------------------------------------------					 
#STAGIONE_B MARZO SPORT
data_inizio<-"2013-03-01"
data_fine<-"2013-03-31"
dir_output="Sample_stagioni/Mar2013"
suffisso_nome_file<-paste0("_stagione_Mar",ANNO)
#visite
load(file=paste0(dir_input,"/",file_visite,"_ordinato"))
	condizione<-(visite$giorno>=as.Date(data_inizio) & visite$giorno<=as.Date(data_fine))
	visite_stagione_Mar<-visite[condizione,]
	save(visite_stagione_Mar,file=paste0(dir_output,"/","visite",suffisso_nome_file))
#piattaforma
load(file=paste0(dir_input,"/",file_piattaforma,"_ordinato"))
	condizione<-(piattaforma$giorno>=as.Date(data_inizio) & piattaforma$giorno<=as.Date(data_fine))
	piattaforma_stagione_Mar<-piattaforma[condizione,]
	save(piattaforma_stagione_Mar,file=paste0(dir_output,"/","piattaforma",suffisso_nome_file))
#pagine
load(file=paste0(dir_input,"/",file_pagine,"_ordinato"))
	condizione<-(pagine$giorno>=as.Date(data_inizio) & pagine$giorno<=as.Date(data_fine))
	pagine_stagione_Mar<-pagine[condizione,]
	save(pagine_stagione_Mar,file=paste0(dir_output,"/","pagine",suffisso_nome_file))					 
#prodotti
load(file=paste0(dir_input,"/",file_prodotti,"_ordinato"))
	condizione<-(prodotti$giorno>=as.Date(data_inizio) & prodotti$giorno<=as.Date(data_fine))
	prodotti_stagione_Mar<-prodotti[condizione,]
	save(prodotti_stagione_Mar,file=paste0(dir_output,"/","prodotti",suffisso_nome_file))
#log
load(file=paste0(dir_input,"/",file_log,"_ordinato"))
	condizione<-(log$giorno>=as.Date(data_inizio) & log$giorno<=as.Date(data_fine))
	log_stagione_Mar<-log[condizione,]
	save(log_stagione_Mar,file=paste0(dir_output,"/","log",suffisso_nome_file))
# FINE STAGIONE_B MARZO SPORT
#---------------------------------------------------------------	


		
#---------------------------------------------------------------
#SECONDO SEMESTRE
#---------------------------------------------------------------
dir_input="Sample_2013_2sem/dati_ordinati"


file_visite<-"sample2013.2_visite"
file_piattaforma<-"sample2013.2_piattaforma"
file_pagine<-"sample2013.2_pagine"
file_prodotti<-"sample2013.2_prodotti"
file_log<-"sample2013.2_log"

#---------------------------------------------------------------
#STAGIONE_C 15AGOSTO-SETTEMBRE CALCIO
#---------------------------------------------------------------
data_inizio<-"2013-08-15"
data_fine<-"2013-10-31"
dir_output="Sample_stagioni/15AgoSet2013"
suffisso_nome_file<-paste0("_stagione_15AgoSet",ANNO)
#visite
load(file=paste0(dir_input,"/",file_visite,"_ordinato"))
	condizione<-(visite$giorno>=as.Date(data_inizio) & visite$giorno<=as.Date(data_fine))
	visite_stagione_15AgoSet<-visite[condizione,]
	save(visite_stagione_15AgoSet,file=paste0(dir_output,"/","visite",suffisso_nome_file))
#piattaforma
load(file=paste0(dir_input,"/",file_piattaforma,"_ordinato"))
	condizione<-(piattaforma$giorno>=as.Date(data_inizio) & piattaforma$giorno<=as.Date(data_fine))
	piattaforma_stagione_15AgoSet<-piattaforma[condizione,]
	save(piattaforma_stagione_15AgoSet,file=paste0(dir_output,"/","piattaforma",suffisso_nome_file))
#pagine
load(file=paste0(dir_input,"/",file_pagine,"_ordinato"))
	condizione<-(pagine$giorno>=as.Date(data_inizio) & pagine$giorno<=as.Date(data_fine))
	pagine_stagione_15AgoSet<-pagine[condizione,]
	save(pagine_stagione_15AgoSet,file=paste0(dir_output,"/","pagine",suffisso_nome_file))					 
#prodotti
load(file=paste0(dir_input,"/",file_prodotti,"_ordinato"))
	condizione<-(prodotti$giorno>=as.Date(data_inizio) & prodotti$giorno<=as.Date(data_fine))
	prodotti_stagione_15AgoSet<-prodotti[condizione,]
	save(prodotti_stagione_15AgoSet,file=paste0(dir_output,"/","prodotti",suffisso_nome_file))
#log
load(file=paste0(dir_input,"/",file_log,"_ordinato"))
	condizione<-(log$giorno>=as.Date(data_inizio) & log$giorno<=as.Date(data_fine))
	log_stagione_15AgoSet<-log[condizione,]
	save(log_stagione_15AgoSet,file=paste0(dir_output,"/","log",suffisso_nome_file))
#FINE STAGIONE_C 15AGOSTO-SETTEMBRE CALCIO
#---------------------------------------------------------------


				#DA COMPLETARE

#STAGIONE_D OTTOBRE-NOVEMBRE SKY-FASTWEB


#STAGIONE_E 15NOVEMBRE-DICEMBRE CINEMA
