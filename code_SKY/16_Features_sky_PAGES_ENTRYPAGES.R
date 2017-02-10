#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#richiamo lo script che genera la funzioni per lavorare sugli URL
source("script/16_Features_sky_PAROLE_URL.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#richiamo lo script che genera la lista delle parole
source("script/16_Features_sky_FUNZIONI.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(dplyr)
library(data.table)
#carico il file di pages
pages<-read.csv2("Sample_stagioni/pages/pages_2013.1.csv")  #<-----MODIFICARE

pages<-pages[,"pages"] #<-----MODIFICARE

# esempio
# load("sample2/pagine_sample")
# pages<-pagine_sample %>% distinct(Pages) %>% select(Pages)
# pages<-pages[1:100,]

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
URL$pages<-as.character(URL$pages) #per sicurezza trasformo in character

write.csv2(file=paste0(dir_output,"/URL_PAGES_semantizzate_2014"), URL, row.names=F)




############################################################################################
#carico il file di pages
entry_pages<-read.csv2("XXXXX")  #<-----MODIFICARE

entry_pages<-entry_pages[,"entry_pages"] #<-----MODIFICARE

# esempio
# load("sample2/pagine_sample")
# entry_pages<-pagine_sample %>% distinct(Entry.Pages) %>% select(Entry.Pages)
# entry_pages<-entry_pages[1:100,]


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
entryURL$pages<-as.character(entryURL$pages) #per sicurezza trasformo in character


write.csv2(file=paste0(dir_output,"/URL_ENTRYPAGES_semantizzate_2014"), entryURL, row.names=F)

