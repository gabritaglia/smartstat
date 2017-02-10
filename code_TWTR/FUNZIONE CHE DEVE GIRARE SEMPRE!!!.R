setwd("/home/gabriele/TWTR")
source("script.R")

getwd()
#-- FUNZIONE CHE DEVE GIRARE SEMPRE!!!
for(j in 1:10000000){
  if(format(Sys.time(),"%H:%M")=="01:00"){
    for(i in list.files("DATI")[1:length(list.files("DATI"))]){
      load("CONN/con_2")
      registerTwitterOAuth(twitCred)
      get_tweets(dir=i,paste(Sys.Date()-1)) #-- prendi i tweet
      send_mail(i) #-- mando la mail
      Sys.sleep(1200)
      print(i)
    }
  }
  Sys.sleep(20)
  print(paste(j,"--", Sys.time()))
}

if(FALSE){
#-- funzione artigianale di download tweet
for(j in paste(Sys.Date()-1)){
  for(i in list.files("DATI")[1:length(list.files("DATI"))]){
    print(i)
    load("CONN/con_2")
    registerTwitterOAuth(twitCred)
    get_tweets(dir=paste0(i),paste(j))
    Sys.sleep(1200)
    print(paste("fine",i))
  }
  Sys.sleep(20)
}
}



#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/NUOVOTEMA")
dir.create("DATI/NUOVOTEMA/tweets")
dir.create("DATI/NUOVOTEMA/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/NUOVOTEMA") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok



############################################################################

#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/TIM")
dir.create("DATI/TIM/tweets")
dir.create("DATI/TIM/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/TIM") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok


#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/VODAFONE")
dir.create("DATI/VODAFONE/tweets")
dir.create("DATI/VODAFONE/summary")
file.copy("DATI/VODAFONE/word_search.txt", "DATI/VODAFONE") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok

#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/WIND")
dir.create("DATI/WIND/tweets")
dir.create("DATI/WIND/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/WIND") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok

#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/IMMIGRAZIONE")
dir.create("DATI/IMMIGRAZIONE/tweets")
dir.create("DATI/IMMIGRAZIONE/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/IMMIGRAZIONE") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok


#PER AGGIUNGERE NUOVI ARGOMENTI
#-- CREO TEMA NUOVOTEMA
dir.create("DATI/REDDITOCITTADINANZA_MARONI")
dir.create("DATI/REDDITOCITTADINANZA_MARONI/tweets")
dir.create("DATI/REDDITOCITTADINANZA_MARONI/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/REDDITOCITTADINANZA_MARONI") #modificare il file DATI/NUOVOTEMA/word_search.txt con le parole chiave del nuovotema
# d <- get_tweets("NUOVOTEMA","2015-05-10") #per provare che tutto sia ok

