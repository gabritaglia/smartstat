library(ROAuth)
library(twitteR)

setwd("/home/gabriele/TWTR")
#source("http://dl.dropboxusercontent.com/u/51118288/send_mail_r.txt")
#non apre la connessione, non so per quale motivo, così ho salvato il file in locale in home/gabriele/TWTR e lo leggo da lì
source("send_mail_r.txt")

create_contents_mail <- function(toaddrs,subject,text){
  library(rJython) 
  rJython <- rJython() 
  rJython$exec("import smtplib") 
  rJython$exec("from email.MIMEText import MIMEText") 
  rJython$exec("import email.utils") 
  
  mail <- c(
    "fromaddr = 'barbapianist@gmail.com'", 
    "toaddrs  = 'stefanobarberis@hotmail.it'", 
    "msg = MIMEText('Ciao bel cipollotto!!!')", 
    "msg['From'] = email.utils.formataddr(('sender name', fromaddr))", 
    "msg['To'] = email.utils.formataddr(('recipient name', toaddrs))", 
    "msg['Subject'] = 'Download tweets'", 
    
    #SMTP server credentials 
    "username = 'barbapianist@gmail.com'", 
    "password = 'barberizzio:112'", 
    
    #Set SMTP server and send email, e.g., google mail SMTP server 
    "server = smtplib.SMTP('smtp.gmail.com:587')", 
    "server.ehlo()", 
    "server.starttls()", 
    "server.ehlo()", 
    "server.login(username,password)", 
    "server.sendmail(fromaddr,toaddrs,msg.as_string())", 
    "server.quit()")
  mail[2] <- paste("toaddrs = '",toaddrs,"'")
  mail[3] <- paste("msg = MIMEText('",text,"')")
  mail[6] <- paste("msg['Subject'] = '",subject,"'")
  jython.exec(rJython,mail)
}


#-- creo connessioni
load("CONN/con_1")
load("CONN/con_2")
registerTwitterOAuth(twitCred)

#-- test della connessione
#searchTwitter("settemari", n=100)

#-- download dei tweet
download <- function(dir,keywords,intervallotempo,n=100){
  data_inizio <- paste(intervallotempo[1])
  data_fine <- paste(intervallotempo[2])
  
  megafile <- data.frame()
  conteggio <- data.frame(keywords,0)
  names(conteggio)[2] <- "frequenza"
  
  for(i in 1:length(keywords)){
    print(1)
    tweets <- try(searchTwitter(keywords[i],n=n,lang="it",since=data_inizio,until=data_fine,locale=NULL,geocode=NULL, sinceID=NULL,retryOnRateLimit=1200),TRUE)
    print(paste(keywords[i],"---",length(tweets)))
    if(class(tweets)!="try-error"){
      if(length(tweets)!=0){
        df <- twListToDF(tweets)
        megafile <- rbind(megafile,df)
        conteggio[i,2] <- length(tweets)
      }
    }
  }
  megafile <- megafile[which(duplicated(megafile$id)==F),] #-- elimina i tweets duplicati
  
  nome_file <- paste(data_inizio)
  save(megafile,file=paste0("DATI/",dir,"/tweets/",nome_file))
  write.table(conteggio,file=paste0("DATI/",dir,"/summary/",nome_file,".csv"),row.names=FALSE,sep=";")  
  return(megafile)
}

#prova
if(FALSE){
data <- paste(Sys.Date()-1)
keywords <- c("@Expo2015Milano")
data_inizio <- paste(as.Date(data))
data_fine <- paste(as.Date(data)+1)
i <- 1
n <- 1000
tweets <- try(searchTwitter(keywords[i],n=n,lang="it",since=data_inizio,until=data_fine,locale=NULL,geocode=NULL, sinceID=NULL,retryOnRateLimit=1200),silent=TRUE)
download("TEST",keywords,c(as.Date(data),as.Date(data)+1),n=10000)
res <- searchTwitter("#expo365",n=900,lang="it",since=paste(as.Date(data)),until=paste(as.Date(data)+1),locale=NULL,geocode=NULL, sinceID=NULL,retryOnRateLimit=1200)
twListToDF(tweets)
}

#-- strutturazione resoconto tweet
get_tweets <- function(dir,data,n=20000){
  word_search <- paste(t(read.table(paste0("DATI/",dir,"/word_search.txt"),fill=TRUE,sep=",")[1,])[,1])
  d <- download(dir,word_search,c(as.Date(data),as.Date(data)+1),n=n)
  return(d)
}

#-- mando la mail
send_mail <- function(dir){
  subject <- paste("ESITO DOWNLOAD TWEETS - ",dir," - ",format(Sys.time(),"%d-%m-%Y %H:%M"))
  file_path <- paste0("DATI/",dir,"/summary/",Sys.Date()-1,".csv")
  tabella <- read.table(file_path,sep=";",header=TRUE,stringsAsFactors=FALSE)
  
  text_1 <- paste("NOME FILE:",file_path)
  text_2 <- paste("---------------------------------")
  text_3 <- paste(apply(apply(tabella,2,paste),1,paste,collapse="-->"),collapse="\\n")
  text_4 <- paste("totale(con eventuali duplicati) -->",sum(tabella[,2]))
  text_5 <- paste("---------------------------------")
  text <- paste(text_1,text_2,text_3,text_4, text_5,sep=" \\n ")
  
  create_contents_mail("stefanobarberis@hotmail.it",subject,text)
  create_contents_mail("gabri.tagliabue@gmail.com",subject,text)
  create_contents_mail("marco.fattore@smartstat.it",subject,text)
}

if(FALSE){
    #-- FUNZIONE CHE DEVE GIRARE SEMPRE!!!
    for(j in 1:10000000){
      if(format(Sys.time(),"%H:%M")=="01:00"){
        for(i in list.files("DATI")[1:3]){
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
}
if(FALSE){
    #-- funzione artigianale di download tweet
    for(j in paste(Sys.Date()-1)){
      for(i in list.files("DATI")[1:3]){
        print(i)
        load("CONN/con_2")
        registerTwitterOAuth(twitCred)
        get_tweets(dir=paste0(i),paste(j))
        Sys.sleep(20)
        print(paste("fine",i))
      }
      Sys.sleep(10)
    }
}
if(FALSE){ #equivale a commentare tutte le righe
#-- CREO TEMA EXPO
dir.create("DATI/EXPO")
dir.create("DATI/EXPO/tweets")
dir.create("DATI/EXPO/summary")

d <- get_tweets("EXPO","2014-05-24")

#-- CREO TEMA H3G
dir.create("DATI/H3G")
dir.create("DATI/H3G/tweets")
dir.create("DATI/H3G/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/H3G")
d <- get_tweets("H3G","2014-08-24")

#-- CREO TEMA MEETING
dir.create("DATI/MEETING")
dir.create("DATI/MEETING/tweets")
dir.create("DATI/MEETING/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/MEETING")
d <- get_tweets("MEETING","2014-05-24")

#-- CREO TEMA BUONASCUOLA
dir.create("DATI/BUONASCUOLA")
dir.create("DATI/BUONASCUOLA/tweets")
dir.create("DATI/BUONASCUOLA/summary")
file.copy("DATI/EXPO/word_search.txt", "DATI/BUONASCUOLA")
d <- get_tweets("BUONASCUOLA","2015-05-10")
}


########################################################################## prove - non toccare
if(FALSE){ #equivale a commentare tutte le righe
subject <- paste("ESITO DOWNLOAD TWEETS - ",format(Sys.time(),"%d-%m-%Y %H:%M"))
tabella <- read.table(paste0("DATI/EXPO/",Sys.Date()-1,".csv"),sep=";",header=TRUE,stringsAsFactors=FALSE)
text_1 <- paste("---------------------------------")
text_2 <- paste(apply(apply(tabella,2,paste),1,paste,collapse="-->"),collapse="\\n")
text_3 <- paste("---------------------------------")
text <- paste(text_1,text_2,text_3,sep=" \\n ")
create_contents_mail("stefanobarberis@hotmail.it",subject,text)

send_mail("EXPO")
send_mail("H3G")
send_mail("MEETING")
}
