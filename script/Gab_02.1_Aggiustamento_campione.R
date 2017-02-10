#TABELLA 1 -- VISITE

load("sample/visite_sample")


visite_sample<-as.character(visite_sample$ora)
#distinguo il timestamp in giorno/ora/weekend
ore_prelavoro<-c("06","07","08")
ore_mattina<-c("09","10","11")
ore_pranzo<-c("12","13","14")
ore_pome<-c("15","16","17","18")
ore_sera<-c("19","20","21","22","23")
ore_notte<-c("00","01","02","03","04","05")
#fascia_oraria
visite_sample$fascia_oraria<-""
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_prelavoro)]<-"0_prelavoro"
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_mattina)]<-"1_morning"
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_pranzo)]<-"2_lunch"
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_pome)]<-"3_afternoon"
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_sera)]<-"4_evening"
visite_sample$fascia_oraria[which(visite_sample$ora %in% ore_notte)]<-"5_night"



table(visite_sample$fascia_oraria)
save(visite_sample,file="sample/visite_sample")
