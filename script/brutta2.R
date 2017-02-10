id<-unique(pg$visitorID)
id<-id[sample(1:length(id),1000)]

cookies<-data.frame(id)
names(cookies)<-"visitorID"
cookies$visitorID<-as.character(cookies$visitorID)
pg1<-pg %>%
  filter(visitorID %in% id)

pg<-pg1
rm(pg1)
