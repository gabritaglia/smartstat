d$set<-"dic"
dati$set<-"feb"
a<-merge(d,dati, by="promo_id", all=T)
a$set.x[is.na(a$set.x)]<-"nodic"
a$set.y[is.na(a$set.y)]<-"nofeb"
table(a$set.x, a$set.y)

b<-a[a$promo_tipologia.y=="Cartaceo",]
table(b$set.x, b$set.y)

e<-a[a$promo_tipologia.y=="Print@home",]
table(e$set.x, e$set.y)

b<-a[a$promo_tipologia.x=="Cartaceo",]
table(b$set.x, b$set.y)

e<-a[a$promo_tipologia.x=="Print@home",]
table(e$set.x, e$set.y)
