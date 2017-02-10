par.default <- par(no.readonly=TRUE)
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")

load("cartaceo")
ca<-cartaceo

load("printhome")
ph<-printhome

d<-list(ca=ca,ph=ph)

sel<-which(!ca$promo_redemption%in%c(0,100))
hist(ca$promo_redemption[sel], breaks=200, col="blue")
summary(ca$promo_redemption[sel])

#distribuzione della numerosità delle promo
X11()
par(mfrow=c(2,1))
barplot(table(ca$buoni_inizio_validita_aa), main="cartaceo",ylim=c(0,400),)
barplot(table(ph$buoni_inizio_validita_aa), main="print@home",,ylim=c(0,400),)

#promo definitive
lapply(d,function(y) table(y$buoni_dati_promo_definitivi)/nrow(y))

#c'è relazione tra anno e redemption?
X11()
par(mfrow=c(2,1))
#cartaceo
summary(ca$promo_redemption)
plot(d[[1]]$promo_redemption ~ d[[1]]$buoni_inizio_validita_aa, main="cartaceo")
#printhome
summary(ph$promo_redemption)
plot(d[[2]]$promo_redemption ~ d[[2]]$buoni_inizio_validita_aa, main="cartaceo")

#commento al grafico: i livelli di redemption non dipendono dall'anno

#distribuzione della redemption negli anni
X11()
par(mfrow=c(2,1))
boxplot(ca$promo_redemption ~ ca$buoni_inizio_validita_aa ,ylim=c(0,100), main="cartaceo")
boxplot(ph$promo_redemption ~ ph$buoni_inizio_validita_aa , ylim=c(0,100), main="print@home")
#per slides
ca1<-ca[which(ca$buoni_dati_promo_definitivi!=0 & ca$buoni_inizio_validita_aa!=2015),]
png("images/boxplot_redemptiom_anni_cartaceo.png", width=1280, height=800)
par(par.negative)
bp<-boxplot(ca1$promo_redemption ~ ca1$buoni_inizio_validita_aa , ylim=c(0,100), col="white",border="white",medcol="black", las=2,,xaxt="n")
text(x=1:ncol(bp$stats), bp$stats[3,]+10, bp$names,cex=1.5,srt=90, col="black")
dev.off()
#per slides
ca1<-ca[which(ca$buoni_dati_promo_definitivi!=0 & ca$buoni_inizio_validita_aa!=2015
              & ca$cliente_settore!=""),]
ca1$cliente_settore<-factor(ca1$cliente_settore)
levels(ca1$cliente_settore)<-c("Agency","Apparel","Auto","ConsEl","En&Petr","Entert","Financ&Insur","FMCG","Media&Pub","PA","Retail","Services",
                               "Telco","Travel")
png("images/boxplot_redemptiom_settore_cartaceo.png", width=1280, height=800)
par(par.negative)
bp<-boxplot(ca1$promo_redemption ~ ca1$cliente_settore , ylim=c(0,100), col="white",border="white",medcol="black", cex.axis=1, las=1,xaxt="n")
loc<-bp$stats[3,]+c(-10,-7,-10,-10,-20,-10,
                    +10,+15,-10,-3,+15,-10,
                    +15,+10)
text(x=1:ncol(bp$stats), loc, bp$names,cex=1.5,srt=90, col="black")
dev.off()




#correlazione redemption-biglietti emessi
lapply(d,function(y) cor(y$promo_redemption,y$buoni_qta_emessa))
X11()
par(mfrow=c(2,1))
plot(d[[1]]$buoni_qta_emessa, d[[1]]$promo_redemption, main="cartaceo", xlab="buoni_qta_emessa",ylab="promo_redemption")
plot(d[[2]]$buoni_qta_emessa, d[[2]]$promo_redemption, main="print@home", xlab="buoni_qta_emessa",ylab="promo_redemption")

#numerosità per settore
t1<-data.frame(table(ca$cliente_settore))
t2<-data.frame(table(ph$cliente_settore))
t<-merge(t1,t2,by="Var1")
names(t)<-c("Settore","Cartaceo","Print@home")
t
rm(t1,t2,t)
X11()
par(mfrow=c(2,1))
plot(table(ca$cliente_settore), main="cartaceo")
plot(table(ph$cliente_settore), main="print@home")

#redemption per settore
X11()
par(mfrow=c(2,1), cex=0.8)
bymedian <- with(ca, reorder(cliente_settore, -promo_redemption, median))
boxplot(ca$promo_redemption ~ bymedian ,ylim=c(0,100), main="cartaceo")
bymedian <- with(ph, reorder(cliente_settore, -promo_redemption, median))
boxplot(ph$promo_redemption ~ bymedian , ylim=c(0,100), main="print@home")


mer<-lapply(d,function(y)tapply(y$promo_redemption, y$cliente_settore,median))
rgr<-lapply(d,function(y)tapply(y$promo_redemption, y$cliente_settore,IQR))
X11()
par(mfrow=c(1,2), cex=0.7)
plot(mer[[1]], rgr[[1]],main="cartaceo")
text(mer[[1]], rgr[[1]],names(mer[[1]]))
plot(mer[[2]], rgr[[2]],main="print@home")
text(mer[[2]], rgr[[2]],names(mer[[2]]))

#redemption vs buoni_durata
X11()
par(mfrow=c(1,2))
plot(ca$promo_redemption, ca$buoni_durata,, main="cartaceo", col=ca$cliente_settore)
plot(ph$promo_redemption, ph$buoni_durata, main="print@home", col=ca$cliente_settore)

#frequenze rilevanti
names(d$ca)
apply(d$ca[,12:44],2,table)


###
#salvo in locale i grafici rilevanti
###
#dir.create("images")
#
png(filename = "images/distribuzione della numerosità delle promo.png",
    width = 720, height = 720)
par(mfrow=c(2,1))
barplot(table(ca$buoni_inizio_validita_aa), main="cartaceo",ylim=c(0,400))
barplot(table(ph$buoni_inizio_validita_aa), main="print@home",,ylim=c(0,400))
dev.off()
#
png(filename = "images/distribuzione della redemption negli anni.png",
    width = 720, height = 720)
par(mfrow=c(2,1))
boxplot(ca$promo_redemption ~ ca$buoni_inizio_validita_aa ,ylim=c(0,100), main="cartaceo")
boxplot(ph$promo_redemption ~ ph$buoni_inizio_validita_aa , ylim=c(0,100), main="print@home")
dev.off()
#
png(filename = "images/correlazione redemption-biglietti emessi.png",
    width = 720, height = 720)
par(mfrow=c(2,1))
plot(d[[1]]$buoni_qta_emessa, d[[1]]$promo_redemption, main="cartaceo", xlab="buoni_qta_emessa",ylab="promo_redemption")
plot(d[[2]]$buoni_qta_emessa, d[[2]]$promo_redemption, main="print@home", xlab="buoni_qta_emessa",ylab="promo_redemption")
dev.off()

#
png(filename = "images/redemption per settore.png",
    width = 720, height = 720)
par(mfrow=c(2,1), cex=0.8)
bymedian <- with(ca, reorder(cliente_settore, promo_redemption, median))
boxplot(ca$promo_redemption ~ bymedian ,ylim=c(0,100), main="cartaceo", cex=0.7)
print(bymedian)
bymedian <- with(ph, reorder(cliente_settore, promo_redemption, median))
boxplot(ph$promo_redemption ~ bymedian , ylim=c(0,100), main="print@home")
print(bymedian)
dev.off()
#
