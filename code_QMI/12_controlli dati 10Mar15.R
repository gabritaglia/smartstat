
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")

summary(dati)


names(dati)


png("plot_anno.png")
boxplot(dati$promo_redemption ~ dati$buoni_inizio_validita_aa, xlab="Anno", ylab="Redemption")
dev.off()

png("plot_settore.png")
boxplot(dati$promo_redemption ~ dati$cliente_settore, xlab="Settore", ylab="Redemption")
dev.off()

png("plot_settore2.png")
boxplot(dati$promo_redemption ~ dati$settore2, xlab="Settore", ylab="Redemption", )
dev.off()

table(dati$settore2, useNA="always")


par.default <- par(no.readonly=TRUE)
par.negative<-c(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white", fg="white")


#per slides
ca1<-ca[which(ca$buoni_dati_promo_definitivi!=0 & ca$buoni_inizio_validita_aa!=2015),]
ca1$buoni_inizio_validita_aa<-factor(ca1$buoni_inizio_validita_aa)
png("images/boxplot_redemptiom_anni_printHome.png", width=1280, height=800)
par(par.negative)
bp<-boxplot(ca1$promo_redemption ~ ca1$buoni_inizio_validita_aa , ylim=c(0,100), col="white",border="white",medcol="black", las=2,,xaxt="n")
text(x=1:ncol(bp$stats), bp$stats[3,]+10, bp$names,cex=1.5,srt=90, col="black")
dev.off()
#per slides
ca1<-ca[which(ca$buoni_dati_promo_definitivi!=0 & ca$buoni_inizio_validita_aa!=2015
              & ca$settore2!=""),]
ca1$settore2<-factor(ca1$settore2)
levels(ca1$settore2)[4]<-"FIN&INSUR"

png("images/boxplot_redemptiom_settore_printHome.png", width=1280, height=800)
par(par.negative)
bp<-boxplot(ca1$promo_redemption ~ ca1$settore2 , ylim=c(0,100), col="white",border="white",medcol="black", cex.axis=1, las=1)
dev.off()