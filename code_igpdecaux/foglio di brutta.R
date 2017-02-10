require(classInt)
data(jenks71)
pal1 <- c("wheat1", "red3")
opar <- par(mfrow=c(2,3))
plot(classIntervals(jenks71$jenks71, n=5, style="fixed",
                    fixedBreaks=c(15.57, 25, 50, 75, 100, 155.30)), pal=pal1, main="Fixed")
x <- Prop.cat[,2]
classIntervals(x, n=4, style="jenks")     #Jenks algorithm (scelta Debora)
classIntervals(x, n=4, style="kmeans")    #kmeans breaks
classIntervals(x, n=4, style="quantile")  #quantile breaks
classIntervals(x, n=4, style="equal")     #equal parts
classIntervals(x, n=4, style="pretty")    #legible breaks
classIntervals(x, n=4, style="hclust")    #hierarchical clustering
classIntervals(x, n=4, style="bclust")    #bagged clustering
classIntervals(x, n=4, style="bclust")    #Fisher algorithm clustering



names(Anse2013.punteggi.rank)
somma<-apply(Anse2013.punteggi.rank[,c("PR1", "PR2", "PR3")],1,sum)
somma[1:10]
fattore<-Anse2013.punteggi.rank[,c(1,2,14,16:21)]
write.csv(fattore, file="Anse2013.RiepilogoRank.csv")
getwd()

apply(dati[,c("PR1","PR2","PR3")],2,table)
apply(x[,c("PR1","PR2","PR3")],2,table)

dati[dati$PR1==5, c("PR1","PR2","PR3")]
