load("dati/promo")
load("work/tt")
load("rf/rf11")

covariate<-tt$covariate
dist.matrix<-1/rf11$proximity
dist.matrix[which(dist.matrix==Inf)]<-max(dist.matrix[which(dist.matrix!=Inf)])

quali<-which(promo$train==1)
campione<-sample(1:length(quali),size=100)

d<-dist.matrix[campione,campione]
d<-as.dist(d)
data<-promo[,covariate]

hc<-hclust(d, method="complete")
str(hc)
plot(hc, hang=-1)
# draw dendogram with red borders around the 5 clusters
rect.hclust(hc, k=10, border="red") 

groups <- cutree(hc, k=10) # cut tree into 5 clusters

aggregate(data,by=list(groups), mean)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster) 



library(cluster)
#DIANA ALGORITHM
d<-dist.matrix
dv <- diana(d,diss=T, metric = "euclidean", stand = F)
print(dv)
plot(dv)
## Cut into 2 groups:
dv2 <- cutree(as.hclust(dv), k = 2)
table(dv2) # 8 and 42 group members


#AGNES ALGORITHM
ac<-agnes(d, diss =T, stand = F, method = "ward")
print(ac)
plot(ac,which.plots =2)
## Cut into 2 groups:
ac2 <- cutree(as.hclust(ac), k = 2)
table(dv2) # 8 and 42 group members


p1<-promo[which(promo$train==1),]
dim(p1)
plot(p1$media.rf,p1$promo_redemption, col=ac2,xlim=c(0,100),ylab=c(0,100) )

X.VarExp(p1$promo_redemption,p1$media.rf)
X.VarExp(p1$promo_redemption[ac2==1],p1$media.rf[ac2==1])
X.VarExp(p1$promo_redemption[ac2==2],p1$media.rf[ac2==2])




