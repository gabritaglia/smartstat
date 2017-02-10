load("dati/promo")
load("work/tt")
load("rf/rf11")


data(iris)
set.seed(543)

newdata=promo[which(promo$train==1), tt$covariate]
partialPlot(rf11, newdata ,cliente_settore)


partialPlot(rf11, newdata ,buoni_qta_emessa)


promo_spat<-promo[,c("mediana", "skew", "settore2")]
save(promo_spat, file="work/promo_spat")

plot(promo_spat)