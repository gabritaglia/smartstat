library(dplyr)

load("sample2/cookies")

cookies$n_entry_pages_totali[is.na(cookies$n_entry_pages_totali)]<-1
subset$n_entry_pages_totali[is.na(subset$n_entry_pages_totali)]<-1
#seleziono solo le variabili di navigazione e il visitorID
names(cookies)
mydata<-subset %>%
        select(n_visite, 
#                X1Monday.00:X7Sunday.22,
               p_visite_Lun,
               p_visite_Mar,                  
               p_visite_Mer,                 
               p_visite_Gio,                 
               p_visite_Ven,                 
               p_visite_Sab,                 
#                p_visite_Dom,                  
#                p_visite_wd,                   
                p_visite_we,                   
#                p_visite_prelav,               
#                p_visite_morn,                 
#                p_visite_lunc,                 
#                p_visite_afte,                 
#                p_visite_even,                 
#                p_visite_nigh,                 
#                p_visite_prelav_wd,            
#                p_visite_prelav_we,            
#                p_visite_morn_wd,              
#                p_visite_morn_we,              
#                p_visite_lunc_wd,              
#                p_visite_lunc_we,              
#                p_visite_afte_wd,              
#                p_visite_afte_we,             
#                p_visite_even_wd,             
#                p_visite_even_we,             
#                p_visite_nigh_wd,              
#                p_visite_nigh_we,              
               n_days_visite,                 
               int_days_life,                 
               p_days_visiteSUlife,           
               p_days_visiteSUperiodo, 
               n_pagine_viste,
               n_entry_pages_distinte,
               n_entry_pages_totali
               )

rownames(mydata)<-subset$visitorID

# 
# mydata<-mydata[sample(1:nrow(mydata),10000),]
summary(mydata)
# str(mydata)

cor.matrix<-cor(mydata)

# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components


plot(fit$scores[,1:2], col=factor(subset$soggetto), cex=0.4)

#zoomed
plot(fit$scores[,1:2], col=factor(subset$soggetto), cex=0.4,
     xlim=c(-100,max(fit$scores[,1])), ylim=c(min(fit$scores[,2]),100))


# 3D Scatterplot
library(scatterplot3d)
scatterplot3d(fit$scores[,1:3], main="3D Scatterplot")

boxplot(fit$scores[,1]~ factor(cookies$soggetto), outline=F)
boxplot(fit$scores[,2]~ factor(cookies$soggetto), outline=F)




plot(fit$scores[,1:2], col=factor(cookies$soggetto), cex=0.4,
     xlim=c(-100,max(fit$scores[,1])), ylim=c(min(fit$scores[,2]),100))



cor.matrix<-cor(mydata)

# apply PCA - scale. = TRUE is highly
# advisable, but default is FALSE.
pca <- prcomp(mydata,
                 center = TRUE,
                 scale. = TRUE) 


print(pca)
plot(pca, type = "l")


summary(pca)
# 
# library(devtools)
# install_github("ggbiplot", "vqv")
# 
# library(ggbiplot)
# g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
#               groups = cookies$soggetto, ellipse = TRUE,
#               circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal',
#                legend.position = 'top')
# print(g)































# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
fit <- factanal(mydata, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7) # add variable names 


# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(mydata) # graphs generated automatically 