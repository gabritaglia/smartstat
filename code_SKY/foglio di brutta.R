cookies$mobile.vendor<-paste0(cookies$piattaforma,".",cookies$vendor)
t<-table(cookies$vendor.mobile,cookies$soggetto)

prop.table(t,1)*100

prop.table(t,2)*100


t<-table(cookies$old,cookies$soggetto)
prop.table(t,1)*100
prop.table(t,2)*100


t<-table(cookies$nav_tanto,cookies$soggetto)
t
addmargins(t,margin=c(1,2))

prop.table(t,1)*100
prop.table(t,2)*100

t<-table(N$mobile.vendor,N$soggetto)
t
addmargins(t,margin=c(1,2))

round(prop.table(t,1)*100,2)
round(prop.table(t,2)*100,2)






cookies$nav_tanto<-0
cookies$nav_tanto[cookies$visitorID %in% nav_tanto]<-1
sum(cookies$nav_tanto)

sum(cookies$n_visite>mean(cookies$n_visite))
sum(cookies$n_days_visite>mean(cookies$n_days_visite))
sum(cookies$n_pagine_viste>mean(cookies$n_pagine_viste))
sum(cookies$n_entry_pages_distinte>mean(cookies$n_entry_pages_distinte))
sum(cookies$n_entry_pages_totali>mean(cookies$n_entry_pages_totali))




A<-cookies %>%
  select(n_visite, n_days_visite, n_pagine_viste,n_entry_pages_distinte,n_entry_pages_totali)

N<-cookies %>%
    filter(nav_tanto==1) %>%
    select(n_visite, n_days_visite, n_pagine_viste,n_entry_pages_distinte,n_entry_pages_totali)

summary(N)
summary(A)


A.varnav<-cookies %>%
  dplyr::select(soggetto,n_visite, n_days_visite, n_pagine_viste,
         n_entry_pages_distinte,n_entry_pages_totali,
         p_visite_Mar:p_visite_Dom,
#          p_visite_wd,
#          p_visite_morn:p_visite_nigh,
         p_visite_morn_wd:p_visite_nigh_we)
A.varnav$soggetto<-factor(A.varnav$soggetto)

# Linear Discriminant Analysis with Jacknifed Prediction
library(MASS)
fit <- lda(outcome ~ ., data=mydata,
           na.action="na.omit", CV=TRUE)
fit # show results
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(mydata$soggetto, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))




# Linear Discriminant Analysis with Jacknifed Prediction
library(MASS)
fit <- lda(G ~ x1 + x2 + x3, data=mydata,
           na.action="na.omit", CV=TRUE)
fit # show results





a<-table(acq$days_basket_purchase)
a<-as.data.frame(a)
a$Var1<-as.numeric(as.character(a$Var1))
a$new<-"uno"
a$new[a$Var1>1 & a$Var1<=7]<-"sett"
a$new[a$Var1>7]<-"oltre"


aggregate(a$Freq, by=list(a$new), sum)