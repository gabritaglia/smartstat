#visite
load("sample2/visite_sample2")
visite_sample<-visite_sample2
save(visite_sample,file="sample2/visite_sample")
rm(visite_sample2,visite_sample)

#piattaforma
load("sample2/piattaforma_sample2")
piattaforma_sample<-piattaforma_sample2
save(piattaforma_sample, file="sample2/piattaforma_sample")
rm(piattaforma_sample2,piattaforma_sample)

#pagine
load("sample2/pagine_sample2")
pagine_sample<-pagine_sample2
save(pagine_sample, file="sample2/pagine_sample")
rm(pagine_sample2,pagine_sample)

#prodotti
load("sample2/prodotti_sample2")
prodotti_sample<-prodotti_sample2
save(prodotti_sample, file="sample2/prodotti_sample")
rm(prodotti_sample2,prodotti_sample)

#log
load("sample2/log_sample2")
log_sample<-log_sample2
save(log_sample, file="sample2/log_sample")
rm(log_sample2,log_sample)
