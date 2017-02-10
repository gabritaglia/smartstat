#FLUSSO QMI

source("script/script_APP/funzioni.R")

#richiamo gli oggetti salvati
load(file="Oggetti_per_Piattaforma/dati_input_cartaceo")
dati_a_posto_cartaceo<-dati_a_posto
rm(dati_a_posto)
load(file="Oggetti_per_Piattaforma/rf_cartaceo")
rf_cartaceo<-rf
rm(rf)
load(file="Oggetti_per_Piattaforma/rf_cartaceo_similarita")
rf_cartaceo_similarita<-rf
rm(rf)
load(file="Oggetti_per_Piattaforma/qrf_cartaceo")
qrf_cartaceo<-qrf
rm(qrf)
load(file="Oggetti_per_Piattaforma/Profili_campagne_cartaceo")
prof.cmp_cartaceo<-cmp.profili
rm(cmp.profili)

#INTRODUCO UNA NUOVA CAMPAGNA come data.frame con 1 riga e tante colonne quante variabili di stima.
cmp.nuova_cartaceo<-dati_a_posto_cartaceo[750,2:ncol(dati_a_posto_cartaceo)] #FAKE

#sporco il dato per prova
cmp.nuova_cartaceo$buoni_film_target<-NA
cmp.nuova_cartaceo$buoni_film_genere1<-NA
#CALCOLO LA PREVISIONE DI MEDIA, MEDIANA, PROFILO,RISCHIO DI SUPERARE UN BREAK EVEN per questa nuova campagna
previsione.nuova_cartaceo<-PREVISIONE(CMP.NUOVA=cmp.nuova_cartaceo,
                                      RF=rf_cartaceo,
                                      RF_SIMILARITA=rf_cartaceo_similarita,
                                      QRF=qrf_cartaceo,
                                      BreakEven=70)

#OTTENGO IL DATASET CON LE N CAMPAGNE PIU' SIMILI A QUELLA NUOVA E GRAFICO CORRISPONDENTE
cmp.simili_cartaceo<-SOMIGLIANZE( DATI_INPUT=dati_a_posto_cartaceo,
                                  CMP.PROFILI=prof.cmp_cartaceo,
                                  CMP.NUOVA.PREVISIONE=previsione.nuova_cartaceo,
                                  N_SIMILI=20)

#AGGREGO TUTTI GLI OUTPUT RILEVANTI IN UN UNICO OGGETTO
CMP_OUTPUT_CARTACEO<-list(redemption_media=previsione.nuova_cartaceo$redemption_media,
                          redemption_mediana=previsione.nuova_cartaceo$redemption_mediana,
                          rischio_di_superare_BE=previsione.nuova_cartaceo$rischio_di_superare_BE,
                          profilo_cmp.nuova=previsione.nuova_cartaceo$profilo,
                          cmp.simili=cmp.simili_cartaceo)

###################################################
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
###################################################
source("script/script_APP/funzioni.R")




#richiamo gli oggetti salvati
load(file="Oggetti_per_Piattaforma/dati_input_printhome")
dati_a_posto_printhome<-dati_a_posto
rm(dati_a_posto)
load(file="Oggetti_per_Piattaforma/rf_printhome")
rf_printhome<-rf
rm(rf)
load(file="Oggetti_per_Piattaforma/rf_printhome_similarita")
rf_printhome_similarita<-rf
rm(rf)
load(file="Oggetti_per_Piattaforma/qrf_printhome")
qrf_printhome<-qrf
rm(qrf)
load(file="Oggetti_per_Piattaforma/Profili_campagne_printhome")
prof.cmp_printhome<-prof.cmp
rm(prof.cmp)

#INTRODUCO UNA NUOVA CAMPAGNA come data.frame con 1 riga e tante colonne quante variabili di stima.
cmp.nuova_printhome<-dati_a_posto_printhome[100,2:ncol(dati_a_posto_printhome)] #FAKE

#CALCOLO LA PREVISIONE DI MEDIA, MEDIANA, PROFILO,RISCHIO DI SUPERARE UN BREAK EVEN per questa nuova campagna
previsione.nuova_printhome<-PREVISIONE(CMP.NUOVA=cmp.nuova_printhome,
                                       RF=rf_printhome,
                                       RF_SIMILARITA=rf_printhome_similarita,
                                       QRF=qrf_printhome,
                                       BreakEven=70)

#OTTENGO IL DATASET CON LE N CAMPAGNE PIU' SIMILI A QUELLA NUOVA E GRAFICO CORRISPONDENTE
cmp.simili_printhome<-SOMIGLIANZE( DATI_INPUT=dati_a_posto_printhome,
                                   CMP.PROFILI=prof.cmp_printhome,
                                   CMP.NUOVA.PREVISIONE=previsione.nuova_printhome,
                                   N_SIMILI=20)

#AGGREGO TUTTI GLI OUTPUT RILEVANTI IN UN UNICO OGGETTO
CMP_OUTPUT_PRINTHOME<-list(redemption_media=previsione.nuova_printhome$redemption_media,
                           redemption_mediana=previsione.nuova_printhome$redemption_mediana,
                           rischio_di_superare_BE=previsione.nuova_printhome$rischio_di_superare_BE,
                           profilo_cmp.nuova=cmp.simili_printhome$profilo,
                           cmp.simili=cmp.simili_printhome)


###################################################
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
###################################################