#FLUSSO QMI

source("script/script_APP/funzioni.R")


#DATI NUOVI 10 Marzo 2015
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")


CONTROLLODATI(dati)
#######################################
#  PARTE I MONDO CARTACEO
#######################################

#nomi delle variabili su cui si farà la random forest
#ELENCO DELLE VARIABILI SU CUI SI FARA' LA RANDOM FOREST
var_names_cartaceo<-c( "promo_meccanica",
                       "promo_distribuzione_biglietti",
                       "cliente_settore",
                       #              "cliente_sottosettore",
                       #              "buoni_inizio_validita_aa",
                       "buoni_durata",
                       "buoni_stagione_primavera",
                       "buoni_stagione_estate",
                       "buoni_stagione_autunno",
                       "buoni_stagione_inverno",
                       "buoni_feste_capodanno",
                       "buoni_feste_epifania",
                       "buoni_feste_sanvalentino",
                       "buoni_feste_venticinqueaprile",
                       "buoni_feste_primomaggio",
                       "buoni_feste_pasqua",
                       "buoni_feste_duegiugno",
                       "buoni_feste_ferragosto",
                       "buoni_feste_primonovembre",
                       "buoni_feste_ottodicembre",
                       "buoni_feste_natale",
                       "buoni_feste_santostefano",
                       "buoni_feste_ultimodellanno",
                       "buoni_per_tutta_la_durata",
                       "buoni_redemption_o_forfait",
                       "buoni_intero_o_sconto",
                       "buoni_tipologia",
                       "buoni_film_target",
                       "buoni_film_genere1",
#                        "buoni_film_genere2",
#                        "buoni_film_genere3",
                       "buoni_3d",
                       "buoni_valido_lun",
                       "buoni_valido_mar",
                       "buoni_valido_mer",
                       "buoni_valido_gio",
                       "buoni_valido_ven",
                       "buoni_valido_sab",
                       "buoni_valido_dom",
                       "buoni_qta_emessa"
)

#Preparazione dei dati
dati_a_posto_cartaceo<-PREPARAZIONEDATI(DATI_INPUT=dati,
                               TIPOLOGIA="Cartaceo",
                               VARIABILI_INCLUSE=var_names_cartaceo,
                               VARIABILI_ESCLUSE=NULL,
                               OUTCOME="promo_redemption",
                               ANNI_TRAIN=c(2011,2012,2013,2014),
                               NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/dati_input_cartaceo")

#STIMA DELLA RANDOM FOREST PER IL CARTACEO
rf_cartaceo<-RANDOMFOREST(DATI_INPUT=dati_a_posto_cartaceo,
                 NTREE=1000,
                 N_VAR_SPLIT= "default",
                 NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_cartaceo")


#STIMA DELLA RANDOM FOREST PER IL CARTACEO SIMILARITA
rf_cartaceo_similarita<-RANDOMFOREST_SIMILARITA(DATI_INPUT=dati_a_posto_cartaceo,
                          NTREE=1000,
                          N_VAR_SPLIT= "default",
                          MAXNODES=10,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_cartaceo_similarita")

#STIMA DELLA QUANTILE RANDOM FOREST PER IL CARTACEO
qrf_cartaceo<-QUANTILERANDOMFOREST(DATI_INPUT=dati_a_posto_cartaceo,
                          NTREE=1000,
                          N_VAR_SPLIT= "default",
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/qrf_cartaceo")

#SALVATAGGIO DEI PROFILI DEI NODI DEI DATI DI STIMA
cmp.profili<-PROFILICAMPAGNE(RF=rf_cartaceo_similarita,
                          DATI=dati_a_posto_cartaceo,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/Profili_campagne_cartaceo")

#FINE PARTE PRE NUOVA CAMPAGNA

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
#DATI NUOVI 10 Marzo 2015
dati<-read.csv2("dati originali/statistiche_promozioni _ 20150310.csv")
CONTROLLODATI(dati)

#######################################
#  PARTE II MONDO PRINT@HOME
#######################################

#nomi delle variabili su cui si farà la random forest
#ELENCO DELLE VARIABILI SU CUI SI FARA' LA RANDOM FOREST
var_names_printhome<-c("promo_meccanica",
                       "promo_distribuzione_biglietti",
                       "cliente_settore",
#                      "cliente_sottosettore",
#                      "buoni_inizio_validita_aa",
                       "buoni_durata",
                       "buoni_stagione_primavera",
                       "buoni_stagione_estate",
                       "buoni_stagione_autunno",
                       "buoni_stagione_inverno",
                       "buoni_feste_capodanno",
                       "buoni_feste_epifania",
                       "buoni_feste_sanvalentino",
                       "buoni_feste_venticinqueaprile",
                       "buoni_feste_primomaggio",
                       "buoni_feste_pasqua",
                       "buoni_feste_duegiugno",
                       "buoni_feste_ferragosto",
                       "buoni_feste_primonovembre",
                       "buoni_feste_ottodicembre",
                       "buoni_feste_natale",
                       "buoni_feste_santostefano",
                       "buoni_feste_ultimodellanno",
                       "buoni_per_tutta_la_durata",
                       "buoni_redemption_o_forfait",
                       "buoni_intero_o_sconto",
                       "buoni_tipologia",
                       "buoni_film_target",
                       "buoni_film_genere1",
#                        "buoni_film_genere2",
#                        "buoni_film_genere3",
                       "buoni_3d",
                       "buoni_valido_lun",
                       "buoni_valido_mar",
                       "buoni_valido_mer",
                       "buoni_valido_gio",
                       "buoni_valido_ven",
                       "buoni_valido_sab",
                       "buoni_valido_dom",
                       "buoni_qta_emessa",
                       "codici_durata",
                       "codici_validita",
                       #               "codici_inseriti",
                       #               "codici_stampati",
                       "codici_carnet",
                       "codici_numero_utilizzi"
  )

#Preparazione dei dati
dati_a_posto_printhome<-PREPARAZIONEDATI(DATI_INPUT=dati,
                                        TIPOLOGIA="Print@home",
                                        VARIABILI_INCLUSE=var_names_printhome,
                                        VARIABILI_ESCLUSE=NULL,
                                        OUTCOME="promo_redemption",
                                        ANNI_TRAIN=c(2011,2012,2013,2014),
                                        NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/dati_input_printhome")

#STIMA DELLA RANDOM FOREST PER IL printhome
rf_printhome<-RANDOMFOREST(DATI_INPUT=dati_a_posto_printhome,
                          NTREE=1000,
                          N_VAR_SPLIT= "default",
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_printhome")


#STIMA DELLA RANDOM FOREST PER IL printhome SIMILARITA
rf_printhome_similarita<-RANDOMFOREST_SIMILARITA(DATI_INPUT=dati_a_posto_printhome,
                                                NTREE=1000,
                                                N_VAR_SPLIT= "default",
                                                MAXNODES=10,
                                                NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/rf_printhome_similarita")

#STIMA DELLA QUANTILE RANDOM FOREST PER IL printhome
qrf_printhome<-QUANTILERANDOMFOREST(DATI_INPUT=dati_a_posto_printhome,
                                   NTREE=1000,
                                   N_VAR_SPLIT= "default",
                                   NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/qrf_printhome")

#SALVATAGGIO DEI PROFILI DEI NODI DEI DATI DI STIMA
prof.cmp<-PROFILICAMPAGNE(RF=rf_printhome_similarita,
                          DATI=dati_a_posto_printhome,
                          NOME_FILE_OUTPUT="Oggetti_per_Piattaforma/Profili_campagne_printhome")

#FINE PARTE PRE NUOVA CAMPAGNA

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