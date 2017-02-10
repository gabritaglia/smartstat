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
