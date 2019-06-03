require(dplyr)
require(data.table)
require(tidyr)
require(caret)
require(missForest)

# reading serasa definition of net user / not net user
setwd("D:/Users/sb044936/Desktop/Databases")
base_serasa = fread("base_serasa.txt", sep = "\t", dec = ",", colClasses = c("cpf_cnpj" = "character"), na.strings = "")
base_serasa_2 = fread("base_serasa_2.txt", sep = "\t", dec = ",", colClasses = c("cpf_cnpj" = "character"), na.strings = "")
base_serasa_3 = fread("base_serasa_3.txt", sep = "\t", dec = ",", colClasses = c("cpf_cnpj" = "character"), na.strings = "")
base_serasa = bind_rows(base_serasa, base_serasa_2); rm(base_serasa_2)
base_serasa = bind_rows(base_serasa, base_serasa_3); rm(base_serasa_3)

base_serasa = base_serasa %>% select(cpf_cnpj, ENR_mosaic, ENR_descricao_mosaic, ENR_score_netuser, ENR_propensao_netuser, 
                                     REC_fx_cons_cred, REC_fx_cred_ativos, REC_fx_score, REC_obito) %>% distinct()

# reading igb database
setwd("~/IGB/Daily IGB")
setwd("C:/CADOCAN")
base_fecha = fread("igb_daily_31_05.txt", header = TRUE, 
                       dec = ",", check.names = TRUE, 
                       colClasses = c("Contrato" = "character",
                                      "Cep" = "character",
                                      "Cep l" = "character",
                                      "CPF CNPJ" = "character"), na.strings = "")

# reading all functions for daily scoring - formatting_database function standardize column names / create new features / clean some data
setwd("R:/Estatística/BHB/R Scripts")
if(!exists("foo", mode="function")) source("[BHB] Daily Scoring Functions.R", encoding = "UTF-8") 
formatting_database(base_fecha)

# filtering days overdue > 0 and <= 360
bhb.final = bhb.final %>% filter(qtd_dias_em_atraso > 0 & qtd_dias_em_atraso <= 360)

# joining igb database with serasa infos
all_data = left_join(bhb.final, base_serasa, by = "cpf_cnpj") %>% distinct()

# selecting columns that are most important for imputting missing data
all_data_filtered = all_data %>% select(select = -c(cpf_cnpj,tabela_neg,num_chassi,cep_digito_cli,cep_cli,nome_cliente,vlr_tx_anual_ctr,
                                                   cep_loja,vlr_tx_banco,vlr_taxa_cliente,cod_tabela,nome_placa,analista_c,
                                                   data_contrato, vlr_vrg_antecipado, vlr_vrg_diluido, vlr_saldo_inicial,
                                                   vlr_liberado, cod_inst_financ, cod_marca, data_risco_contabil, vlr_seguri_casco,
                                                   vlr_tac, data_ult_pgt, data_vencimento, data_nascimento_cli, data_baixa, cod_banco,
                                                   data_ult_alt, proposta, cod_plano, vlr_subs_conc, vlr_subs_marca, vlr_taxa_subs_conc, vlr_desp_finan,
                                                   car, cod_pessoa, data_ult_vencimento, vlr_tx_subs_marc, re, data_ini_seguro, data_fim_seguro,
                                                   data_prim_vencimento, nome_renavam, `for`, contrato_cedido, numero_contrato_cessao,
                                                   coobrigacao_sem_n, vlr_seg_gara_estend, status_contrato, qtd_parcelas_pagas, #score_credit,
                                                   tipo_pessoa, #stat_model, stat_model_update, 
                                                   nome_seg_prot_finan, nome_seg_gara_estend, nome_seg_casco # qtd_dias_em_atraso_collection, prob_bad, prob_good, 
                                                   ))

require(mice)
#MICE WORKED FINE! SEVERAL MODELS DESIGNED WITH ALL VARIABLES AS PREDICTOR (PMM & LOGISTIC)

# running mice through all selected data above using pmm
setwd("D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)")
imp <- mice(select(all_data_filtered,-cod_hda), m=2, maxit=3, meth="pmm", seed=500)
# applying to database definided model
comp = complete(imp, 1)

# creating new variables to database: imputed 'propensao_net_user' and 'score_net_user'
all_data_filtered$IMP_ENR_propensao_netuser = comp$ENR_propensao_netuser
all_data_filtered$IMP_ENR_score_netuser = comp$ENR_score_netuser

# filtering rows that have missing 'propensao_netuser'
miss = all_data_filtered %>% filter(is.na(ENR_propensao_netuser)) %>% select(cod_contrato, ENR_score_netuser, ENR_propensao_netuser, IMP_ENR_score_netuser, IMP_ENR_propensao_netuser)
# checking if all filter above have imputed 'propensao_netuser'
table(is.na(miss$IMP_ENR_propensao_netuser))

# saving model and imputed database
save(imp, file = paste0("mice_imputation_serasa_",Sys.Date(),".RData"))
save(all_data_filtered, file = paste0("all_data_imputed_",Sys.Date(),".RData"))

# selecting the last score info because sometimes the same contract has 2 net user scores.
view = all_data_filtered %>% select(cod_contrato, qtd_dias_em_atraso, cod_hda, nome_hda, nome_est_loja, segmento, IMP_ENR_propensao_netuser) %>% distinct()
x = view[!duplicated(view$cod_contrato),]

# loading dealer attached to each collection agency
setwd("D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)")
load("assessoria_database_2019-05-20.RData")

# joining database with 'netuser' infos with collection agency attached to each dealer
digital_rule_db = left_join(x, assessoria.db, by = c("cod_hda", "nome_hda", "nome_est_loja", "segmento"))
digital_rule_db = digital_rule_db %>% select(cod_contrato, cod_hda, nome_hda, nome_est_loja, qtd_dias_em_atraso, segmento, IMP_ENR_propensao_netuser, assessoria)

# checking if there are any duplicated cod_contrato
table(duplicated(digital_rule_db$cod_contrato))
# checking if there are any missing collection agency
table(is.na(digital_rule_db$assessoria))

# filtering rows that have missing collection agency
missing.assessoria = digital_rule_db %>% filter(is.na(assessoria))

r######################################################
# reshuffing dealers that are without any assessoria #
######################################################

# loading collection score database
setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/1-30/MOT")
load("final_db_MOT.RData")
final_db_mot = final_db

setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/1-30/CAR")
load("final_db_CAR.RData")
final_db_car = final_db

final_db = bind_rows(final_db_car, final_db_mot)
rm(final_db_car, final_db_mot)

# adding collection score database by contract
bhb.final$score = NULL
bhb.final = left_join(bhb.final, final_db, by = "cod_contrato")

# creating a database containing dealers visions
dealers.vision = bhb.final %>% filter(qtd_dias_em_atraso > 0 & qtd_dias_em_atraso <= 20) %>%
  group_by(cod_hda, nome_hda, segmento, nome_est_loja) %>% 
  dplyr::summarise(median_atraso = median(qtd_dias_em_atraso, na.rm = TRUE),
                   median_balance = median(vlr_vencido, na.rm = TRUE),
                   median_score = median(score, na.rm = TRUE),
                   contracts = n()) %>%
  mutate(cut_median_atraso = cut(median_atraso, breaks = c(seq(0, 400, by = 5)), include.lowest = TRUE, dig.lab = 6),
         cut_median_balance = cut(median_balance, breaks = c(seq(0, 100000, by = 200)), include.lowest = TRUE, dig.lab = 6),
         cut_median_score = cut(median_score, breaks = c(seq(0, 1000, by = 50)), include.lowest = TRUE, dig.lab = 6),
         cut_contracts = cut(contracts, breaks = c(seq(0, 10000, by = 100)), include.lowest = TRUE, dig.lab = 6))

# couting outstading and delinquents by dealer, respectivelly
out = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(outstanding_hda = sum(qtd_itens, na.rm = TRUE))
del = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(delinquents_hda = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

# creating delinquency index by dealer
all = all %>% mutate(index_hda = delinquents_hda/outstanding_hda)

# joining dealers vision with delinquency index info
dealers.vision = left_join(dealers.vision, all)
# selecting dealers that has no collection agency vinculated
dealers.vision = dealers.vision %>% filter(paste0(cod_hda, nome_hda) %in% c(paste0(missing.assessoria$cod_hda, missing.assessoria$nome_hda)))

# cutting index into groups and creating id
dealers.vision = dealers.vision %>% mutate(cut_index = cut(index_hda, breaks = c(seq(0, 1, by = 0.05)), include.lowest = TRUE),
                                           id = rownames(dealers.vision))

set.seed(276)
# selecting randomly 50% of data to a collection agency, using as stratification 'cut_median_atraso, cut_median_balance, cut_median_score, cut_contracts, segmento'
assessoria.db.1 <- dealers.vision %>% group_by(cut_median_atraso, cut_median_balance, cut_median_score, cut_contracts, segmento) %>% sample_frac(0.5)
assessoria.db.1$assessoria <- "PASCHOALOTTO"
# assigning the remaining data to the other collection agency
assessoria.db.2 <- dealers.vision %>% filter(!id %in% c(assessoria.db.1$id))
assessoria.db.2$assessoria <- "FLEX"

# binding rows into one dataframe
assessoria.db.missing <- bind_rows(assessoria.db.1, assessoria.db.2)

# creating statistics by each collection agency
summar.shuffle =  assessoria.db.missing %>% group_by(assessoria, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                                        contracts = sum(contracts, na.rm  = TRUE),
                                                                                        states = n_distinct(nome_est_loja)) %>%
                                                                                      mutate(perc_dealers = dealers/n_distinct(assessoria.db$nome_hda),
                                                                                             perc_contracts = contracts/sum(assessoria.db$contracts, na.rm = TRUE))

summar.index =  assessoria.db.missing %>% group_by(assessoria, segmento) %>% dplyr::summarise(delinquents_assess = sum(delinquents_hda),
                                                                                              outstanding_assess = sum(outstanding_hda),
                                                                                              index = delinquents_assess/outstanding_assess)

summar = left_join(summar.shuffle, summar.index)
assessoria.db <- bind_rows(assessoria.db, assessoria.db.missing)


# saving new distribution
setwd("D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)")
save(assessoria.db, file = paste0("assessoria_database_",Sys.Date(),".RData"))

#######################
# creating final file #
#######################

digital_rule_db = left_join(x, assessoria.db, by = c("cod_hda", "nome_hda", "nome_est_loja", "segmento"))

missing.assessoria = digital_rule_db %>% filter(is.na(assessoria))

# creating a final dataframe with all contracts between 1 - 360 days overdue
digital_rule_db = digital_rule_db %>% mutate(IMP_ENR_propensao_netuser = ifelse(IMP_ENR_propensao_netuser == 1, "NET USER", "NOT NET USER"),
                                             update = Sys.Date())
digital_rule_db = digital_rule_db %>% select(cod_contrato, cod_hda, nome_hda, nome_est_loja, qtd_dias_em_atraso, segmento, IMP_ENR_propensao_netuser, assessoria, update)

# writing database in a .csv file
fwrite(digital_rule_db, file = paste0("base_digital_rule_",Sys.Date(),".csv"), sep = ";", dec = ",")

# filtering contracts within 8 and 17 days overdue
analysis = digital_rule_db %>% filter(qtd_dias_em_atraso >= 9 & qtd_dias_em_atraso <= 18)
analysis.2 = analysis %>% group_by(assessoria, IMP_ENR_propensao_netuser, segmento) %>% dplyr::summarise(contracts = n())

# writing analysis by collection agency in a .csv file
fwrite(analysis.2, file = "analysis_2.csv", sep = ";")

#################

paschoa_db = digital_rule_db %>% filter(assessoria == "PASCHOALOTTO" &  IMP_ENR_propensao_netuser == "NET USER")
setwd("R:/BHB/Régua Digital/2019/05 - Mai´19/CHATBOT_OMNICHANNELL/PASCHOALOTTO/5ª Semana")
fwrite(paschoa_db, file = "base_digital_rule_11_20 - PASCHOALOTTO 31_05_19.csv", sep = ";")
flex_db = digital_rule_db %>% filter(assessoria == "FLEX" &  IMP_ENR_propensao_netuser == "NET USER")
setwd("R:/BHB/Régua Digital/2019/05 - Mai´19/CHATBOT_OMNICHANNELL/FLEX/5ª Semana")
fwrite(flex_db, file = "base_digital_rule_11_20 - FLEX 31_05_19.csv", sep = ";")