require(lubridate)
require(dplyr)
require(tidyr)

setwd("~/IGB/Histórico Fechamento/2017")
load("train_test_201706_201712.RData")
setwd("~/IGB/Histórico Fechamento/2018")
load("train_test_201801_201805.RData")

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("target_201806.RData")
load("delay_count_by_contr_until_201805.RData")

min = 1
max = 30

train.test.201706.201712 = train.test.201706.201712 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                                     qtd_dias_em_atraso >= min & 
                                                                     qtd_dias_em_atraso <= max)

train.test.201801.201805 = train.test.201801.201805 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                                 qtd_dias_em_atraso >= min & 
                                                                 qtd_dias_em_atraso <= max)

train.test.201706.201805 = bind_rows(train.test.201706.201712, train.test.201801.201805); rm(train.test.201706.201712, train.test.201801.201805)

train.test.201706.201805 = train.test.201706.201805 %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                                               list(~as.Date(as.character(.), 
                                                                             format = "%d/%m/%Y")))

train.test.201706.201805 = left_join(train.test.201706.201805, count.all)
train.test.201706.201805 = left_join(train.test.201706.201805, target.201806)
train.test.201706.201805 = train.test.201706.201805 %>% filter(!is.na(target))

train.test = train.test.201706.201805

train.test$vlr_renda_mensal_cli = ifelse(train.test$vlr_renda_mensal_cli == 1, NA, 
                                                       train.test$vlr_renda_mensal_cli) 

train.test$max_dif_entre_pgto = as.numeric(train.test$max_dif_entre_pgto)

# train.test.201607.201706$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), train.test$data_nascimento), "years"))
train.test$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date(as.character("2018-05-31"), format = "%Y-%m-%d"), train.test$data_contrato), "years"))
train.test$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date(as.character("2018-05-31"), format = "%Y-%m-%d"), train.test$data_contrato), "months"))
train.test$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date(as.character("2018-05-31"), format = "%Y-%m-%d"), train.test$data_ult_pgt), "days"))

train.test$`perc_parc_pagas` = train.test$qtd_parc_pagas / (train.test$qtd_parc_pagas + train.test$qtd_parc_restantes)
train.test$`perc_vnc_finan` = train.test$vlr_vencido / train.test$vlr_total_financiado
train.test$`perc_vnc_renda` = train.test$vlr_vencido / train.test$vlr_renda_mensal
train.test$`perc_vnc_bens` = train.test$vlr_vencido / train.test$vlr_total_bens
train.test$`perc_ult_pgt_parc` = train.test$vlr_ult_pgt / train.test$vlr_parcela
train.test$`perc_a_vencer_finan` = train.test$vlr_a_vencer / train.test$vlr_total_financiado

train.test$`perc_pg_atr_1_10` = train.test$qtd_pg_atr_1_10 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_1_15` = train.test$qtd_pg_atr_1_15 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_16_30` = train.test$qtd_pg_atr_16_30/ train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_1_60` = train.test$qtd_pg_atr_1_60 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_11_60` = (train.test$qtd_pg_atr_11_30 + train.test$qtd_pg_atr_31_60) / (train.test$qtd_pg_atr_12_meses)
train.test$`perc_pg_atr_61_360` = train.test$qtd_pg_atr_61_360 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_360_mais` = train.test$qtd_pg_atr_360_mais/ train.test$qtd_pg_atr_12_meses

train.test$`perc_parc_renda` = train.test$vlr_parcela/train.test$vlr_renda_mensal_cli
train.test$`perc_pg_finan` = (train.test$vlr_parcela*train.test$qtd_parc_pagas)/train.test$vlr_total_financiado

# 3 above lines are generating -inf values :(
train.test$perc_pg_atr_12_1 = train.test$qtd_pg_atr_12_meses/train.test$qtd_pg_atr_1_mes
train.test$perc_pg_atr_7_1 = train.test$qtd_pg_atr_7_meses/train.test$qtd_pg_atr_1_mes
train.test$perc_pg_atr_4_1 = train.test$qtd_pg_atr_4_meses/train.test$qtd_pg_atr_1_mes

train.test$tempo_ate_primeiro_atr = as.integer(train.test$data_primeiro_atraso - train.test$data_contrato)

train.test = train.test %>% mutate_at(vars( qtd_pg_atr_1_mes, vlr_pg_atr_1_mes,      
                                                                       qtd_pg_atr_4_meses, vlr_pg_atr_4_meses, qtd_pg_atr_7_meses, 
                                                                       vlr_pg_atr_7_meses, qtd_pg_atr_12_meses, vlr_pg_atr_12_meses,   
                                                                       qtd_pg_1_mes, valor_pg_1_mes, qtd_pg_4_meses,       
                                                                       valor_pg_4_meses, qtd_pg_7_meses, valor_pg_7_meses,      
                                                                       qtd_pg_12_meses, valor_pg_12_meses, qtd_pg_atr_1_10,
                                                                       vlr_pg_atr_1_10, qtd_pg_atr_1_15, vlr_pg_atr_1_15, 
                                                                       qtd_pg_atr_16_30, vlr_pg_atr_16_30,
                                                                       qtd_pg_atr_1_30, vlr_pg_atr_1_30, 
                                                                       qtd_pg_atr_1_60,qtd_pg_atr_360_mais, vlr_pg_atr_360_mais,
                                                                       vlr_pg_atr_1_60, qtd_pg_atr_11_30, vlr_pg_atr_11_30, 
                                                                       qtd_pg_atr_11_60, qtd_pg_atr_31_60, vlr_pg_atr_31_60, 
                                                                       qtd_pg_atr_61_360, vlr_pg_atr_61_360, qtd_pg_atr_1_360, 
                                                                       vlr_pg_atr_1_360), list(~replace(., is.na(.), 0)))

# train.test = train.test %>% mutate_at(vars(starts_with("perc_")), list(~replace(., is.na(.), 0)))

db_to_predict = train.test %>% select(-c(cpf_cnpj, data_safra, 
                                         data_contrato, data_primeiro_atraso, 
                                         data_ult_pgt, data_vencimento, status_contrato, 
                                         num_chassi, tabela, assessoria, assessoria_cob, 
                                         cod_hda, Cod))

segment = "MOT"
segment = "CAR"
sampling = "ROSE"
sampling = "SMOTE"
sampling = "OVER"

preparing(db_to_predict, segment, sampling)
modelling(dbs, segment)

############################

teste = teste %>% filter(!is.na(target))
db_to_predict = teste %>% select(-c(cod_contrato))

segment = "MOT"
segment = "CAR"

preparing(db_to_predict, segment, sampling)
modelling(dbs, segment)
writing(lasso.model, selected_var, probs_by_contract, imp, desired_model, segment, perf_stats1, perf_stats2)


setwd("~/IGB/Histórico Fechamento/2018")
load("train_test_201807_201812.RData")

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("target_201901.RData")
load("delay_count_by_contr_until_201812 (valid).RData")

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/", min, "-", max, "/", segment))
load("bins_CAR_2019-05-03.RData")
load("selected_var_CAR_2019-05-03.RData")
load("lasso_model_CAR_2019-05-03.RData")

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/", min, "-", max, "/", segment))
load("bins_MOT_2019-04-29.RData")
load("selected_var_MOT_2019-04-29.RData")
load("lasso_model_MOT_2019-04-29.RData")

min = 31
max = 60

train.test.201807.201812 = train.test.201807.201812 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                                 qtd_dias_em_atraso >= min & 
                                                                 qtd_dias_em_atraso <= max)

train.test.201807.201812 = train.test.201807.201812 %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                                                         list(~as.Date(as.character(.), 
                                                                                       format = "%d/%m/%Y")))

train.test.201807.201812 = left_join(train.test.201807.201812, count.all)
train.test.201807.201812 = left_join(train.test.201807.201812, target.201901)
train.test.201807.201812 = train.test.201807.201812 %>% filter(!is.na(target))

train.test = train.test.201807.201812

train.test$vlr_renda_mensal_cli = ifelse(train.test$vlr_renda_mensal_cli == 1, NA, 
                                         train.test$vlr_renda_mensal_cli) 

train.test$max_dif_entre_pgto = as.numeric(train.test$max_dif_entre_pgto)

# train.test.201607.201706$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), train.test$data_nascimento), "years"))
train.test$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date(as.character("2018-12-31"), format = "%Y-%m-%d"), train.test$data_contrato), "years"))
train.test$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date(as.character("2018-12-31"), format = "%Y-%m-%d"), train.test$data_contrato), "months"))
train.test$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date(as.character("2018-12-31"), format = "%Y-%m-%d"), train.test$data_ult_pgt), "days"))

train.test$`perc_parc_pagas` = train.test$qtd_parc_pagas / (train.test$qtd_parc_pagas + train.test$qtd_parc_restantes)
train.test$`perc_vnc_finan` = train.test$vlr_vencido / train.test$vlr_total_financiado
train.test$`perc_vnc_renda` = train.test$vlr_vencido / train.test$vlr_renda_mensal
train.test$`perc_vnc_bens` = train.test$vlr_vencido / train.test$vlr_total_bens
train.test$`perc_ult_pgt_parc` = train.test$vlr_ult_pgt / train.test$vlr_parcela
train.test$`perc_a_vencer_finan` = train.test$vlr_a_vencer / train.test$vlr_total_financiado

train.test$`perc_pg_atr_1_10` = train.test$qtd_pg_atr_1_10 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_1_15` = train.test$qtd_pg_atr_1_15 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_16_30` = train.test$qtd_pg_atr_16_30/ train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_1_60` = train.test$qtd_pg_atr_1_60 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_11_60` = (train.test$qtd_pg_atr_11_30 + train.test$qtd_pg_atr_31_60) / (train.test$qtd_pg_atr_12_meses)
train.test$`perc_pg_atr_61_360` = train.test$qtd_pg_atr_61_360 / train.test$qtd_pg_atr_12_meses
train.test$`perc_pg_atr_360_mais` = train.test$qtd_pg_atr_360_mais/ train.test$qtd_pg_atr_12_meses

train.test$`perc_parc_renda` = train.test$vlr_parcela/train.test$vlr_renda_mensal_cli
train.test$`perc_pg_finan` = (train.test$vlr_parcela*train.test$qtd_parc_pagas)/train.test$vlr_total_financiado

#3 above lines are generating -inf values :(
train.test$perc_pg_atr_12_1 = train.test$qtd_pg_atr_12_meses/train.test$qtd_pg_atr_1_mes
train.test$perc_pg_atr_7_1 = train.test$qtd_pg_atr_7_meses/train.test$qtd_pg_atr_1_mes
train.test$perc_pg_atr_4_1 = train.test$qtd_pg_atr_4_meses/train.test$qtd_pg_atr_1_mes

train.test$tempo_ate_primeiro_atr = as.integer(train.test$data_primeiro_atraso - train.test$data_contrato)

train.test = train.test %>% mutate_at(vars( qtd_pg_atr_1_mes, vlr_pg_atr_1_mes,      
                                            qtd_pg_atr_4_meses, vlr_pg_atr_4_meses, qtd_pg_atr_7_meses, 
                                            vlr_pg_atr_7_meses, qtd_pg_atr_12_meses, vlr_pg_atr_12_meses,   
                                            qtd_pg_1_mes, valor_pg_1_mes, qtd_pg_4_meses,       
                                            valor_pg_4_meses, qtd_pg_7_meses, valor_pg_7_meses,      
                                            qtd_pg_12_meses, valor_pg_12_meses, qtd_pg_atr_1_10,
                                            vlr_pg_atr_1_10, qtd_pg_atr_1_15, vlr_pg_atr_1_15, 
                                            qtd_pg_atr_16_30, vlr_pg_atr_16_30,
                                            qtd_pg_atr_1_30, vlr_pg_atr_1_30, 
                                            qtd_pg_atr_1_60,qtd_pg_atr_360_mais, vlr_pg_atr_360_mais,
                                            vlr_pg_atr_1_60, qtd_pg_atr_11_30, vlr_pg_atr_11_30, 
                                            qtd_pg_atr_11_60, qtd_pg_atr_31_60, vlr_pg_atr_31_60, 
                                            qtd_pg_atr_61_360, vlr_pg_atr_61_360, qtd_pg_atr_1_360, 
                                            vlr_pg_atr_1_360), list(~replace(., is.na(.), 0)))

# train.test = train.test %>% mutate_at(vars(starts_with("perc_")), list(~replace(., is.na(.), 0)))

db_to_predict = train.test %>% select(-c(cpf_cnpj, data_safra, 
                                         data_contrato, data_primeiro_atraso, 
                                         data_ult_pgt, data_vencimento, status_contrato, 
                                         num_chassi, tabela, assessoria, assessoria_cob, 
                                         cod_hda, Cod))


fwrite(dbs$db_woe, file = "db_woe.csv", sep = ";", dec = ",")
fwrite(dbs$db_raw, file = "db_raw.csv", sep = ";", dec = ",")
fwrite(dbs$db_woe_valid, file = "db_woe_valid.csv", sep = ";", dec = ",")
