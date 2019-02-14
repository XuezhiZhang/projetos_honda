require(dplyr)
require(data.table)
require(tidyr)
require(caret)

setwd("D:/Users/sb044936/Desktop/Databases")
base_serasa = fread("base_serasa.txt", sep = "\t", dec = ",", colClasses = c("cpf_cnpj" = "character"), na.strings = "")

base_serasa = base_serasa %>% select(ENR_mosaic, ENR_descricao_mosaic, ENR_score_netuser, ENR_propensao_netuser, 
                                     REC_fx_cons_cred, REC_fx_cred_ativos, REC_fx_score, REC_obito)

setwd("~/IGB/Daily IGB")
base_fecha_jan = fread("igb_daily_01_02.txt", header = TRUE, 
                       dec = ",", check.names = TRUE, 
                       colClasses = c("Contrato" = "character",
                                      "Cep" = "character",
                                      "Cep l" = "character",
                                      "CPF CNPJ" = "character"), na.strings = "")

x = base_serasa %>% group_by(ENR_descricao_mosaic) %>% dplyr::summarise(median_score_netuser = median(ENR_score_netuser, na.rm = TRUE))
z = all_data %>% group_by(nome_estado_cli) %>% dplyr::summarise(median_score_netuser = median(ENR_score_netuser, na.rm= TRUE), n = n())

formatting_database(base_fecha_jan)

bhb.final = bhb.final %>% filter(qtd_dias_em_atraso >= 30 & qtd_dias_em_atraso <= 360)
teste = anti_join(bhb.final, base_serasa, by = "cpf_cnpj")

all_data = left_join(bhb.final, base_serasa, by = "cpf_cnpj") %>% distinct()
all_data = left_join(all_data, all_contracts_scored, by = "cod_contrato", suffix = c("_credit", "_collection"))

all_data_filtered = all_data %>% select(select = -c(cpf_cnpj,tabela_neg,num_chassi,cep_digito_cli,cep_cli,nome_cliente,vlr_tx_anual_ctr,
                                                   cep_loja,vlr_tx_banco,vlr_taxa_cliente,cod_tabela,nome_placa,analista_c,
                                                   data_contrato, cod_hda, vlr_vrg_antecipado, vlr_vrg_diluido, vlr_saldo_inicial,
                                                   vlr_liberado, cod_inst_financ, cod_marca, data_risco_contabil, vlr_seguri_casco,
                                                   vlr_tac, data_ult_pgt, data_vencimento, data_nascimento_cli, data_baixa, cod_banco,
                                                   data_ult_alt, proposta, cod_plano, vlr_subs_conc, vlr_subs_marca, vlr_taxa_subs_conc, vlr_desp_finan,
                                                   car, cod_pessoa, data_ult_vencimento, vlr_tx_subs_marc, re, data_ini_seguro, data_fim_seguro,
                                                   data_prim_vencimento, nome_renavam, `for`, contrato_cedido, numero_contrato_cessao,
                                                   coobrigacao_sem_n, qtd_pg_atr_em_1_ano, valor_pg_atr_em_1_ano, qtd_pg_atr_1_10_em_1_ano,  
                                                   vlr_pg_atr_1_10_em_1_ano, qtd_pg_atr_11_30_em_1_ano, vlr_pg_atr_11_30_em_1_ano, qtd_pg_atr_1_30_em_1_ano,    
                                                   vlr_pg_atr_1_30_em_1_ano, qtd_pg_atr_1_60_em_1_ano, vlr_pg_atr_1_60_em_1_ano, qtd_pg_atr_31_60_em_1_ano,   
                                                   vlr_pg_atr_31_60_em_1_ano, qtd_pg_atr_61_360_em_1_ano, vlr_pg_atr_61_360_em_1_ano, qtd_pg_atr_360_mais_em_1_ano,
                                                   vlr_pg_atr_360_mais_em_1_ano, qtd_pg_atr_11_60_em_1_ano, vlr_pg_atr_11_60_em_1_ano, qtd_pg_atr_1_360_em_1_ano, vlr_pg_atr_1_360_em_1_ano,
                                                   perc_pg_atr_1_60, perc_pg_atr_1_10, perc_pg_atr_360_mais, vlr_seg_gara_estend, status_contrato, qtd_parcelas_pagas, score_credit,
                                                   pnad_versao, pnad_ano, descricao_uf, stat_model, stat_model_update, HDA_seg, HDA_valor_divida, HDA_dias_atraso, 
                                                   HDA_valor_parcela, HDA_numero_parcela, HDA_vlr_total_contrato, HDA_qtd_parc_atraso, TRAT_doc, ENR_logr_tipo1, 
                                                   ENR_logr_titulo1, ENR_logr_nome1, ENR_logr_numero1, ENR_logr_complemento1, ENR_endereco1, ENR_bairro1, 
                                                   ENR_cidade1, ENR_uf1, ENR_cep1, ENR_ddd1, ENR_ddd2, ENR_fone2, ENR_fonetipo2, ENR_ddd3, ENR_fone3, 
                                                   ENR_fonetipo3, ENR_ddd4, ENR_fone4, ENR_fonetipo4, qtd_dias_em_atraso_collection, prob_bad, prob_good, cod_contrato))

all_data_filtered = all_data %>% select_if(is.numeric) %>% select(-c(nome_renavam, TRAT_doc))

mod_11_60 = left_join(mod_11_60, base_serasa)



preproc <- preProcess(mod_11_60, method = "bagImpute")
all_data_imp <- predict(preproc, newdata = mod_11_60)

p = all_data %>% group_by(REC_fx_score, cluster) %>% dplyr::summarise(n = n()) #median_score = median(score.y, na.rm = TRUE))
p2 = spread(p, cluster, n)

p2 = p2 %>% select(-`<NA>`)
p2 = p2 %>% filter(!REC_fx_score %in% c("SEM SCORE", NA))

matrix = as.table(p2)[,-1]

chisq.test(table(all_data$REC_fx_score, all_data$cluster))
