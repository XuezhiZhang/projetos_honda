require(scorecard)
require(dplyr)
require(smbinning)
require(logiBin)
require(ggplot2)
require(qtlcharts)
require(caroline)
require(rlang)

#######################
#JOINING ALL DATABASES#
#######################

#JOINING DATABASES: PAYMENTS & IGB

# names(atrasos.count.by.ctr)[1] = "cod_contrato"
# bhb.fecha.oct.18 <- left_join(bhb.fecha.oct.18, atrasos.count.by.ctr)
# 
#STANDARDIZING DATABASE & CREATING NEW VARIABLES FOR MODELLING
bhb.final$`perc_pg_atr_1_10` = bhb.final$qtd_pg_atr_1_10_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_1_60` = bhb.final$qtd_pg_atr_1_60_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_11_60` = (bhb.final$qtd_pg_atr_11_30_em_1_ano + bhb.final$qtd_pg_atr_31_60_em_1_ano) / (bhb.final$qtd_pg_atr_em_1_ano)
bhb.final$`perc_pg_atr_61_360` = bhb.final$qtd_pg_atr_61_360_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_360_mais` = bhb.final$qtd_pg_atr_360_mais_em_1_ano/ bhb.final$qtd_pg_atr_em_1_ano

##################################
#VISUALIZATION OF PAYMENT PATTERN#
##################################

ggplot(bhb.final, aes(x=`perc_pg_atr_1_10`, fill=segmento)) + geom_density(alpha=.3) + ggtitle("Distribution of payment pattern by client [till 10 days]") +
  scale_x_continuous(name = "Percentage of payments delayed and paid within 1 - 10 days") +
  scale_y_continuous(name = "Clients' percentage")
ggplot(bhb.final, aes(x=`perc_pg_atr_11_60`, fill=segmento)) + geom_density(alpha=.3) + ggtitle("Distribution of payment pattern by client [11 - 60 days]") +
  scale_x_continuous(name = "Percentage of payments delayed and paid between 11 - 60") +
  scale_y_continuous(name = "Clients' percentage")
ggplot(bhb.final, aes(x=`perc_pg_atr_61_360`, fill=segmento)) + geom_density(alpha=.3) +
  scale_x_continuous(name = "Percentage of payments delayed and paid between 61 - 360") + ggtitle("Distribution of payment pattern by client [61 - 360 days]") +
  scale_y_continuous(name = "Clients' percentage")
ggplot(bhb.final, aes(x=`perc_pg_atr_1_60`, fill=segmento)) + geom_density(alpha=.3) +
  scale_x_continuous(name = "Percentage of payments delayed and paid between 1 - 60") + ggtitle("Distribution of payment pattern by client [1 - 60 days]") +
  scale_y_continuous(name = "Clients' percentage")

##########################
#CREATING TARGET VARIABLE#
#  & INFORMATION VALUE   #
##########################

mod_1_60 <- bhb.final %>% filter(qtd_dias_em_atraso >= 1 & qtd_dias_em_atraso <= 60) %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
         -cep_loja,-vlr_tx_banco,-vlr_taxa_cliente,-cod_tabela,-nome_placa,-analista_c,
         -data_contrato, -cod_hda, -vlr_vrg_antecipado, -vlr_vrg_diluido, -vlr_saldo_inicial,
         -vlr_liberado, -cod_inst_financ, -cod_marca, -data_risco_contabil, -vlr_seguri_casco,
         -vlr_tac, -data_ult_pgt, -data_vencimento, -data_nascimento_cli, -data_baixa, -cod_banco,
         -data_ult_alt, -proposta, -cod_plano, -vlr_subs_conc, -vlr_subs_marca, -vlr_taxa_subs_conc, -vlr_desp_finan,
         -car, -cod_pessoa, -data_ult_vencimento, -vlr_tx_subs_marc, -re, -data_ini_seguro, -data_fim_seguro,
         -data_prim_vencimento, -nome_renavam, -`for`, -contrato_cedido, -numero_contrato_cessao,
         -coobrigacao_sem_n, -qtd_pg_atr_em_1_ano, -valor_pg_atr_em_1_ano, -qtd_pg_atr_1_10_em_1_ano,  
         -vlr_pg_atr_1_10_em_1_ano, -qtd_pg_atr_11_30_em_1_ano, -vlr_pg_atr_11_30_em_1_ano, -qtd_pg_atr_1_30_em_1_ano,    
         -vlr_pg_atr_1_30_em_1_ano, -qtd_pg_atr_1_60_em_1_ano, -vlr_pg_atr_1_60_em_1_ano, -qtd_pg_atr_31_60_em_1_ano,   
         -vlr_pg_atr_31_60_em_1_ano, -qtd_pg_atr_61_360_em_1_ano, -vlr_pg_atr_61_360_em_1_ano, -qtd_pg_atr_360_mais_em_1_ano,
         -vlr_pg_atr_360_mais_em_1_ano, -qtd_pg_atr_11_60_em_1_ano, -vlr_pg_atr_11_60_em_1_ano, -qtd_pg_atr_1_360_em_1_ano, -vlr_pg_atr_1_360_em_1_ano,
         -perc_pg_atr_11_60, -perc_pg_atr_61_360, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas) %>%
  mutate(target = ifelse(
    tempo_contrato <= 1 |
      perc_pg_atr_1_60 <= 0.66, 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>%
  select(-perc_pg_atr_1_60, -tempo_contrato)

#verifying the percentage of 1 (bad payer)
table(mod_1_60$target)[2]/(table(mod_1_60$target)[1]+table(mod_1_60$target)[2])

mod_1_60_iv = iv(mod_1_60, y = "target") %>%
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

# base de 11_60 dias 

mod_11_60 <- bhb.final %>% filter(qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 60) %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
         -cep_loja,-vlr_tx_banco,-vlr_taxa_cliente,-cod_tabela,-nome_placa,-analista_c,
         -data_contrato, -cod_hda, -vlr_vrg_antecipado, -vlr_vrg_diluido, -vlr_saldo_inicial,
         -vlr_liberado, -cod_inst_financ, -cod_marca, -data_risco_contabil, -vlr_seguri_casco,
         -vlr_tac, -data_ult_pgt, -data_vencimento, -data_nascimento_cli, -data_baixa, -cod_banco,
         -data_ult_alt, -proposta, -cod_plano, -vlr_subs_conc, -vlr_subs_marca, -vlr_taxa_subs_conc, -vlr_desp_finan,
         -car, -cod_pessoa, -data_ult_vencimento, -vlr_tx_subs_marc, -re, -data_ini_seguro, -data_fim_seguro,
         -data_prim_vencimento, -nome_renavam, -`for`, -contrato_cedido, -numero_contrato_cessao,
         -coobrigacao_sem_n, -qtd_pg_atr_em_1_ano, -valor_pg_atr_em_1_ano, -qtd_pg_atr_1_10_em_1_ano,  
         -vlr_pg_atr_1_10_em_1_ano, -qtd_pg_atr_11_30_em_1_ano, -vlr_pg_atr_11_30_em_1_ano, -qtd_pg_atr_1_30_em_1_ano,    
         -vlr_pg_atr_1_30_em_1_ano, -qtd_pg_atr_1_60_em_1_ano, -vlr_pg_atr_1_60_em_1_ano, -qtd_pg_atr_31_60_em_1_ano,   
         -vlr_pg_atr_31_60_em_1_ano, -qtd_pg_atr_61_360_em_1_ano, -vlr_pg_atr_61_360_em_1_ano, -qtd_pg_atr_360_mais_em_1_ano,
         -vlr_pg_atr_360_mais_em_1_ano, -qtd_pg_atr_11_60_em_1_ano, -vlr_pg_atr_11_60_em_1_ano, -qtd_pg_atr_1_360_em_1_ano, -vlr_pg_atr_1_360_em_1_ano,
         -perc_pg_atr_1_60, -perc_pg_atr_61_360, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score) %>%
  mutate(target = ifelse(
                  tempo_contrato <= 1 |
                  perc_pg_atr_11_60 <= 0.75, 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>% 
  select(-perc_pg_atr_11_60, -tempo_contrato)

mod_11_60_2W = mod_11_60 %>% filter(segmento == "MOT")
mod_11_60_4W = mod_11_60 %>% filter(segmento == "CAR")

rm(mod_11_60)
#verifying the percentage of 1 (bad payer)
table(mod_11_60_2W$target)[2]/(table(mod_11_60_2W$target)[1]+table(mod_11_60_2W$target)[2])
table(mod_11_60_4W$target)[2]/(table(mod_11_60_4W$target)[1]+table(mod_11_60_4W$target)[2])

mod_11_60_2W_iv = iv(dplyr::select(mod_11_60_2W, -cod_contrato), y = "target") %>%
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

mod_11_60_4W_iv = iv(dplyr::select(mod_11_60_4W, -cod_contrato), y = "target") %>%
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

#writing archives with iv for model 11_60
#fwrite(mod_11_60_iv, "mod_11_60_iv.csv")

# base 61_360 dias
mod_61_360 <- bhb.final %>% filter(qtd_dias_em_atraso >= 61 & qtd_dias_em_atraso <= 360) %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
         -cep_loja,-vlr_tx_banco,-vlr_taxa_cliente,-cod_tabela,-nome_placa,-analista_c,
         -data_contrato, -cod_hda, -vlr_vrg_antecipado, -vlr_vrg_diluido, -vlr_saldo_inicial,
         -vlr_liberado, -cod_inst_financ, -cod_marca, -data_risco_contabil, -vlr_seguri_casco,
         -vlr_tac, -data_ult_pgt, -data_vencimento, -data_nascimento_cli, -data_baixa, -cod_banco,
         -data_ult_alt, -proposta, -cod_plano, -vlr_subs_conc, -vlr_subs_marca, -vlr_taxa_subs_conc, -vlr_desp_finan,
         -car, -cod_pessoa, -data_ult_vencimento, -vlr_tx_subs_marc, -re, -data_ini_seguro, -data_fim_seguro,
         -data_prim_vencimento, -nome_renavam, -`for`, -contrato_cedido, -numero_contrato_cessao,
         -coobrigacao_sem_n, -qtd_pg_atr_em_1_ano, -valor_pg_atr_em_1_ano, -qtd_pg_atr_1_10_em_1_ano,  
         -vlr_pg_atr_1_10_em_1_ano, -qtd_pg_atr_11_30_em_1_ano, -vlr_pg_atr_11_30_em_1_ano, -qtd_pg_atr_1_30_em_1_ano,    
         -vlr_pg_atr_1_30_em_1_ano, -qtd_pg_atr_1_60_em_1_ano, -vlr_pg_atr_1_60_em_1_ano, -qtd_pg_atr_31_60_em_1_ano,   
         -vlr_pg_atr_31_60_em_1_ano, -qtd_pg_atr_61_360_em_1_ano, -vlr_pg_atr_61_360_em_1_ano, -qtd_pg_atr_360_mais_em_1_ano,
         -vlr_pg_atr_360_mais_em_1_ano, -qtd_pg_atr_11_60_em_1_ano, -vlr_pg_atr_11_60_em_1_ano, -qtd_pg_atr_1_360_em_1_ano, -vlr_pg_atr_1_360_em_1_ano,
         -perc_pg_atr_1_60, -perc_pg_atr_11_60, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score,
         -pnad_versao, -pnad_ano, -descricao_uf) %>%
  mutate(target = ifelse(
                  tempo_contrato <= 1 |
                  perc_pg_atr_61_360 <= 0.75, 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>%
  select(-perc_pg_atr_61_360, -tempo_contrato)

mod_61_360_2W = mod_61_360 %>% filter(segmento == "MOT")
mod_61_360_4W = mod_61_360 %>% filter(segmento == "CAR")

rm(mod_61_360)
#verifying the percentage of 1 (bad payer)
table(mod_61_360_2W$target)[2]/(table(mod_61_360_2W$target)[1]+table(mod_61_360_2W$target)[2])
table(mod_61_360_4W$target)[2]/(table(mod_61_360_4W$target)[1]+table(mod_61_360_4W$target)[2])

mod_61_360_2W_iv = iv(dplyr::select(mod_61_360_2W, -cod_contrato), y = "target") %>%
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

mod_61_360_4W_iv = iv(dplyr::select(mod_61_360_4W, -cod_contrato), y = "target") %>%
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

rm(mod_11_60, mod_61_360)

names(databases)

train_databases = list(mod_11_60_2W, mod_11_60_4W, mod_61_360_2W, mod_61_360_4W)
rm(mod_11_60_2W, mod_11_60_4W, mod_61_360_2W, mod_61_360_4W)

#writing archives with iv for model 61_360
#fwrite(mod_11_60_iv, "mod_61_360_iv.csv")

#interactive correlations between all numeric variables
iplotCorr(select_if(mod_11_60, is.numeric), reorder = TRUE)
iplotCorr(select_if(mod_61_360, is.numeric), reorder = TRUE)

#save(bhb.final.db.sep.18, file = "bhb_final_db_sep'18.RData")

set.seed(8)

###################
#BINNING VARIABLES#
###################

train_data = list(train_data_11_60_2W, train_data_11_60_4W,
                  train_data_61_360_2W, train_data_61_360_4W)

bins_mod_11_60_2W_train = woebin(dplyr::select(train_data_11_60_2W, -cod_contrato), y = "target")
bins_mod_11_60_2W_test = woebin(dplyr::select(test_data_11_60_2W, -cod_contrato), y = "target")

bins_mod_11_60_4W_train = woebin(dplyr::select(train_data_11_60_4W, -cod_contrato), y = "target")
bins_mod_11_60_4W_test = woebin(dplyr::select(test_data_11_60_4W, -cod_contrato), y = "target")

#view all plots
woebin_plot(bins_mod_11_60_2W_train)

#view plots separetely
woebin_plot(bins.mod.11.60.tree$vlr_renda_mensal_cli)
woebin_plot(bins.mod.11.60.chim$vlr_renda_mensal_cli)

woebin_plot(bins.mod.11.60.tree$tempo_contrato)
woebin_plot(bins.mod.11.60.chim$tempo_contrato)

woebin_plot(bins.mod.11.60.tree$parcela_atual)
woebin_plot(bins.mod.11.60.chim$parcela_atual)

woebin_plot(bins.mod.11.60.tree$porc_parc_pg_contr)
woebin_plot(bins.mod.11.60.chim$porc_parc_pg_contr)

woebin_plot(bins.mod.11.60.tree$profissao_cli)
woebin_plot(bins.mod.11.60.tree$cod_plano)

breaks_list = list(tipo_pessoa = c("F", "J"),
                   situacao_produto = c("N", "S"),
                   nome_marca = c("CHEVROLET", "CITROEN", "FIAT", "FORD",      
                                  "HONDA", "HYUNDAI", "JEEP", "KIA",       
                                  "MITSUBISHI", "NISSAN", "PEUGEOT", "RENAULT",   
                                  "SSANGYONG" , "TOYOTA", "VOLKSWAGEN", "YAMAHA"),
                   status_contabil = c("CRE", "NML", "PRE", "RMT"),
                   tipo_cc = c("ALTA%,%missing", "MEDIA%,%BAIXA"))

##################

train_11_60_2W_woe = woebin_ply(train_data_11_60_2W, bins_mod_11_60_2W_train)
train_11_60_4W_woe = woebin_ply(train_data_11_60_4W, bins_mod_11_60_4W_train)
test_11_60_2W_woe = woebin_ply(test_data_11_60_2W, bins_mod_11_60_2W_test)
test_11_60_4W_woe = woebin_ply(test_data_11_60_4W, bins_mod_11_60_4W_test)

###################

mod_11_60 %<>% mutate_at(funs(factor(.))) %>% sapply(levels)

require(psych)
x <- corr.test(select_if(mod_11_60, is.numeric))
x <- data.frame(x$r)

require(broom)
require(logiBin)

x = logiBin::getBins(mod_11_60, "target", (select_if(mod_11_60, is.numeric)))
                     
bins_df = data.table::rbindlist(bins.mod.11.60.tree)
woe_11_60 = woebin_ply(x, bins.mod.11.60.tree)

train.data.11.60 = var_filter(train.data.11.60, "target", return_rm_reason = TRUE, iv_limit = 0.02)
x = data.frame(train.data.11.60$dt)
y = data.frame(train.data.11.60$rm)

#############

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)


setdiff(train_11_60_2W_woe, test_11_60_2W_woe)
setdiff(train_11_60_4W_woe, test_11_60_4W_woe)

load("mod_11_60.RData")

