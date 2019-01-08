require(dplyr)

load("bhb_final.RData")
output.df <- data.frame()

counter = 0
teste = function(base, segment){
  for(j in seq(0.05,0.95,0.05)){
    db = subset(base, base$segmento == segment) %>% 
    mutate(target =  ifelse(tempo_contrato <= 1 |
                            perc_pg_atr_61_360 <= j,
                            1, 0))
    perc = table(db$target)[2]/(table(db$target)[1]+table(db$target)[2])
    counter = counter + 1
    output.df[counter, "loop"] <- counter
    output.df[counter, "perc"] <- j
    output.df[counter, "bad_dist"] <- perc
    output.df[counter, "segment"] <- segment
  }
  return(output.df)}

output_11_60_car = teste(mod_11_60, "CAR")
output_11_60_mot = teste(mod_11_60, "MOT")
output_61_360_car = teste(mod_61_360, "CAR")
output_61_360_mot = teste(mod_61_360, "MOT")

##############

filter_out = output_61_360_mot %>% filter(perc >= 0.35 & perc <= 0.75)
cutoff = filter_out$perc[which.min(abs(0.5-filter_out$bad_dist))]

mod_11_60.car = mod_11_60 %>% filter(segmento == "CAR") %>%
                              mutate(target = ifelse(tempo_contrato <= 1 |
                                                     perc_pg_atr_11_60 <= cutoff, 1, 0)) #%>%
                              #mutate(target = ifelse(is.na(target), 1, target)) #%>% 
                              #select(-perc_pg_atr_11_60, -tempo_contrato)

mod_11_60.mot = mod_11_60 %>% filter(segmento == "MOT") %>%
                              mutate(target = ifelse(tempo_contrato <= 1 |
                                                     perc_pg_atr_11_60 <= cutoff, 1, 0)) #%>%
                              #mutate(target = ifelse(is.na(target), 1, target)) #%>% 
                              #select(-perc_pg_atr_11_60, -tempo_contrato)

mod_11_60 = bind_rows(mod_11_60.car, mod_11_60.mot)

mod_61_360.car = mod_61_360 %>% filter(segmento == "CAR") %>%
                                       mutate(target = ifelse(tempo_contrato <= 1 |
                                                              perc_pg_atr_61_360 <= cutoff, 1, 0)) #%>%
                                       #mutate(target = ifelse(is.na(target), 1, target)) #%>% 
                                       #select(-perc_pg_atr_61_360, -tempo_contrato)

mod_61_360.mot = mod_61_360 %>% filter(segmento == "MOT") %>%
                                       mutate(target = ifelse(tempo_contrato <= 1 |
                                                              perc_pg_atr_61_360 <= cutoff, 1, 0)) #%>%
                                       #mutate(target = ifelse(is.na(target), 1, target)) #%>% 
                                       #select(-perc_pg_atr_61_360, -tempo_contrato)

mod_61_360 = bind_rows(mod_61_360.car, mod_61_360.mot)

##################################

mod_11_60 <- subset(bhb.final, qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 60,
                    select = c(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
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
                               -perc_pg_atr_1_60, -perc_pg_atr_61_360, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score,
                               -pnad_versao, -pnad_ano, -descricao_uf))
mod_11_60$perc_pg_atr_11_60 = replace(mod_11_60$perc_pg_atr_11_60, is.na(mod_11_60$perc_pg_atr_11_60), 0)
  
mod_61_360 <- subset(bhb.final, qtd_dias_em_atraso >= 61 & qtd_dias_em_atraso <= 360, 
                     select = c(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
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
                               -pnad_versao, -pnad_ano, -descricao_uf))
mod_61_360$perc_pg_atr_61_360 = replace(mod_61_360$perc_pg_atr_61_360, is.na(mod_61_360$perc_pg_atr_61_360), 0)
