require(dplyr)
require(data.table)
require(tidyr)

setwd("D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)")

first = fread("base_digital_rule_11_20.csv", dec = ",", colClasses = c("cod_contrato" = "character")) %>% filter(qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 20)
second = fread("base_digital_rule_2019-05-10.csv", dec= ",", colClasses = c("cod_contrato" = "character")) %>% filter(qtd_dias_em_atraso >= 8 & qtd_dias_em_atraso <= 17)

setwd("~/IGB/Histórico Fechamento/2019")
load("2019.RData")
levels(as.factor(db.2019$data_safra))

fecha_mar = db.2019 %>% filter(data_safra == "01/04/2019") %>% select(cod_contrato, assessoria)
fecha_abr = db.2019 %>% filter(data_safra == "03/05/2019") %>% select(cod_contrato, assessoria)

first_add = left_join(first, fecha_mar, by = "cod_contrato", suffix = c("_digitalrule", "_standard")) %>% filter(qtd_dias_em_atraso_digitalrule >= 11 & qtd_dias_em_atraso_digitalrule <= 20)
first_add_summary = first_add %>% group_by(IMP_ENR_propensao_netuser, segmento_digitalrule) %>% dplyr::summarise(renda = median(vlr_renda_mensal_cli, na.rm = TRUE),
                                                                                                                                        qtd_dias_em_atraso_digitalrule = median(qtd_dias_em_atraso_digitalrule, na.rm = TRUE),
                                                                                                                                        parcela_atual = median(parcela_atual, na.rm = TRUE),
                                                                                                                                        qtd_parc_pagas = median(qtd_parc_pagas, na.rm = TRUE),
                                                                                                                                        qtd_parc_atrasadas = median(qtd_parc_atrasadas, na.rm = TRUE),
                                                                                                                                        vlr_parcela = median(vlr_parcela, na.rm = TRUE),
                                                                                                                                        vlr_total_bens = median(vlr_total_bens, na.rm = TRUE),
                                                                                                                                        prazo_contrato = median(prazo_contrato, na.rm = TRUE),
                                                                                                                                        vlr_total_financiado = median(vlr_total_financiado, na.rm = TRUE))

first_add_summary = first_add_summary %>% mutate(id = paste0(segmento_digitalrule, IMP_ENR_propensao_netuser)) %>% select(-c(segmento_digitalrule, IMP_ENR_propensao_netuser))
first_add_summary = first_add_summary %>% select(-IMP_ENR_propensao_netuser)
x = gather(first_add_summary, key = "id", value = "median",
       "renda", "qtd_dias_em_atraso_digitalrule", "parcela_atual", "qtd_parc_pagas",                
       "qtd_parc_atrasadas", "vlr_parcela", "vlr_total_bens",                
       "prazo_contrato", "vlr_total_financiado")
first_add_summary = spread(first_add_summary, IMP_ENR_propensao_netuser, n)

second_add = left_join(first, fecha_abr, by = "cod_contrato", suffix = c("_digitalrule", "_standard")) %>% filter(qtd_dias_em_atraso >= 8 & qtd_dias_em_atraso <= 17)
second_add_summary = second_add %>% group_by(assessoria_standard, assessoria_digitalrule, IMP_ENR_propensao_netuser) %>% dplyr::summarise(n = n())

fwrite(first_add_summary, file = "first_add_summary.csv", sep = ";", dec = ",")

ggplot(data = first_add_summary, mapping = aes(x = year, y = n)) +
              geom_line() +
              facet_wrap(~ species_id)