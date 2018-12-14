require(dplyr)

bhb.final = bhb.fecha

out = bhb.final %>% filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del = bhb.final %>% filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda) %>% summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

vlr_31_90 = bhb.final %>% filter(qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 90) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(vlr_vencido_31_90 = sum(vlr_vencido, na.rm = TRUE))
vlr_91_180 = bhb.final %>% filter(qtd_dias_em_atraso >= 91 & qtd_dias_em_atraso <= 180) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(vlr_vencido_91_180 = sum(vlr_vencido, na.rm = TRUE))
vlr_181_360 = bhb.final %>% filter(qtd_dias_em_atraso >= 181 & qtd_dias_em_atraso <= 360) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(vlr_vencido_181_360 = sum(vlr_vencido, na.rm = TRUE))
vlr_360_mais = bhb.final %>% filter(qtd_dias_em_atraso > 360) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(vlr_vencido_360_mais = sum(vlr_vencido, na.rm = TRUE))

vlr = full_join(vlr_31_90, vlr_91_180)
vlr = full_join(vlr, vlr_181_360)
vlr = full_join(vlr, vlr_360_mais)

geral = bhb.final %>% filter(qtd_parc_restantes > 0) %>% group_by(nome_est_loja, nome_mun_loja, nome_hda, segmento) %>% summarise(active_contracts = n(),
                                                                                           vlr_total_vencido = sum(vlr_vencido, na.rm = TRUE),
                                                                                           ticket_medio = vlr_total_vencido/active_contracts)

ticket_mun <- bhb.final %>% filter(qtd_parc_restantes > 0) %>% group_by(nome_mun_loja, segmento) %>% summarise(active_contracts_mun = n(),
                                                                            vlr_total_vencido_mun = sum(vlr_vencido, na.rm = TRUE),
                                                                            ticket_medio_mun = vlr_total_vencido_mun/active_contracts_mun)

final = full_join(all, vlr)
final = full_join(final, geral)
final = full_join(final, ticket_mun)

final = final %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

fwrite(final, file = "BHB - Concessionarias.csv", dec = ",", sep = ";")

ggplot(final, aes(x = index, fill = segmento)) + geom_density(alpha=.3) + 
  geom_vline(xintercept=c(1.95, 2.05, 14.5, 15.25),
             color=c("yellow", "red", "yellow", "red"), linetype="dashed", size=0.8) +
  scale_x_continuous(name = "Delinquency index (%)", limits = c(0,40), breaks = c(seq(0,40, by = 2.5))) +
  scale_y_continuous(name = "Dealers distribution") +
  ggtitle("BHB - Delinquency index by dealer")

z  = final %>% group_by(segmento) %>% summarise(outstanding = sum(outstanding, na.rm = TRUE),
                                                delinquents = sum(delinquents, na.rm = TRUE),
                                                index = delinquents/outstanding)

