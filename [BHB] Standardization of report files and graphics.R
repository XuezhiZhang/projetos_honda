setwd("D:/Users/sb044936/Desktop/Reshuffle")



resumo = db.2019 %>% group_by(cut_atraso) %>% summarise(amount_delay = sum(vlr_vencido, na.rm = TRUE))

db.2019 = db.2019 %>% mutate(cut_atraso = cut(qtd_dias_em_atraso, breaks = c(seq(0, 10000, by = 30))))


head(db.2019)


out = db.2019 %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(, segmento) %>% dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del = db.2019 %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(nome_est_loja, segmento) %>% dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

