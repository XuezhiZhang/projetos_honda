after = fread("after_redistribution.txt", header = TRUE, sep = "\t")
before = fread("before_redistribution.txt", header = TRUE, sep = "\t")

all_redist = full_join(before, after, by = "CODIGO_ASSISTENCIA", suffix = c("_before", "_after"))

base.fecha.mar.filtered = bhb.fecha.01.mar.2019 %>% filter(status_contrato == "ATV")

out = base.fecha.mar.filtered %>% dplyr::filter(qtd_parc_restantes > 0 & 
                                         qtd_dias_em_atraso <= 360 & 
                                         situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% 
                           dplyr::group_by(nome_hda, segmento) %>% 
                           dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))

del = base.fecha.mar.filtered %>% dplyr::filter(qtd_parc_restantes > 0 & 
                                         qtd_dias_em_atraso >= 31 & 
                                         qtd_dias_em_atraso <= 360 & 
                                         situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% 
                           dplyr::group_by(nome_hda, segmento) %>% 
                           dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all_index = full_join(out, del)
all_index = all_index %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

all_index = all_index %>% mutate(index = delinquents/outstanding)

x = all_redist %>% filter(NOME_ESCRITORIO_before != NOME_ESCRITORIO_after)

names(all_index)[1] = "CONCESSIONARIA_before"
x = left_join(x, all_index, by = "CONCESSIONARIA_before")

y = all_redist %>% filter(NOME_ESCRITORIO_before != NOME_ESCRITORIO_after)

# names(all_index)[1] = "CONCESSIONARIA_before"
# y = left_join(x, all_index, by = "CONCESSIONARIA_before")
# 
# fwrite(x, file = "redistribution.csv", sep = ";", dec = ",")

