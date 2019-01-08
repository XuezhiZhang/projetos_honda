require(dplyr)
require(ggplot2)

bhb.final = bhb.fecha
bhb.final = left_join(bhb.final, original.cob, by = "cod_contrato")

out = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(nome_est_loja, loja, segmento) %>% dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(nome_est_loja, loja, segmento) %>% dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

fwrite(all, file = "index_by_state.csv", sep = ";", dec = ",")

index_valid  = all %>% dplyr::group_by(segmento) %>% dplyr::summarise(outstanding = sum(outstanding, na.rm = TRUE),
                                                                      delinquents = sum(delinquents, na.rm = TRUE),
                                                                      index = delinquents/outstanding)

############
#clustering#
############

median = all %>% dplyr::group_by(nome_est_loja, segmento) %>% dplyr::summarise(median.index = median(index))
join = left_join(all, median, by = c("nome_est_loja", "segmento"))

join$flag = ifelse(join$index > join$median.index, "index > median", ifelse(join$index < join$median.index, "index < median", "equal"))
join$id = seq.int(nrow(join))
set.seed(123)
id1 = join %>% group_by(flag, segmento) %>% sample_frac(0.5)
id1 = c(id1$id)

join$assessoria = ifelse(join$id %in% id1, "PASCHOALOTTO", "FLEX CONTACT")

index_by_sample = join %>% group_by(assessoria, segmento) %>% summarise(outstanding = sum(outstanding, na.rm = TRUE),
                                                           delinquents = sum(delinquents, na.rm = TRUE),
                                                           index = delinquents/outstanding,
                                                           count = n())

count = join %>% group_by(segmento, nome_est_loja, assessoria) %>% summarise(delinquents = sum(delinquents))
count2 =  join %>% group_by(segmento, nome_est_loja) %>% summarise(delinquents.state = sum(delinquents))

count = left_join(count, count2) %>% mutate(prop = delinquents/delinquents.state)
count$version = "new"

ggplot(data=count, aes(x=nome_est_loja, y=delinquents, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(title = "Digital Rule | Balanced distribution of delinquents by dealer, state and segment",
       fill = "", x = "dealer state", y = "delinquents (%)", 
       caption = "Source: IGB | november 2018 | active contracts.") + 
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("#999999", "#E69F00"))

#############################

mot = fread("2W.txt", sep = "\t", dec = ",", colClasses = c("Contrato" = "character"))
car = fread("4W.txt", sep = "\t", dec = ",", colClasses = c("Contrato" = "character"))

mot.select = mot %>% select(Contrato, ASSESSORIA)
car.select = car %>% select(Contrato, ASSESSORIA)

original.cob = bind_rows(mot.select, car.select) %>% rename("cod_contrato" = "Contrato",
                                                            "assessoria" = "ASSESSORIA")

count.orig = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% 
                           group_by(segmento, nome_est_loja, assessoria) %>% summarise(delinquents = sum(qtd_itens, na.rm = TRUE))
count2.orig =  bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% 
                             group_by(segmento, nome_est_loja) %>% summarise(delinquents.state = sum(qtd_itens, na.rm = TRUE))

count.orig = left_join(count.orig, count2.orig) %>% mutate(prop = delinquents/delinquents.state)

count.orig$version = "actual"

ggplot(data=count.orig, aes(x=nome_est_loja, y=delinquents, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(title = "Digital Rule | Balanced distribution of delinquents by dealer, state and segment",
       fill = "", x = "dealer state", y = "delinquents (%)", 
       caption = "Source: IGB | november 2018 | active contracts.") + 
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Paired")

out.orig = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(assessoria, segmento) %>% dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del.orig = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(assessoria, segmento) %>% dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all.orig = full_join(out.orig, del.orig)
all.orig = all.orig %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all.orig = all.orig %>% mutate(index = delinquents/outstanding)

index_by_sample_orig = all.orig %>% group_by(assessoria, segmento) %>% summarise(outstanding = sum(outstanding, na.rm = TRUE),
                                                                    delinquents = sum(delinquents, na.rm = TRUE),
                                                                    index = delinquents/outstanding,
                                                                    count = n())


counts_merge = bind_rows(count, count.orig)


pic = ggplot(data=counts_merge, aes(x=nome_est_loja, y=delinquents, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") + theme_bw() +
  facet_wrap(~version+segmento) + coord_flip() +
  labs(fill = "", x = "dealer state", y = "delinquents (%)", 
       caption = "Source: IGB | november 2018 | active contracts.") + 
  theme(legend.position="bottom") +
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#934607"))
  scale_fill_brewer(palette="Paired")

fwrite(index_by_sample, file = "index_by_advisory_new.csv", sep = ";", dec = ",")
fwrite(index_by_sample_orig, file = "index_by_advisory_orig.csv", sep = ";", dec = ",")
fwrite(join, file = "dealers_by_advisory.csv", sep = ";", dec = ",")


filename=paste0("D:/Users/sb044936/Desktop/Databases/Dist_plot.tiff")
tiff(filename, units="in", width=12, height=8, res=500)
print(pic)
dev.off()
