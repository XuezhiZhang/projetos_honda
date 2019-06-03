# reading march database fechamento (4 files)

setwd("~/IGB/Histórico Fechamento/2019/Março")

files = list.files(pattern="*txt")
l = sapply(files, fread, sep = "\t", dec = ",", colClasses = c("Contrato" = "character",
                                                               "Dt_Safra" = "character",
                                                               "CPF_CNPJ" = "character"), check.names = TRUE,  na.strings = "")

db.2019 = l %>% bind_rows()

db.2019 = db.2019 %>% select(-c(V60, V61, V62, V63, Cod)) 
db.2019 = db.2019 %>% dplyr::rename("cod_contrato" = "Contrato",
                                    "data_contrato" = "Data_Ctr",
                                    "tipo_pessoa" = "T",
                                    "cpf_cnpj" = "CPF_CNPJ",
                                    "cod_hda" = "Cod_HDA",
                                    "nome_hda" = "Nome_HDA",
                                    "nome_mun_loja" = "Municipio_Loj",
                                    "nome_est_loja" = "Estado_Loj",
                                    "prazo_contrato" = "Prz",
                                    "vlr_tx_venda" = "Tx_Venda",
                                    "vlr_total_financiado" = "Vlr_Total_Financiado",
                                    "vlr_total_bens" = "Vlr_Total_Bens",
                                    "vlr_liberado" = "Valor_Liberado",
                                    "vlr_a_vencer" = "Valor_AVC",
                                    "vlr_vencido" = "Valor_VC",
                                    "qtd_itens" = "Qtd",
                                    "nome_marca" = "Marca",
                                    "nome_modelo" = "Modelo",
                                    "ano_fabr" = "Fabr",
                                    "ano_modelo" = "Mod",
                                    "status_contrato" = "Sta",
                                    "qtd_dias_em_atraso" = "Dias",
                                    "segmento" = "Seg",
                                    "data_ult_pgt" = "Dt_Ult_Pgt",
                                    "vlr_ult_pgt" = "Vlr_Ult_Pgto",
                                    "data_vencimento" = "Dt_Vencto",
                                    "vlr_parcela" = "Vlr_Pcl",
                                    "situacao_produto" = "Z",
                                    "tabela" = "TAb_Negocia",
                                    "vlr_renda_mensal_cli" = "Renda_Mensal",
                                    "profissao_cli" = "Profissao",
                                    #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                    "num_chassi" = "Chassi",
                                    "qtd_parcelas_pagas" = "Pcl_p",
                                    #"score" = "SCORE",                            
                                    "situacao_contrato" = "Sit",
                                    "faixa_atraso" = "FAIXA",
                                    "assessoria_cob" = "ASSESSORIA_COB",
                                    "assessoria" = "ASSESSORIA",
                                    #"assessoria_nova" = "NOVA.ASSESSORIA",
                                    "status_cob" = "STATUS",
                                    "ano_contrato" = "Ano_Data_Ctr",
                                    "mes_contrato" = "Mes_Data_Ctr",
                                    "agr_prazo" = "AGRUP_PRAZO",
                                    "agr_entrada" = "AGRUP_ENTRADA",
                                    "agr_retorno" = "AGRUP_RETORNO",
                                    "familia_bem" = "FAMILIA",
                                    "cc" = "CC",
                                    "estado_bem" = "CLASS_ESTADO_BEM",
                                    "motivo_inad" = "MOT_INADIMPLENCIA",
                                    "subs_marca" = "SUBS_MARCA",
                                    "vlr_subs_marca" = "Vlr_subs_Marca",
                                    "nome_regiao_cli" = "REGIAO",
                                    "qtd_parc_restantes" = "Qtd_Parc_restante",
                                    "parcela_atual" = "Parcela_Atual",
                                    "qtd_parc_atrasadas" = "Qtd_P_Atrasadas",
                                    "qtd_parc_pagas" = "Qtd_P_Pagas",
                                    "data_safra" = "Dt_Safra",
                                    "perc_entrada" = "Perc",
                                    "status_contabil" = "St")

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("delay_count_by_contr_until_2019-03.RData")

dealers.vision = march.db.out %>% filter(qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 30) %>%
  group_by(cod_hda, nome_hda, segmento, nome_est_loja) %>% 
  dplyr::summarise(median_atraso = median(qtd_dias_em_atraso, na.rm = TRUE),
                   median_balance = median(vlr_vencido, na.rm = TRUE),
                   median_score = median(score, na.rm = TRUE),
                   contracts = n()) %>%
                   mutate(cut_median_atraso = cut(median_atraso, breaks = c(seq(0, 400, by = 5)), include.lowest = TRUE),
                          cut_median_balance = cut(median_balance, breaks = c(seq(0, 100000, by = 200)), include.lowest = TRUE),
                          cut_median_score = cut(median_score, breaks = c(seq(0, 1000, by = 50)), include.lowest = TRUE),
                          cut_contracts = cut(contracts, breaks = c(seq(0, 10000, by = 100)), include.lowest = TRUE))

out = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(outstanding_hda = sum(qtd_itens, na.rm = TRUE))
del = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(delinquents_hda = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

all = all %>% mutate(index_hda = delinquents_hda/outstanding_hda)

dealers.vision = left_join(dealers.vision, all)
dealers.vision = dealers.vision %>% mutate(cut_index = cut(index_hda, breaks = c(seq(0, 1, by = 0.05)), include.lowest = TRUE))
                                    

# # inserting actual distribution of contracts for each collection agency
# share_cesec_mot_ini = 0.3659; share_cesec_car_ini = 0.3453
# share_paschoa_mot_ini = 0.2880; share_pachoa_car_ini = 0.3408
# share_flex_mot_ini = 0.2013; share_flex_car_ini = 0.3026
# share_localcred_mot_ini = 0.1448; share_localcred_car_ini = 0.0113
# 
# share_cesec_mot = share_cesec_mot_ini
# share_cesec_car = share_cesec_car_ini
# 
# # recreating the distribution for segment: car or mot
# left = 1 - share_cesec_mot_ini
# share_paschoa_mot = share_paschoa_mot_ini/left
# 
# left = 1 - share_cesec_car_ini
# share_paschoa_car = share_pachoa_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini) 
# share_flex_mot = share_flex_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini) 
# share_flex_car = share_flex_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini, share_flex_mot_ini) 
# share_localcred_mot = share_localcred_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini, share_flex_car_ini) 
# share_localcred_car = share_localcred_car_ini/left
# 
# # creating contract shuffle by cut_atraso, cut_balance and risk cluster
# set.seed(888)
# cesec_mot = march.db.out %>% filter(segmento == "MOT") %>% group_by(nome_hda) %>% sample_frac(share_cesec_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec_car = march.db.out %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec = bind_rows(cesec_mot, cesec_car)
# 
# paschoa_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# paschoa_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# paschoa = bind_rows(paschoa_mot, paschoa_car)
# 
# flex_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# flex_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# flex = bind_rows(flex_mot, flex_car)
# 
# localcred = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato, flex$cod_contrato),] %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# 
# cesec$assessoria = "CESEC"
# paschoa$assessoria = "PASCHOALOTTO"
# flex$assessoria = "FLEX"
# localcred$assessoria = "LOCALCRED"
# 
# all_labeled = bind_rows(cesec, paschoa, flex, localcred)

summar = march.db.out %>% filter(qtd_dias_em_atraso >= 1 & qtd_dias_em_atraso <= 360) %>% group_by(assessoria, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                               contracts = n(),
                                                               states = n_distinct(nome_est_loja)) %>%
                                                               mutate(perc_dealers = dealers/n_distinct(march.db.out$cod_hda),
                                                                      perc_contracts = contracts/nrow(march.db.out))

fwrite(summar, file = "summar_assess.csv", sep = ";", dec = ",")
# 
# 
# set.seed(20)
# clusters <- kmeans(, 5)
# 
# ##################
# # MANUAL SHUFFLE # 
# ##################
# 
# # inserting actual distribution of contracts for each collection agency
# share_cesec_mot_ini = 0.3659; share_cesec_car_ini = 0.3453
# share_paschoa_mot_ini = 0.2880; share_pachoa_car_ini = 0.3408
# share_flex_mot_ini = 0.2013; share_flex_car_ini = 0.3026
# share_localcred_mot_ini = 0.1448; share_localcred_car_ini = 0.0113
# 
# share_cesec_mot = share_cesec_mot_ini
# share_cesec_car = share_cesec_car_ini
# 
# # recreating the distribution for segment: car or mot
# left = 1 - share_cesec_mot_ini
# share_paschoa_mot = share_paschoa_mot_ini/left
# 
# left = 1 - share_cesec_car_ini
# share_paschoa_car = share_pachoa_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini) 
# share_flex_mot = share_flex_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini) 
# share_flex_car = share_flex_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini, share_flex_mot_ini) 
# share_localcred_mot = share_localcred_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini, share_flex_car_ini) 
# share_localcred_car = share_localcred_car_ini/left
# 
# set.seed(888)
# cesec_mot = dealers.vision %>% dplyr::filter(segmento == "MOT") %>% dplyr::group_by(cut_median_atraso, cut_median_balance, cut_median_score, cut_contracts) %>% 
#                                dplyr::sample_n(150) %>% select(nome_hda, nome_est_loja, segmento, 
#                                                                    contracts, median_atraso, median_balance, 
#                                                                    median_score, contracts); sum(cesec_mot$contracts); sum(cesec_mot$contracts)/sum(dealers.vision$contracts)
# 
# cesec_car = march.db.out %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec = bind_rows(cesec_mot, cesec_car)

####################
# inserting actual distribution of contracts for each collection agency
# outstanding + 1 (dias atrasados > 0)
share_cese_mot = 27578; share_cese_car = 1440
share_pasc_mot = 21702; share_pasc_car = 1421
share_flex_mot = 15171; share_flex_car = 1262
share_loca_mot = 10913; share_loca_car = 47

# outstanding (dias atrasados >= 0)
share_cese_mot = 106095; share_cese_car = 19461
share_pasc_mot = 99741; share_pasc_car = 26346
share_flex_mot = 63161; share_flex_car = 22836
share_loca_mot = 41996; share_loca_car = 1322

# outstanding (dias atrasados >= 11 <= 30)
share_cese_mot = 6472; share_cese_car = 443
share_pasc_mot = 5390; share_pasc_car = 407
share_flex_mot = 3392; share_flex_car = 358
share_loca_mot = 2; share_loca_car = 16

assessoria.db = data.frame()
list_files = list()

assessorias = c("CESEC", "PASCHOALOTTO", "FLEX", "LOCALCRED")
segments = c("MOT", "CAR")

dealers.vision.filt = dealers.vision %>% select(cod_hda, nome_hda, segmento, contracts, cut_median_atraso, cut_median_balance, cut_median_score, cut_index, nome_est_loja, median_score, outstanding_hda, delinquents_hda)
dealers.vision.filt = dealers.vision.filt %>% mutate(id = paste0(cod_hda,"#",segmento))
# r = 1
# random_rate = c(0.6, 0.65, 0.70, 0.75, 0.80, 0.85, 0.9, 1)

  for(i in 1:length(assessorias)){
    for(j in 1:length(segments)){

  if(assessorias[i] == "CESEC" & segments[j] == "MOT"){share = share_cese_mot}
  if(assessorias[i] == "CESEC" & segments[j] == "CAR"){share = share_cese_car}
  if(assessorias[i] == "PASCHOALOTTO" & segments[j] == "MOT"){share = share_pasc_mot}
  if(assessorias[i] == "PASCHOALOTTO" & segments[j] == "CAR"){share = share_pasc_car}
  if(assessorias[i] == "FLEX" & segments[j] == "MOT"){share = share_flex_mot}
  if(assessorias[i] == "FLEX" & segments[j] == "CAR"){share = share_flex_car}
  if(assessorias[i] == "LOCALCRED" & segments[j] == "MOT"){share = share_loca_mot}
  if(assessorias[i] == "LOCALCRED" & segments[j] == "CAR"){share = share_loca_car}
  
  assessoria.db <- dealers.vision.filt %>% dplyr::filter(segmento == segments[j])
  
  set.seed(2445)
  assessoria.db <- assessoria.db %>% group_by(cut_median_atraso, cut_median_balance, cut_median_score) %>% sample_frac(1)
  set.seed(5398)
  assessoria.db <- assessoria.db[sample(nrow(assessoria.db)),]
  assessoria.db$contracts_cumsum <- cumsum(assessoria.db$contracts)
  assessoria.db <- assessoria.db[assessoria.db$contracts_cumsum <= share,]
  print(paste0("share of ", segments[j], " for ", assessorias[i], " is: ", round(sum(assessoria.db$contracts)/sum(summar$contracts),6)*100, "% (", sum(assessoria.db$contracts),") of total contracts"))
  
  assessoria.db <- assessoria.db %>% mutate(assessoria = assessorias[i])
  file.name <- paste0(assessorias[i],"_",segments[j])
  list_files[[file.name]] <- assessoria.db
  dealers.vision.filt <<- dealers.vision.filt %>% filter(!id %in% c(assessoria.db$id))
  
  # r <<- r+1
    }}

dealers.final = bind_rows(list_files)
dealers.final = bind_rows(dealers.final, dealers.vision.filt)
dealers.final$assessoria[dealers.final$cod_hda %in% c("1016121")] <- "PASCHOALOTTO"
dealers.final$assessoria[dealers.final$cod_hda %in% c("1019252")] <- "FLEX"
dealers.final$assessoria[dealers.final$cod_hda %in% c("1594313")] <- "LOCALCRED"
# dealers.final$assessoria[dealers.final$cod_hda %in% c("1592855", "1594600", "1595307", "1605789", "1614579", "1619959", "1627504", "1639153", "1652880", "1681603", "1596056")] <- "PASCHOALOTTO"
# dealers.final$assessoria[dealers.final$cod_hda %in% c("1692136", "1719942")] <- "LOCALCRED"

contracts.final = left_join(march.db.out, dealers.final, by = c("cod_hda", "nome_hda", "nome_est_loja", "segmento"))
contracts.final = contracts.final %>% filter(qtd_dias_em_atraso >= 1 & qtd_dias_em_atraso <= 360)

table(is.na(contracts.final$assessoria.y))

summar.shuffle =  contracts.final %>% group_by(assessoria.y, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                      contracts = n(),
                                                                      states = n_distinct(nome_est_loja)) %>%
                                                                      mutate(perc_dealers = dealers/n_distinct(march.db.out$nome_hda),
                                                                             perc_contracts = contracts/nrow(march.db.out))

names(summar.shuffle)[1] = "assessoria"
summar$assessoria = gsub("_","", summar$assessoria)
summar$assessoria = gsub(" CONTACT", "", summar$assessoria)

summar.final = left_join(summar, summar.shuffle, by = c("assessoria", "segmento"), suffix = c("_before", "_after"))
summar.final = summar.final %>% mutate(dif_perc_contracts = abs(perc_contracts_after - perc_contracts_before),
                                       dif_contracts = contracts_after - contracts_before)

fwrite(dealers.vision.filt, file = "dealers_vision_remain.csv", dec = ",", sep = ";")
fwrite(summar.final, file = "summar_final.csv", dec = ",", sep = ";")

##############################

plot.table.est.actual = contracts.final %>% group_by(assessoria.x, nome_est_loja, segmento) %>% dplyr::summarise(n = n())
plot.table.days.actual = contracts.final %>% group_by(assessoria.x, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance.actual = contracts.final %>% group_by(assessoria.x, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster.actual = contracts.final %>% group_by(assessoria.x, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda.actual = contracts.final %>% group_by(assessoria.x, nome_hda, segmento) %>% dplyr::summarise(n = n())
plot.table.index.actual = contracts.final %>% group_by(assessoria.x, cut_index, segmento) %>% dplyr::summarise(n = n())

plot.table.est.shuffle = contracts.final %>% group_by(assessoria.y, nome_est_loja, segmento) %>% dplyr::summarise(n = n())
plot.table.days.shuffle = contracts.final %>% group_by(assessoria.y, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance.shuffle = contracts.final %>% group_by(assessoria.y, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster.shuffle = contracts.final %>% group_by(assessoria.y, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda.shuffle = contracts.final %>% group_by(assessoria.y, nome_hda, segmento) %>% dplyr::summarise(n = n())
plot.table.index.shuffle = contracts.final %>% group_by(assessoria.y, cut_index, segmento) %>% dplyr::summarise(n = n())

# add balance (owed value), dealer (with and without), score, 

est.plot.actual = ggplot(data=plot.table.est.actual, aes(x=nome_est_loja, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot.actual = ggplot(data=plot.table.days.actual, aes(x=cut_atraso, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot.actual = ggplot(data=plot.table.balance.actual, aes(x=cut_balance, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts actual by balance",
       fill = "", x = "balance", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot.actual = ggplot(data=plot.table.hda.actual, aes(x=nome_hda, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by dealer",
       fill = "", x = "dealer", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot.actual = ggplot(data=plot.table.cluster.actual, aes(x=cluster, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

index.plot.actual = ggplot(data=plot.table.index.actual, aes(x=cut_index, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by index",
       fill = "", x = "delinquency index", y = "contracts (%)") + 
#       caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay <- rbind(c(1,2,3),
              c(4,5,6))

# both_plot_actual <- grid.arrange(est.plot, delay.plot, balance.plot, hda.plot, cluster.plot, index.plot, layout_matrix = hlay)

##################
#CONTRACTS VISION#
##################

est.plot.shuffle = ggplot(data=plot.table.est.shuffle, aes(x=nome_est_loja, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot.shuffle = ggplot(data=plot.table.days.shuffle, aes(x=cut_atraso, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot.shuffle = ggplot(data=plot.table.balance.shuffle, aes(x=cut_balance, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts shuffle by balance",
       fill = "", x = "balance", y = "contracts (%)",
  caption = "Source: IGB | march, 2019 | active contracts [11 - 30]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot.shuffle = ggplot(data=plot.table.hda.shuffle, aes(x=nome_hda, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by dealer",
       fill = "", x = "dealer", y = "contracts (%)",
       caption = "source: IGB | march, 2019 | active contracts [11 - 30]") +
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot.shuffle = ggplot(data=plot.table.cluster.shuffle, aes(x=cluster, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)") + 
#      caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

index.plot.shuffle = ggplot(data=plot.table.index.shuffle, aes(x=cut_index, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by index",
       fill = "", x = "delinquency index", y = "contracts (%)", 
       caption = "source: IGB | march, 2019 | active contracts [11 - 30]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay1 <- rbind(c(1,2,3),
              c(4,5,6))

hlay2 <- rbind(c(1,2),
               c(3,4))

both_plot_shuffle <- grid.arrange(est.plot.shuffle, delay.plot.shuffle, balance.plot.shuffle, cluster.plot.shuffle, index.plot.shuffle, layout_matrix = hlay1)
both_plot_1 = grid.arrange(est.plot.actual, delay.plot.actual, balance.plot.actual, est.plot.shuffle, delay.plot.shuffle, balance.plot.shuffle, layout_matrix = hlay1)
both_plot_2 = grid.arrange(cluster.plot.actual, index.plot.actual, cluster.plot.shuffle, index.plot.shuffle, layout_matrix = hlay2)

setwd("D:/Users/sb044936/Desktop/Reshuffle/1-360")
load("contracts_final.RData")

ggsave(paste0("plots_actual_vs_shuffle_1_11_30","_", Sys.Date(),".tiff"), both_plot_1, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_vs_shuffle_2_1_360","_", Sys.Date(),".tiff"), both_plot_2, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")

index_shuffle = contracts.final %>% select(assessoria.y, cod_hda, nome_hda, segmento, delinquents_hda, outstanding_hda) %>% distinct()
index_shuffle = index_shuffle %>% group_by(assessoria.y, segmento) %>% summarise(delinquents_assess = sum(delinquents_hda),
                                                                                 outstanding_assess = sum(outstanding_hda)) %>% mutate(index_assess = delinquents_assess/outstanding_assess)
analysis_shuffle = contracts.final %>% group_by(assessoria.y, segmento) %>% dplyr::summarise(contracts = n(),
                                                                                                    balance = sum(vlr_vencido, na.rm = TRUE),
                                                                                                    dealers = n_distinct(nome_hda),
                                                                                                    median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                                                    median_delay = median(qtd_dias_em_atraso, na.rm = TRUE),
                                                                                                    median_score = median(score, na.rm = TRUE))
analysis_shuffle = left_join(analysis_shuffle, index_shuffle)

index_actual = contracts.final %>% select(assessoria.x, cod_hda, nome_hda, segmento, delinquents_hda, outstanding_hda) %>% distinct()
index_actual = index_actual %>% group_by(assessoria.x, segmento) %>% summarise(delinquents_assess = sum(delinquents_hda),
                                                                                 outstanding_assess = sum(outstanding_hda)) %>% mutate(index_assess = delinquents_assess/outstanding_assess)
analysis_actual = contracts.final %>% group_by(assessoria.x, segmento) %>% dplyr::summarise(contracts = n(),
                                                                                             balance = sum(vlr_vencido, na.rm = TRUE),
                                                                                             dealers = n_distinct(nome_hda),
                                                                                             median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                                             median_delay = median(qtd_dias_em_atraso, na.rm = TRUE),
                                                                                             median_score = median(score, na.rm = TRUE))
analysis_actual = left_join(analysis_actual, index_actual)

dealers_shuffle = contracts.final %>% select(cod_hda, nome_hda, nome_est_loja, segmento, assessoria.x, assessoria.y) %>% distinct()
dealers_shuffle = dealers_shuffle %>% mutate(ass_actual_equal_shuffle = ifelse(assessoria.x == assessoria.y, TRUE, FALSE))

contracts.final = left_join(contracts.final, all)

fwrite(analysis_actual, file = "analysis_actual.csv", sep = ";", dec = ",")
fwrite(analysis_shuffle, file = "analysis_shuffle.csv", sep = ";", dec= ",")
save(contracts.final, file = "contracts_final.RData")

load("contracts_final.RData")
analysis_shuffle = fread("analysis_shuffle.csv", sep = ",")
analysis_actual = fread("analysis_actual.csv", sep = ",")

save(est.plot.shuffle, file = "est_plot_shuffle.RData") 
save(delay.plot.shuffle, file = "delay_plot_shuffle.RData")
save(balance.plot.shuffle, file = "balance_plot_shuffle.RData") 
save(cluster.plot.shuffle, file = "cluster_plot_shuffle.RData") 
save(index.plot.shuffle, file = "index_plot_shuffle.RData")

save(est.plot.actual, file = "est_plot_actual.RData") 
save(delay.plot.actual, file = "delay_plot_actual.RData")
save(balance.plot.actual, file = "balance_plo_actual.RData") 
save(cluster.plot.actual, file = "cluster_plot_actual.RData") 
save(index.plot.actual, file = "index_plot_actual.RData")

teste = contracts.final %>% filter(is.na(cut_balance))


# ggplot(contracts.final, aes(x=vlr_vencido, fill=segmento)) + geom_density(alpha=0.25) + xlim(0,5000)
# 
# medians = contracts.final %>% group_by(segmento) %>% summarise(median = median(vlr_vencido, na.rm = TRUE),
#                                                                mean = mean(vlr_vencido, na.rm = TRUE))

load("contracts_final.RData")

fwrite(x, file = "assess.csv")
