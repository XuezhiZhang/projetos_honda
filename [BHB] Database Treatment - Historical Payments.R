require(dplyr)
require(ggplot2)
require(plyr)
require(data.table)

##############################################
# reading all files with historical payments #
##############################################

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/Old models")

x = sapply(list.files(pattern = "*.txt"), fread, colClasses = c("CONT" = "character",
                                                                "N. N" = "character"), na.strings = c("", " ", "#N/D"), dec = ",")

bhb.hist.paym = bind_rows(x)

load("bhb_hist_paym_all.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2013")
load("bhb_hist_paym_2013.RData")
save(bhb.hist.paym.2013, file = "bhb_hist_paym_2013.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2014")
load("bhb_hist_paym_2014.RData")
save(bhb.hist.paym.2014, file = "bhb_hist_paym_2014.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2015")
load("bhb_hist_paym_2015.RData")
save(bhb.hist.paym.2015, file = "bhb_hist_paym_2015.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2016")
load("bhb_hist_paym_2016.RData")
save(bhb.hist.paym.2016, file = "bhb_hist_paym_2016.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2017")
load("bhb_hist_paym_2017.RData")
save(bhb.hist.paym.2017, file = "bhb_hist_paym_2017.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2018")
load("bhb_hist_paym_2018.RData")
save(bhb.hist.paym.2018, file = "bhb_hist_paym_2018.RData")
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/2019")
load("bhb_hist_paym_2019.RData")
save(bhb.hist.paym.2019, file = "bhb_hist_paym_2019.RData")

bhb.hist.paym = bind_rows(x)
bhb.hist.paym = bind_rows(bhb.hist.paym.2013, bhb.hist.paym.2014)
bhb.hist.paym = bind_rows(bhb.hist.paym, bhb.hist.paym.2015)
bhb.hist.paym = bind_rows(bhb.hist.paym, bhb.hist.paym.2016)
bhb.hist.paym = bind_rows(bhb.hist.paym, bhb.hist.paym.2017)
bhb.hist.paym = bind_rows(bhb.hist.paym, bhb.hist.paym.2018)
bhb.hist.paym = bind_rows(bhb.hist.paym, bhb.hist.paym.2019)

rm(bhb.hist.paym.2013, bhb.hist.paym.2014, bhb.hist.paym.2015,
   bhb.hist.paym.2016, bhb.hist.paym.2017, bhb.hist.paym.2018, 
   bhb.hist.paym.2019)

#####################
# cleaning database #
#####################

bhb.hist.paym = bhb.hist.paym %>% select(-c("debt", "debt-desc", "FAIA LOSS", "FAIXA 1", "FAIXA LOSS", "FAIXA", "STATUS",
                                            "V10", "DEBT-DEC", "DEBT-DESC", "Faixa", "DEB-DESC", "ATRASO LOSS", "V27", 
                                            "V28", "V29", "UF", "DEBTR", "JUROS + MULTA"))

bhb.hist.paym = bhb.hist.paym %>% select(-c("V28", "V29", "UF", "DEBTR", "JUROS + MULTA"))
bhb.hist.paym = bhb.hist.paym %>% select(-c("ATRASO LOSS", "V27", "JUROS+MULTA", "JUR + MULTA"))#, "FAIXA", "JUROS + MULTA", "UF",  "DEBT-DEC", "DEBT-DESC"))
bhb.hist.paym = bhb.hist.paym %>% distinct()

bhb.hist.paym.2019 = bhb.hist.paym; rm(bhb.hist.paym)
bhb.hist.paym.2018 = bhb.hist.paym; rm(bhb.hist.paym)
bhb.hist.paym.2017 = bhb.hist.paym; rm(bhb.hist.paym)
bhb.hist.paym.2016 = bhb.hist.paym; rm(bhb.hist.paym)
bhb.hist.paym.2014 = bhb.hist.paym; rm(bhb.hist.paym)
bhb.hist.paym.2013 = bhb.hist.paym; rm(bhb.hist.paym)

bhb.hist.paym = bind_rows(bhb.hist.paym.2018, bhb.hist.paym.2019)

bhb.hist.paym = bhb.hist.paym %>% dplyr::rename("escritorio" = "ESCRITORIO",
                                         "cod_contrato" = "CONT",
                                         "n_n" = "N. N",
                                         "data_pagamento" = "DT_PGTO",
                                         "data_vnc" = "VTO PARC",
                                         "dias_atrasados" = "ATRASO",
                                         "data_emissao" = "EMISSÃO",
                                         "parcela" = "PARC",
                                         "data_agendamento" = "DT AGEND.",
                                         "situacao" = "SITUAÇÃO",
                                         "campanha" = "CAMPANHA",
                                         "segmento" = "MODAL.",
                                         "dif_valor" = "DIF VALOR",
                                         "vlr_parcela" = "VR PARCELA",
                                         "vlr_juros" = "JUROS",
                                         "vlr_multa" = "MULTA",
                                         "vlr_debt" = "DEBT",
                                         "vlr_honor" = "HONOR.",
                                         "vlr_desc" = "DESC.",
                                         "vlr_custas" = "CUSTAS",
                                         "vlr_total" = "VALOR TOTAL",
                                         "vlr_boleto" = "VALOR DO BOLETO")

bhb.hist.paym = bhb.hist.paym %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                                   list(~as.Date(as.character(.), 
                                                                format = "%d/%m/%Y")))

save(bhb.hist.paym, file = "bhb_hist_paym_all.RData")
load("bhb_hist_paym_all.RData")

# train & validation 1 month perfomance window and 12 months of observation window
date.12 = "2018-08" # last month (first month + 11)
date.7 = "2018-05"  # first month + 8
date.4 = "2018-01"  # first month + 4
date.1 = "2017-09"  # first month

# test 1 month perfomance window and 6 months of observation window
date.12 = "2019-03" # last month (first month + 5)
date.7 = "2018-02"  # first month + 4
date.4 = "2018-12"  # first month + 2
date.1 = "2018-10"  # first month

# old models
date.12 = "2019-03" # also define 1 year of observation window
date.1 = "2018-04" 

# selecting just moments until data cut for performance window
bhb.hist.paym = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") <= date.12)

################################
# creation of historical counts#
################################

# delays & payments count between observation window
# delays
atraso.count.1 = bhb.hist.paym %>% dplyr::filter(dias_atrasados > 0 &
                                                   format(as.Date(data_pagamento), "%Y-%m") == date.1) %>%
                                  dplyr::group_by(cod_contrato) %>% 
                                  dplyr::summarise(qtd_pg_atr_1_mes = n(),
                                                   vlr_pg_atr_1_mes = sum(vlr_boleto, na.rm = TRUE))

atraso.count.4 = bhb.hist.paym %>% dplyr::filter(dias_atrasados > 0 &
                                                   format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                                   format(as.Date(data_pagamento), "%Y-%m") <= date.4) %>%
                                  dplyr::group_by(cod_contrato) %>% 
                                  dplyr::summarise(qtd_pg_atr_4_meses = n(),
                                                   vlr_pg_atr_4_meses = sum(vlr_boleto, na.rm = TRUE))

atraso.count.7 = bhb.hist.paym %>% dplyr::filter(dias_atrasados > 0 &
                                                    format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                                    format(as.Date(data_pagamento), "%Y-%m") <= date.7) %>%
                                  dplyr::group_by(cod_contrato) %>% 
                                  dplyr::summarise(qtd_pg_atr_7_meses = n(),
                                                   vlr_pg_atr_7_meses = sum(vlr_boleto, na.rm = TRUE))

atraso.count.12 = bhb.hist.paym %>% dplyr::filter(dias_atrasados > 0 &
                                                    format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                                    format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>%
                                  dplyr::group_by(cod_contrato) %>% 
                                  dplyr::summarise(qtd_pg_atr_12_meses = n(),
                                                   vlr_pg_atr_12_meses = sum(vlr_boleto, na.rm = TRUE))

#payments
pagam.count.1 <- bhb.hist.paym %>% 
                      dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") == date.1) %>%
                      dplyr::group_by(cod_contrato) %>% 
                      dplyr::summarise(qtd_pg_1_mes = n(),
                                       valor_pg_1_mes = sum(vlr_boleto, na.rm = TRUE))

pagam.count.4 <- bhb.hist.paym %>% 
                       dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                     format(as.Date(data_pagamento), "%Y-%m") <= date.4) %>%
                       dplyr::group_by(cod_contrato) %>% 
                       dplyr::summarise(qtd_pg_4_meses = n(),
                                        valor_pg_4_meses = sum(vlr_boleto, na.rm = TRUE))

pagam.count.7 <- bhb.hist.paym %>% 
                      dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                      format(as.Date(data_pagamento), "%Y-%m") <= date.7) %>%
                      dplyr::group_by(cod_contrato) %>% 
                      dplyr::summarise(qtd_pg_7_meses = n(),
                                       valor_pg_7_meses = sum(vlr_boleto, na.rm = TRUE))

pagam.count.12 <- bhb.hist.paym %>% 
                      dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 &
                                    format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>%
                      dplyr::group_by(cod_contrato) %>% 
                      dplyr::summarise(qtd_pg_12_meses = n(),
                                       valor_pg_12_meses = sum(vlr_boleto, na.rm = TRUE))

# joining counts in one dataframe
count.all = left_join(atraso.count.1, atraso.count.4, by = "cod_contrato"); rm(atraso.count.1, atraso.count.4)
count.all = left_join(count.all, atraso.count.7, by = "cod_contrato"); rm(atraso.count.7)
count.all = left_join(count.all, atraso.count.12, by = "cod_contrato"); rm(atraso.count.12)
count.all = left_join(count.all, pagam.count.1, by = "cod_contrato"); rm(pagam.count.1)
count.all = left_join(count.all, pagam.count.4, by = "cod_contrato"); rm(pagam.count.4)
count.all = left_join(count.all, pagam.count.7, by = "cod_contrato"); rm(pagam.count.7)
count.all = left_join(count.all, pagam.count.12, by = "cod_contrato"); rm(pagam.count.12)

# replacing NA for 0
count.all = count.all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

# first delay
primeiro.atraso = bhb.hist.paym %>% dplyr::filter(dias_atrasados > 0 &
                                                  format(as.Date(data_pagamento, "%Y-%m")) <= date.12) %>% 
                                    dplyr::group_by(cod_contrato) %>% 
                                    dplyr::summarise(data_primeiro_atraso = min(data_pagamento, na.rm = TRUE))

count.all = left_join(count.all, primeiro.atraso, by = "cod_contrato")

# count how many consecutive sequences of installments were paid with delay
# y = bhb.hist.paym %>% group_by(cod_contrato) %>% 
#                       arrange(parcela) %>%
#                       mutate(y = paste(rle(diff(sort(parcela))), collapse = ", "))
# 
# y = y %>% select(cod_contrato, y)
# 
# teste = bhb.hist.paym %>% filter(cod_contrato == "1000622")
# 
# paste(rle(diff(sort(teste$parcela))), collapse = ", ")
# x = rle(diff(sort(teste$parcela)))
# 
# # size of repetitions
# length(x$lengths[x$values==1])

# biggest time without payments
max.dif.entre.pgto = bhb.hist.paym %>% dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                     format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>%
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::arrange(data_pagamento) %>%
                                       dplyr::mutate(max_dif_entre_pgto = max(diff(data_pagamento), na.rm = TRUE)) %>% 
                                       dplyr::select(cod_contrato, max_dif_entre_pgto) %>% distinct()

# substituting "-inf" to 0
max.dif.entre.pgto = max.dif.entre.pgto %>% mutate_at(vars(max_dif_entre_pgto), 
                                                      list(~replace(., is.infinite(.), 0)))

# adding max tempo sem pagamento to count.all database
count.all = left_join(count.all, max.dif.entre.pgto, by = "cod_contrato")

# max delay
max.atraso = bhb.hist.paym %>% dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                             format(as.Date(data_pagamento), "%Y-%m") <= date.12 &
                                               dias_atrasados > 0) %>%
                                       dplyr::group_by(cod_contrato) %>%
                                       dplyr::arrange(dias_atrasados) %>%
                                       dplyr::summarise(max_atraso = max(dias_atrasados, na.rm = TRUE)) %>% distinct()

# min delay
min.atraso = bhb.hist.paym %>% dplyr::filter(format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                             format(as.Date(data_pagamento), "%Y-%m") <= date.12 &
                                               dias_atrasados > 0) %>%
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::arrange(dias_atrasados) %>%
                                       dplyr::summarise(min_atraso = min(dias_atrasados, na.rm = TRUE)) %>% distinct()

# first installment delayed?
primeira.parcela = bhb.hist.paym %>% filter(parcela == 1 & dias_atrasados > 0 &
                                            format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                     select(cod_contrato, dias_atrasados) %>% 
                                     dplyr::rename("dias_atr_primeira_parc" = "dias_atrasados")
primeira.parcela$atr_primeira_parc = "sim"

count.all = left_join(count.all, max.atraso)
count.all = left_join(count.all, min.atraso)
count.all = left_join(count.all, primeira.parcela)
count.all = count.all %>% mutate_at(vars(atr_primeira_parc), 
                                    list(~ifelse(is.na(.), "não", .)))


# delays between timeframes
atraso.1.10.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 10 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                      dplyr::group_by(cod_contrato) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_10 = n(),
                                                       vlr_pg_atr_1_10 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.15.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 15 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                      dplyr::group_by(cod_contrato) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_15 = n(),
                                                       vlr_pg_atr_1_15 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.16.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 16 & dias_atrasados <= 30 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                      dplyr::group_by(cod_contrato) %>% 
                                      dplyr::summarise(qtd_pg_atr_16_30 = n(),
                                                       vlr_pg_atr_16_30 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.11.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 11 & dias_atrasados <= 30 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::summarise(qtd_pg_atr_11_30 = n(),
                                                         vlr_pg_atr_11_30 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.11.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 11 & dias_atrasados <= 60 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::summarise(qtd_pg_atr_11_60 = n(),
                                                         vlr_pg_atr_11_60 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 30 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                      dplyr::group_by(cod_contrato) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_30 = n(),
                                                       vlr_pg_atr_1_30 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 60 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                      dplyr::group_by(cod_contrato) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_60 = n(),
                                                       vlr_pg_atr_1_60 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.31.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 31 & dias_atrasados <= 60 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::summarise(qtd_pg_atr_31_60 = n(),
                                                         vlr_pg_atr_31_60 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.61.360.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 61 & dias_atrasados <= 360 &
                                                        format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                        format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                        dplyr::group_by(cod_contrato) %>% 
                                        dplyr::summarise(qtd_pg_atr_61_360 = n(),
                                                         vlr_pg_atr_61_360 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.360.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 360 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                       dplyr::group_by(cod_contrato) %>% 
                                       dplyr::summarise(qtd_pg_atr_1_360 = n(),
                                                        vlr_pg_atr_1_360 = sum(vlr_boleto, na.rm = TRUE)) 

atraso.361.mais.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 361 &
                                                          format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                          format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
                                          dplyr::group_by(cod_contrato) %>% 
                                          dplyr::summarise(qtd_pg_atr_360_mais = n(),
                                                             vlr_pg_atr_360_mais = sum(vlr_boleto, na.rm = TRUE))

##################
# for old models #
##################


atraso.1.10.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 10 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_1_10_em_1_ano = n(),
                   vlr_pg_atr_1_10_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.15.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 15 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_1_15_em_1_ano = n(),
                   vlr_pg_atr_1_15_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.16.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 16 & dias_atrasados <= 30 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_16_30_em_1_ano = n(),
                   vlr_pg_atr_16_30_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.11.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 11 & dias_atrasados <= 30 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_11_30_em_1_ano = n(),
                   vlr_pg_atr_11_30_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.11.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 11 & dias_atrasados <= 60 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_11_60_em_1_ano = n(),
                   vlr_pg_atr_11_60_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.30.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 30 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_1_30_em_1_ano = n(),
                   vlr_pg_atr_1_30_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 60 &
                                                      format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                      format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_1_60_em_1_ano = n(),
                   vlr_pg_atr_1_60_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.31.60.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 31 & dias_atrasados <= 60 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_31_60_em_1_ano = n(),
                   vlr_pg_atr_31_60_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.61.360.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 61 & dias_atrasados <= 360 &
                                                        format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                        format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_61_360_em_1_ano = n(),
                   vlr_pg_atr_61_360_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.360.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 360 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_1_360_em_1_ano = n(),
                   vlr_pg_atr_1_360_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.361.mais.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 361 &
                                                          format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                          format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_360_mais_em_1_ano = n(),
                   vlr_pg_atr_360_mais_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

atraso.1.ano.count = bhb.hist.paym %>% dplyr::filter(dias_atrasados >= 1 & dias_atrasados <= 360 &
                                                       format(as.Date(data_pagamento), "%Y-%m") >= date.1 & 
                                                       format(as.Date(data_pagamento), "%Y-%m") <= date.12) %>% 
  dplyr::group_by(cod_contrato) %>% 
  dplyr::summarise(qtd_pg_atr_em_1_ano = n(),
                   vlr_pg_atr_em_1_ano = sum(vlr_boleto, na.rm = TRUE)) 

#atrasos.count.by.sit = dcast(atraso.sit.count, CONT ~ `SITUAÇÃO`, value.var = "n")

atrasos.count.by.ctr <- atraso.1.10.count
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.30.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.15.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.16.30.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.11.30.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.11.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.31.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.61.360.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.360.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.361.mais.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.ano.count)

count.all = left_join(count.all, atrasos.count.by.ctr, by = "cod_contrato")

#removing all unnecessary databases

rm(atraso.1.10.count, atraso.11.30.count, atraso.1.15.count, atraso.16.30.count,
   atraso.11.60.count, atraso.1.30.count, atraso.1.60.count, atraso.31.60.count,
   atraso.61.360.count, atraso.1.360.count, atraso.361.mais.count, max.atraso,
   max.dif.entre.pgto, min.atraso, primeira.parcela, primeiro.atraso)
gc(); gc()

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(count.all, file = "historic_delay_info_till_2019-03.RData")
# load("delay_count_by_contr_until_2019-03.RData")
# save(atrasos.count.by.ctr, file = "delay_count_by_contr.RData")

################
#VISUALIZATIONS#
################

#historical pattern of payment
# ggplot(x, aes(x=ATRASO,fill=MODAL.)) + geom_density(alpha=.3) + 
#                                        geom_vline(xintercept=c(11, 30, 60),
#                                                   color="blue", linetype="dashed", size=0.5) +
#                                        scale_x_continuous(name = "Days overdue until payment", breaks = seq(0, 100, 5), lim = c(0, 100), minor_breaks = seq(0, 100, 5)) +
#                                        scale_y_continuous(name = "Payments (Installments)") +
#                                        ggtitle("BHB - Delinquency behaviour until payment [sep'17 till oct'18]")

###############################
#PAYMENT EVOLUTION BY CONTRACT#
###############################

# teste = bhb.hist.paym %>% group_by(CONT, COMP) %>% summarise(n = n())
# teste = dcast(teste, CONT + CAMPANHA ~ COMP, value.var = "n")

# bhb.hist.paym$DT_PGTO = as.Date(bhb.hist.paym$DT_PGTO, format = "%d/%m/%Y")
# bhb.hist.paym$COMP = substr(bhb.hist.paym$DT_PGTO, 0, 7)
# count.pay = bhb.hist.paym %>% dplyr::group_by(CONT, `MODAL.`) %>% dplyr::summarise(payments = n()) 
# count.pay.camp =  bhb.hist.paym %>% dplyr::filter(!CAMPANHA %in% c("2 a 12 Sem Notif. (13) UFs - Baixa CC -", "2 a 6 C. Notif. 13 UFs - Baixa CC -", "7 a 12 C. Notif. 13 UFs - Baixa CC -", "Campanha (6) Estados -", "CAMPANHA 1 a 6 P Estados PA RN -", "CAMPANHA 2 a 6 P Estados Outros -", "CAMPANHA 7 a 12 P Estados PA RN -", "CAMPANHA JURIDICO S/ DESC 2W -", "CAMPANHA JURIDICO S/ DESC 2W - A", "CAMPANHA JURIDICO S/ DESC 2W - Q", "CAMPANHA JURIDICO S/ DESC 4W -", "CAMPANHA JURIDICO S/ DESC 4W - A", "CAMPANHA JURIDICO S/ DESC 4W - Q", "CAMPANHA JURIDICO S/ DESCONTO -", "Campanha não cadastrada", "Campanha não cadastrada Q", "Campanha PA e RN -", "CAMPANHA PAGOU FACIL -", "CAMPANHA PAGOU FACIL - A", "CAMPANHA PAGOU FACIL - Q", "CAMPANHA PARA SALDO REMANESCENTE (RETOMADO) -", "CAMPANHA PARA SALDO REMANESCENTE (RETOMADO) - Q", "CAMPANHA SEM DESCONTO 2W -", "CAMPANHA SEM DESCONTO 2W - A", "CAMPANHA SEM DESCONTO 2W - P", "CAMPANHA SEM DESCONTO 2W - Q", "CAMPANHA SEM DESCONTO 4W -", "CAMPANHA SEM DESCONTO 4W - A", "CAMPANHA SEM DESCONTO 4W - P", "CAMPANHA SEM DESCONTO 4W - Q", "Honda 2 Rodas Jurídico (ret) -", "Honda 2 Rodas Jurídico (ret) - A", "Honda 2 Rodas Jurídico (ret) - Q", "Honda 4 Rodas -")) %>% 
#   dplyr::group_by(CONT, `MODAL.`) %>% dplyr::summarise(payments.with.camp = n())
# 
# count = left_join(count.pay, count.pay.camp)
# count$payments.with.camp = ifelse(is.na(count$payments.with.camp),0,count$payments.with.camp)
# count$perc = (count$payments.with.camp/count$payments)
# 
# ggplot(count, aes(x=perc, fill=`MODAL.`)) + geom_density(alpha=.3) +
#   scale_x_continuous(name = "Percentage of payments involved in any campaign with discount") + ggtitle("Distribution of payments with campaign [Sep'17 - Oct'18]") +
#   scale_y_continuous(name = "Clients' percentage")
# 
# fwrite(count, file = "campaign_payment_pattern.csv")
# 
# rm(count, count.pay, count.pay.camp); gc();gc()
# 
# #################################
# 
# atrasos.count.by.ctr$`perc_pg_atr_11_60` = (atrasos.count.by.ctr$qtd_pg_atr_11_30_em_1_ano + atrasos.count.by.ctr$qtd_pg_atr_31_60_em_1_ano) / (atrasos.count.by.ctr$qtd_pg_atr_em_1_ano)
# atrasos.count.by.ctr$`perc_pg_atr_61_360` = atrasos.count.by.ctr$qtd_pg_atr_61_360_em_1_ano / atrasos.count.by.ctr$qtd_pg_atr_em_1_ano
# 
# atrasos.count.by.ctr = atrasos.count.by.ctr %>% dplyr::mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
# 
# join = left_join(bhb.hist.paym, atrasos.count.by.ctr, by = c("CONT", "MODAL."))
# join.select = join %>% select(CONT, ATRASO, MODAL., perc_pg_atr_11_60, perc_pg_atr_61_360) %>% distinct()
# anual_median = join %>% filter(ATRASO >= 11 & ATRASO <= 60) %>% group_by(MODAL.) %>% summarise(median_11_60 = median(perc_pg_atr_11_60, na.rm = TRUE))
# 
# medians = filter.11.60 %>% dplyr::group_by(MODAL.) %>% dplyr::summarise(median_perc_11_60 = median(perc_pg_atr_11_60, na.rm= TRUE))
# medians = filter.61.360 %>% dplyr::group_by(MODAL.) %>% dplyr::summarise(median_perc_61_360 = median(perc_pg_atr_61_360, na.rm= TRUE))
# 
# filter.11.60 = join.select %>% filter(ATRASO >= 11 & ATRASO <= 60)
# filter.61.360 = join.select %>% filter(ATRASO >= 61 & ATRASO <= 360)
# 
# ggplot(filter.61.360, aes(x=perc_pg_atr_61_360, fill = MODAL.))+
#   geom_density(alpha = 0.4)
# 
# ggplot(filter.11.60, aes(x=perc_pg_atr_11_60, fill = MODAL.))+
#   geom_density(alpha = 0.4)

# x$cut = cut(x$dias_atrasados, breaks = c(seq(0, 360, by = 30)))
# x = x %>% filter(dias_atrasados > 0 & dias_atrasados <= 360)
# y = x %>% group_by(cut, segmento) %>% summarise(n = n())
# 
# bhb.hist.paym = bhb.hist.paym %>% filter(dias_atrasados > 0 & dias_atrasados <= 360)
# bhb.hist.paym = bhb.hist.paym %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 360, by = 30))))
# y = bhb.hist.paym %>% group_by(cut, segmento) %>% summarise(n = n())
# y = spread(y, segmento, n)

###################
# target creation #
###################

target.201809  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2018-09") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201809 = target.201809  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
target.201809 = target.201809 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())

target.201809 = spread(target.201809, target, n)
target.201809 = target.201809 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201809 = target.201809 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201809 = target.201809 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201809, file = "target_201809.RData")

######

target.201904  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2019-04") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201904 = target.201904  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
target.201904 = target.201904 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())

target.201904 = spread(target.201904, target, n)
target.201904 = target.201904 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201904 = target.201904 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201904 = target.201904 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201904, file = "target_201904.RData")
