require(dplyr)
require(ggplot2)
require(plyr)

############################################
#READING ALL FILES WITH HISTORICAL PAYMENTS#
############################################

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB")
load("bhb_hist_paym_jan'18_dez'18.RData")

bhb.hist.paym <- list.files(pattern = "*.txt") %>%
  lapply(fread, stringsAsFactors=F,
         colClasses = c("CONT" = "character",
                        "N. N" = "character"), dec = ",",
         header = TRUE) %>%
  bind_rows

#save(bhb.hist.paym, file ="bhb_hist_paym_nov'17_dez'18.RData")

###############
#TRATANDO BASE#w
###############

bhb.hist.paym$SITUAÇÃO = revalue(bhb.hist.paym$SITUAÇÃO, c("AMIGAVEL" = "AMIGÁVEL", "JURIDICO" = "JURÍDICO"))

x = bhb.hist.paym %>% dplyr::filter(ATRASO >= 0) %>%
                      dplyr::group_by(CONT, ATRASO, MODAL.) %>% 
                      dplyr::summarise(qtd_parc_pg_1_ano = n(),
                                       vlr_parc_pgs_1_ano = sum(`VR PARCELA`, na.rm = TRUE),
                                       vlr_pg_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE))

atraso.count <- bhb.hist.paym %>% dplyr::filter(ATRASO > 0) %>% 
                                  dplyr::group_by(CONT, MODAL.) %>% 
                                  dplyr::summarise(qtd_pg_atr_em_1_ano = n(),
                                                   valor_pg_atr_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.1.10.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 1 & ATRASO <= 10) %>% 
                                      dplyr::group_by(CONT) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_10_em_1_ano = n(),
                                                       vlr_pg_atr_1_10_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.11.30.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 11 & ATRASO <= 30) %>% 
                                       dplyr::group_by(CONT) %>% 
                                       dplyr::summarise(qtd_pg_atr_11_30_em_1_ano = n(),
                                                         vlr_pg_atr_11_30_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.11.60.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 11 & ATRASO <= 60) %>% 
                                       dplyr::group_by(CONT) %>% 
                                       dplyr::summarise(qtd_pg_atr_11_60_em_1_ano = n(),
                                                         vlr_pg_atr_11_60_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.1.30.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 1 & ATRASO <= 30) %>% 
                                      dplyr::group_by(CONT) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_30_em_1_ano = n(),
                                                       vlr_pg_atr_1_30_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.1.60.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 1 & ATRASO <= 60) %>% 
                                      dplyr::group_by(CONT) %>% 
                                      dplyr::summarise(qtd_pg_atr_1_60_em_1_ano = n(),
                                                       vlr_pg_atr_1_60_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.31.60.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 31 & ATRASO <= 60) %>% 
                                       dplyr::group_by(CONT) %>% 
                                       dplyr::summarise(qtd_pg_atr_31_60_em_1_ano = n(),
                                                         vlr_pg_atr_31_60_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.61.360.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 61 & ATRASO <= 360) %>% 
                                        dplyr::group_by(CONT) %>% 
                                        dplyr::summarise(qtd_pg_atr_61_360_em_1_ano = n(),
                                                         vlr_pg_atr_61_360_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

atraso.1.360.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 1 & ATRASO <= 360) %>% 
                                       dplyr::group_by(CONT) %>% 
                                       dplyr::summarise(qtd_pg_atr_1_360_em_1_ano = n(),
                                                        vlr_pg_atr_1_360_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 


atraso.361.mais.count = bhb.hist.paym %>% dplyr::filter(ATRASO >= 361) %>% 
                                          dplyr::group_by(CONT) %>% 
                                          dplyr::summarise(qtd_pg_atr_360_mais_em_1_ano = n(),
                                                             vlr_pg_atr_360_mais_em_1_ano = sum(`VALOR DO BOLETO`, na.rm = TRUE)) 

#atrasos.count.by.sit = dcast(atraso.sit.count, CONT ~ `SITUAÇÃO`, value.var = "n")

atrasos.count.by.ctr <- full_join(atraso.count, atraso.1.10.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.30.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.11.30.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.11.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.31.60.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.61.360.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.1.360.count)
atrasos.count.by.ctr <- full_join(atrasos.count.by.ctr, atraso.361.mais.count)

#removing all unnecessary databases

rm(bhb.hist.paym, x, atraso.count, atraso.1.10.count, atraso.11.30.count, 
   atraso.11.60.count, atraso.1.30.count, atraso.1.60.count, atraso.31.60.count,
   atraso.61.360.count, atraso.1.360.count, atraso.361.mais.count)
gc(); gc()

save(atrasos.count.by.ctr, file = "delay_count_by_contr.RData")
load("delay_count_by_contr.RData")

################
#VISUALIZATIONS#
################

#historical pattern of payment
ggplot(x, aes(x=ATRASO,fill=MODAL.)) + geom_density(alpha=.3) + 
                                       geom_vline(xintercept=c(11, 30, 60),
                                                  color="blue", linetype="dashed", size=0.5) +
                                       scale_x_continuous(name = "Days overdue until payment", breaks = seq(0, 100, 5), lim = c(0, 100), minor_breaks = seq(0, 100, 5)) +
                                       scale_y_continuous(name = "Payments (Installments)") +
                                       ggtitle("BHB - Delinquency behaviour until payment [sep'17 till oct'18]")

rm(x); gc();gc()
###############################
#PAYMENT EVOLUTION BY CONTRACT#
###############################

# teste = bhb.hist.paym %>% group_by(CONT, COMP) %>% summarise(n = n())
# teste = dcast(teste, CONT + CAMPANHA ~ COMP, value.var = "n")

bhb.hist.paym$DT_PGTO = as.Date(bhb.hist.paym$DT_PGTO, format = "%d/%m/%Y")
bhb.hist.paym$COMP = substr(bhb.hist.paym$DT_PGTO, 0, 7)
count.pay = bhb.hist.paym %>% dplyr::group_by(CONT, `MODAL.`) %>% dplyr::summarise(payments = n()) 
count.pay.camp =  bhb.hist.paym %>% dplyr::filter(!CAMPANHA %in% c("2 a 12 Sem Notif. (13) UFs - Baixa CC -", "2 a 6 C. Notif. 13 UFs - Baixa CC -", "7 a 12 C. Notif. 13 UFs - Baixa CC -", "Campanha (6) Estados -", "CAMPANHA 1 a 6 P Estados PA RN -", "CAMPANHA 2 a 6 P Estados Outros -", "CAMPANHA 7 a 12 P Estados PA RN -", "CAMPANHA JURIDICO S/ DESC 2W -", "CAMPANHA JURIDICO S/ DESC 2W - A", "CAMPANHA JURIDICO S/ DESC 2W - Q", "CAMPANHA JURIDICO S/ DESC 4W -", "CAMPANHA JURIDICO S/ DESC 4W - A", "CAMPANHA JURIDICO S/ DESC 4W - Q", "CAMPANHA JURIDICO S/ DESCONTO -", "Campanha não cadastrada", "Campanha não cadastrada Q", "Campanha PA e RN -", "CAMPANHA PAGOU FACIL -", "CAMPANHA PAGOU FACIL - A", "CAMPANHA PAGOU FACIL - Q", "CAMPANHA PARA SALDO REMANESCENTE (RETOMADO) -", "CAMPANHA PARA SALDO REMANESCENTE (RETOMADO) - Q", "CAMPANHA SEM DESCONTO 2W -", "CAMPANHA SEM DESCONTO 2W - A", "CAMPANHA SEM DESCONTO 2W - P", "CAMPANHA SEM DESCONTO 2W - Q", "CAMPANHA SEM DESCONTO 4W -", "CAMPANHA SEM DESCONTO 4W - A", "CAMPANHA SEM DESCONTO 4W - P", "CAMPANHA SEM DESCONTO 4W - Q", "Honda 2 Rodas Jurídico (ret) -", "Honda 2 Rodas Jurídico (ret) - A", "Honda 2 Rodas Jurídico (ret) - Q", "Honda 4 Rodas -")) %>% 
  dplyr::group_by(CONT, `MODAL.`) %>% dplyr::summarise(payments.with.camp = n())

count = left_join(count.pay, count.pay.camp)
count$payments.with.camp = ifelse(is.na(count$payments.with.camp),0,count$payments.with.camp)
count$perc = (count$payments.with.camp/count$payments)

ggplot(count, aes(x=perc, fill=`MODAL.`)) + geom_density(alpha=.3) +
  scale_x_continuous(name = "Percentage of payments involved in any campaign with discount") + ggtitle("Distribution of payments with campaign [Sep'17 - Oct'18]") +
  scale_y_continuous(name = "Clients' percentage")

fwrite(count, file = "campaign_payment_pattern.csv")

rm(count, count.pay, count.pay.camp); gc();gc()

#################################

atrasos.count.by.ctr$`perc_pg_atr_11_60` = (atrasos.count.by.ctr$qtd_pg_atr_11_30_em_1_ano + atrasos.count.by.ctr$qtd_pg_atr_31_60_em_1_ano) / (atrasos.count.by.ctr$qtd_pg_atr_em_1_ano)
atrasos.count.by.ctr$`perc_pg_atr_61_360` = atrasos.count.by.ctr$qtd_pg_atr_61_360_em_1_ano / atrasos.count.by.ctr$qtd_pg_atr_em_1_ano

atrasos.count.by.ctr = atrasos.count.by.ctr %>% dplyr::mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

join = left_join(bhb.hist.paym, atrasos.count.by.ctr, by = c("CONT", "MODAL."))
join.select = join %>% select(CONT, ATRASO, MODAL., perc_pg_atr_11_60, perc_pg_atr_61_360) %>% distinct()
anual_median = join %>% filter(ATRASO >= 11 & ATRASO <= 60) %>% group_by(MODAL.) %>% summarise(median_11_60 = median(perc_pg_atr_11_60, na.rm = TRUE))

medians = filter.11.60 %>% dplyr::group_by(MODAL.) %>% dplyr::summarise(median_perc_11_60 = median(perc_pg_atr_11_60, na.rm= TRUE))
medians = filter.61.360 %>% dplyr::group_by(MODAL.) %>% dplyr::summarise(median_perc_61_360 = median(perc_pg_atr_61_360, na.rm= TRUE))

filter.11.60 = join.select %>% filter(ATRASO >= 11 & ATRASO <= 60)
filter.61.360 = join.select %>% filter(ATRASO >= 61 & ATRASO <= 360)

ggplot(filter.61.360, aes(x=perc_pg_atr_61_360, fill = MODAL.))+
  geom_density(alpha = 0.4)

ggplot(filter.11.60, aes(x=perc_pg_atr_11_60, fill = MODAL.))+
  geom_density(alpha = 0.4)
