require(dplyr)
require(data.table)
require(stringr)

setwd("D:/Users/sb044936/Desktop/Carnival Campaign")
campaign = fread("payments.txt", header = TRUE, sep = "\t", dec = ",", colClasses = c("CONT" = "character"), na.strings = c("#N/D"))
previous_db = fread("previous_database.txt", header = TRUE, sep = "\t", dec = ",", colClasses = c("cod_contrato" = "character"), na.strings = c("#N/D", ""))

previous_db = previous_db %>% select(cod_contrato, Z, Dias, score, cluster, archive) %>% rename("situacao_bem" = "Z", 
                                                                                                "qtd_dias_em_atraso" = "Dias")


lr = campaign %>% filter(grepl("LR",CAMPANHA)) %>% mutate(risk = "LR")
hr = campaign %>% filter(grepl("HR",CAMPANHA)) %>% mutate(risk = "HR")

previous_db = previous_db %>% mutate(type = str_sub(, -1))

all_risks = bind_rows(lr, hr)

all_risks = all_risks %>% mutate(type = str_sub(CAMPANHA, -1)) %>% dplyr::rename("escritorio" = "ESCRITORIO",
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
                                                                                 "vlr_juros_multa" = "JUROS + MULTA",
                                                                                 "vlr_honor" = "HONOR.",
                                                                                 "vlr_desc" = "DESC.",
                                                                                 "vlr_custas" = "CUSTAS",
                                                                                 "vlr_total" = "VALOR TOTAL",
                                                                                 "vlr_boleto" = "VALOR DO BOLETO")

join = left_join(previous_db, all_risks, by = "cod_contrato")
join = join %>% mutate(atraso_cut = )

analysis = all_risks %>% group_by(type, risk, segmento, situacao) %>%
                         dplyr::summarise(n = n_distinct(cod_contrato))

x = data.frame(table(join$FAIXA, join$atraso_cut))