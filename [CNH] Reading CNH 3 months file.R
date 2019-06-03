require(readr)
require(dplyr)
require(ggplot2)
require(data.table)
require(janitor)

setwd("D:/Users/sb044936/Desktop/Bases CNH")

pf <- fread("pf.txt", sep = "\t", dec = ",", header = TRUE, na.strings = "", check.names = TRUE)
pj <- fread("pj.txt", sep = "\t", dec = ",", header = TRUE, na.strings = "", check.names = TRUE)

# renaming pf variables
pf$FLGBEMNOVO  = NULL
pf = pf %>% dplyr::rename("FLGBEMNOVO" = "FLGBEMNOVO.1") %>% clean_names() %>% 
            mutate(id_full = paste0(codgrupo, codcota, cotreposicao, digcota),
                                    tipopessoa = "pf")
# renaming pf
pj$FLGBEMNOVO  = NULL
pj = pj %>% dplyr::rename("FLGBEMNOVO" = "FLGBEMNOVO.1") %>% clean_names() %>% 
            mutate(id_full = paste0(codgrupo, codcota, cotreposicao, digcota), 
                                    tipopessoa = "pj")

pf_numeric = pf %>% select_if(is.numeric) %>% select(-c(idcota, idgrupo, idproposta, idfichacadastral,
                                                        codgrupo, codcota, cotreposicao, digcota,
                                                        idpessoa, idserasa, idfabricante, idtipocontemplacao,
                                                        codhonda, nome_ven, cpf_vend))

cor = data.frame(cor(pf_numeric))

require(qtlcharts)
iplotCorr(pf_numeric)

dados_numeric = dados_numeric %>% select()

payments = cnh.hist.paym %>% group_by(id_full) %>% dplyr::summarise(payments = n())

join = left_join(pf, payments)
join = join %>% mutate_at(vars(starts_with("dta")), list(~as.character))
join = join %>% mutate_at(vars(starts_with("dta")), list(~as.Date(., format = "%d/%m/%Y")))
join = join %>% mutate_at(vars(starts_with("nasc")), list(~as.character))
join = join %>% mutate_at(vars(starts_with("nasc")), list(~as.Date(., format = "%d/%m/%Y")))

join = join %>% mutate(idade_cli = as.integer(time_length(difftime(Sys.Date(), dtanascimento), "years")),
                       idade_fia = as.integer(time_length(difftime(Sys.Date(), nasc_fia), "years")))

join$dta_lance = substring(pf$dta_lance, 1, 10)
join = join %>% mutate(dta_lance = as.Date(dta_lance))

cnh.hist.paym = left_join()

x = pf %>% group_by(idpessoa) %>% dplyr::summarise(n = n_distinct(id_full))

idades = join %>% mutate(idade = idade_cli,
                         tipo = "CLI") %>% select(id_full, idade, tipo)
idades.2 = join %>% mutate(idade = idade_fia,
                         tipo = "FIA") %>% select(id_full, idade, tipo)

idades = bind_rows(idades, idades.2) %>% distinct

p1 <- idades %>% 
  ggplot(aes(idade, color = factor(tipo), fill = factor(tipo))) + 
  geom_density(alpha = 0.3) + 
  scale_color_manual(values=c("green4", "brown2")) +
  scale_fill_manual(values=c("green4", "brown2")) +
  guides(fill = "legend", colour = "none") +
  theme(legend.position="bottom") +
  labs(x = "idade", y = "density", fill = "tipo", color = "none")

tempo_prim_pgto = cnh.hist.paym %>% select(id_full, data_pgto) %>% distinct()
tempo_prim_pgto = inner_join(tempo_prim_pgto, select(join, id_full, dtapagamentonotafiscal)) %>% distinct() %>% mutate(dif = data_pgto - dtapagamentonotafiscal)

rendas = join %>% mutate(renda = vlrrendamensal,
                         tipo = "CLI") %>% select(id_full, renda, tipo)
rendas.2 = join %>% mutate(renda = renda_fia,
                           tipo = "FIA") %>% select(id_full, renda, tipo)

rendas = bind_rows(rendas, rendas.2) %>% distinct

p1 <- rendas %>% 
  ggplot(aes(renda, color = factor(tipo), fill = factor(tipo))) + 
  geom_density(alpha = 0.3) + 
  scale_color_manual(values=c("green4", "brown2")) +
  scale_fill_manual(values=c("green4", "brown2")) +
  xlim(0,7000) +
  guides(fill = "legend", colour = "none") +
  theme(legend.position="bottom") +
  labs(x = "idade", y = "density", fill = "tipo", color = "none")

rendas.summary = rendas %>% group_by(tipo) %>% dplyr::summarise(mean_renda = mean(renda, na.rm = TRUE),
                                                                median_renda = median(renda, na.rm = TRUE))

join = join %>% mutate(tempo_ate_lance = time_length(difftime(dta_lance,dtavenda), "months"),
                       perc_ass_inicio = numassinicial/numultassfch)


p1 <- join %>% 
  ggplot(aes(perc_ass_inicio, color = factor(codprazo), fill = factor(codprazo))) + 
  geom_density(alpha = 0.3) +
  facet_wrap

