require(dplyr)
require(data.table)

setwd("R:/BHB/Régua Digital/2019/05 - Mai´19/COBCRED 120 ~ 300 Dias")
contracts <- fread("base_renegociacao_cobcred.txt", colClasses = c("cod_contrato" = "character"))

setwd("~/IGB/Histórico Fechamento/2019/Março")
load("2019_03.RData")

# FILTERING NECESSARY CONTRACTS AND MANIPULATING DATE COLUMNS 
db.03.2019 = db.03.2019 %>% subset(qtd_parc_restantes > 0)
db.03.2019 = db.03.2019 %>% subset(status_contrato == "ATV")
db.03.2019 = db.03.2019 %>% subset(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
db.03.2019 = db.03.2019 %>% filter(qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360)

out_by_assess = db.03.2019 %>% group_by(assessoria_cob) %>% summarise(n = n_distinct(cod_contrato))

x = inner_join(contracts, db.03.2019, by = "cod_contrato")

cob_by_assess = x %>% group_by(assessoria_cob) %>% summarise(n = n_distinct(cod_contrato))

comp = left_join(out_by_assess, cob_by_assess, by = "assessoria_cob", suffix = c("outstanding", "cob"))

summary = x %>% group_by(assessoria_cob) %>% summarise(n = n())
summary = x %>% group_by(familia_bem) %>% summarise(n = n())
fwrite(comp, file = "summary.csv", sep = ";")

levels(as.factor(x$assessoria_cob))
levels(as.factor(x$assessoria))

z = x %>% group_by(assessoria_cob) %>% summarise(n=n())

