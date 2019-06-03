require(readr)
require(dplyr)
require(ggplot2)

setwd("~/CADOCAN")
setwd("C:/CADOCAN")
widths = c(4, 3, 1, 5, 3, 1, 1, 5, 40, 6, 2, 3, 12, 2, 10, 10, 10, 3, 3, 8,
           8, 8, 8, 14, 30, 18, 18, 8, 4, 10, 8, 8, 10, 3, 10, 1, 9, 1, 8, 
           8, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 8, 8, 3, 8, 2, 8, 2, 1, 1, 12, 7, 7, 13, 9)
names <- c("EMPRESA", "ESPECIE", "ORIGEM", "GRUPO", "COTA", "R", "D", "LEI", "CONSORCIADO", 
           "CONTRATO", "SERIE", "PRAZO", "MODELO", "STATUSCOTA", "STATUSGRUPO", "PERAMOMENSA", 
           "PERAMOTTL", "QTDIASATRASO", "ASSATUAL", "DTULASSFECH", "DTULASSPRAZO", "DTINIPARTIC", 
           "DTENCGRUPO", "VLPENDDEV", "REGIAO", "VLCATSSEGURO", "VLCATCSEGURO", "DTVENDACONTRATO",
           "PLANOVENDA", "PERADIANTAMENTO", "DTADIANTAMENTO", "DTSTCONTRATO", "PERCATRASO", 
           "ASSPARTIC", "VLBEMASSFECH", "FLAGPLDOCTO", "CODCLIENTE", "TIPOCONTEMPLACAO", 
           "DATACONTEMPLACAO", "DATAENTREGABEM", "VLBENSERDTBASE", "PERLANCE", "VLCREDDTCTP",
           "VLRBEMCOMPL", "VLLANCEPAGOUEMB", "VLBEMDTULASS", "PERCFR", "PERCREDGRUPO", 
           "PERCREDADM", "PERCDCTO", "DTDESCLASSIFICACAO", "DTULTPAGTOCREDCTP", "ESPECIEREAL", 
           "DTPENULTALTERST", "STPENULTALT", "DTPENULTSTPERANT", "ULTSTPERIODOANT", "FLAGREPOSMES", 
           "FLAGCOTACORRENTE", "BEMANTERIOR", "CODIGOHDA", "CODIGOHDAENTREGA", "MODELOENTREGUE", "TXMEDIA")

dados <- readr::read_fwf(file = "cadoc_201903.txt", fwf_widths(widths, col_names = names), guess_max = 1000, col_types = cols("QTDIASATRASO" = col_integer(),
                                                                                                                              "BEMANTERIOR" = col_character()), locale = locale(decimal_mark = ","))


dados = dados %>% mutate_at(vars(starts_with("DT")), list(~as.character))
dados = dados %>% mutate_at(vars(starts_with("DATA")), list(~as.character))
dados = dados %>% mutate_at(vars(starts_with("DT")), list(~as.Date(., format = "%Y%m%d")))
dados = dados %>% mutate_at(vars(starts_with("DATA")), list(~as.Date(., format = "%Y%m%d")))
dados = dados %>% mutate(id_full = paste0(GRUPO, COTA, R, D))

active = dados %>% filter(!STATUSCOTA %in% c("DC", "DG", "KG", "KC", "PX", "CX"))
contempl = dados %>% filter(!STATUSCOTA %in% c("NM", "OR", "PK", "PR", "RO") & FLAGCOTACORRENTE == "S")
active.contempl = dados %>% filter(!STATUSCOTA %in% c("DC", "DG", "KG", "KC", "PX", "CX", "NM", "OR", "PK", "PR", "RO") & FLAGCOTACORRENTE == "S")
quit = dados %>% filter(PERAMOTTL >= 99.5)
out = active.contempl %>% filter(PERAMOTTL < 99.5)

save(out, file = "out_cadoc_201901.RData")
load("out_all_2019.RData")
save(out.all, file = "out_all_2019.RData")

deliq.cadoc = active.contempl %>% filter(QTDIASATRASO > 30) %>% summarise(n = n())

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

cadoc.03.2019 = dados; rm(dados)
save(cadoc.03.2019, file = "cadoc_0_2019.RData")
load("cadoc_03_2019.RData")
load("cadoc_12_2018.RData")
cadoc.03.2019$QTDIASATRASO = as.numeric(cadoc.03.2019$QTDIASATRASO)

###################

prop.table(table(cadoc.10.2018$TIPOCONTEMPLACAO))
prop.table(table(cadoc.10.2018$PERLANCE))
prop.table(table(cadoc.10.2018$VLRBEMCOMPL))
prop.table(table(cadoc.10.2018$PERCATRASO))
prop.table(table(cadoc.10.2018$PERCREDADM))
prop.table(table(cadoc.10.2018$VLLANCEPAGOUEMB))
prop.table(table(cadoc.10.2018$PERADIANTAMENTO))
prop.table(table(cadoc.10.2018$FLAGCOTACORRENTE))
prop.table(table(cadoc.10.2018$TXMEDIA))
prop.table(table(cadoc.10.2018$REGIAO))
prop.table(table(cadoc.10.2018$PERAMOMENSA))
prop.table(table(cadoc.10.2018$QTDIASATRASO))
prop.table(table(cadoc.10.2018$PERAMOMENSA))

ggplot(filter(cadoc.10.2018, !PERAMOMENSA == 0), aes(x=PERAMOMENSA)) + 
  geom_density(alpha=.2)

ggplot(filter(cadoc.10.2018, !QTDIASATRASO == 0), aes(x=QTDIASATRASO)) + 
  geom_density(alpha=.2)

teste = dados %>% mutate(DIAS_ATRASO_NOVO = (PERCATRASO/PERAMOMENSA) * 30)
hist(teste$DIAS_ATRASO_NOVO, xlim = c(0,750), breaks = 100)

out = teste %>% filter(PERAMOTTL <= 99.5 & STATUSCOTA %in% c("CO","CP","CS","JD","OC","PA","PC","PL","QO","QP") & STATUSGRUPO == "ATIVOS") %>% summarise(outstanding = n())   
del = teste %>% filter(DIAS_ATRASO_NOVO > 30 & STATUSCOTA %in% c("CS", "PL", "QP")) %>% summarise(delinquents = n())

table(str_detect(bhb.final$cpf_cnpj, "w"))
