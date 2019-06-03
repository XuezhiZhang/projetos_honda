require(readr)
require(dplyr)
require(ggplot2)
require(data.table)
require(stringr)
require(tidyr)

setwd("R:/Estatística/CNH/Databases CNH/Historical Payments CNH/")
x = sapply(list.files(pattern = "*.txt"), fread, na.strings = c("", " ", "#N/D"), dec = ",")

cnh.hist.paym.2019 = bind_rows(x) %>% select(-c("&", "&&", "&&&", "GrupoCotaRd", "Coluna2", "PrimeiroDeTIPO_REPASSE"))

cnh.hist.paym.2019$DT_PAGTO = as.Date(cnh.hist.paym.2019$DT_PAGTO, format = "%d/%m/%Y")
cnh.hist.paym.2019$DT_RECTO = as.Date(cnh.hist.paym.2019$DT_RECTO, format = "%d/%m/%Y")
cnh.hist.paym.2019 = cnh.hist.paym.2019 %>% mutate(COTA_full = str_pad(COTA, width=3, side="left", pad="0"),
                                                   ID = paste0(GRUPO, COTA, R, D),
                                                   ID_full = paste0(GRUPO, COTA_full, R, D)) %>% 
                                            filter(!is.na(GRUPO))

sapply(cnh.hist.paym.2019, class)
save(cnh.hist.paym.2019, file = "cnh_hist_paym_2019.RData")

load("cnh_hist_paym_2018.RData")

cnh.hist.paym.2015$YEAR = "2015"
cnh.hist.paym.2016$YEAR = "2016"
cnh.hist.paym.2017$YEAR = "2017"
cnh.hist.paym.2018$YEAR = "2018"
cnh.hist.paym.2019$YEAR = "2019"

cnh.hist.paym = bind_rows(cnh.hist.paym.2015, cnh.hist.paym.2016, 
                          cnh.hist.paym.2017, cnh.hist.paym.2018,
                          cnh.hist.paym.2019)

names(cnh.hist.paym) <- c("grupo", "cota", "r", "d", "data_rcto", 
                          "data_pgto", "valor_pago", "escritorio", 
                          "prazo_repasse", "prazo_real", "cota_full", 
                          "id", "id_full", "ano_ref")

setwd("R:/Estatística/CNH/Databases CNH/Historical Payments CNH/")
save(cnh.hist.paym, file = "cnh_hist_paym.RData")
load("cnh_hist_paym.RData")

plot3 <- ggplot(cnh.hist.paym, aes(prazo_real, colour = escritorio)) + 
  geom_density() + xlim(0,6) +
  facet_wrap(~ ano_ref) +
  theme(legend.position="bottom")
plot3

payments = cnh.hist.paym %>% group_by(id_full, ano_ref) %>% summarise(n = n())

payments.spread = spread(payments, ano_ref, n)
payments.spread = payments.spread %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
payments.spread = payments.spread %>% mutate(n = sum(`2015`, `2016`, `2017`, `2018`, `2019`, na.rm = TRUE))


cadoc.03.2019 = cadoc.03.2019 %>% mutate(id_full = paste0(GRUPO, COTA, R, D))
clients = cadoc.03.2019 %>% group_by(CODCLIENTE, STATUSGRUPO) %>% summarise(n = n())
clients.spread = spread(clients, STATUSGRUPO, n)
clients.spread = clients.spread %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
clients.spread = clients.spread %>% mutate(n = sum(ATIVOS, ENCERRADOS, na.rm = TRUE))

clients.status = cadoc.03.2019 %>% mutate(ifelse(STATUSDELAY = STATUSCOTA %in% c("CS", "PL", "QP"), "DELAY", "NOT DELAY"))

numbers_only <- function(x) !grepl("\\D", x)
table(numbers_only(x$DATACONTEMPLACAO))
