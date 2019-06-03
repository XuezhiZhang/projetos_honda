require(dplyr)
require(data.table)

setwd("~/IGB/Histórico Fechamento/2018")
load("2018.RData")

#ADD SEGMENT AS A GROUPING VARIABLE :)

db.2018 = db.2018 %>% select(cod_contrato, segmento, qtd_dias_em_atraso, data_safra)

db.2018 = db.2018 %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")),
                                       funs(as.Date(as.character(.),
                                                    format = "%d/%m/%Y")))

db.2018$cut = cut(db.2018$qtd_dias_em_atraso, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE)
db.2018 = db.2018 %>% mutate(cut = ifelse(qtd_dias_em_atraso <= 0, "no delay", cut))
db.2018 = db.2018 %>% mutate(cut = ifelse(qtd_dias_em_atraso > 90, "90 +", cut))

current = db.2018 %>% filter(data_safra == "2018-02-01") %>% select(-segmento)
one.month = db.2018 %>% filter(data_safra == "2018-03-02")
three.months = db.2018 %>% filter(data_safra == "2018-05-02")
six.months = db.2018 %>% filter(data_safra == "2018-08-01")
nine.months = db.2018 %>% filter(data_safra == "2018-11-01")

one.month.join = left_join(current, one.month, by = "cod_contrato", suffix = c(".current", ".one"))
three.months.join = left_join(current, three.months, by = "cod_contrato", suffix = c(".current", ".three"))
six.months.join = left_join(current, six.months, by = "cod_contrato", suffix = c(".current", ".six"))
nine.months.join = left_join(current, nine.months, by = "cod_contrato", suffix = c(".current", ".nine"))

one.table = as.data.frame.matrix(table(one.month.join$cut.current, one.month.join$cut.one))
three.table = as.data.frame.matrix(table(three.months.join$cut.current, three.months.join$cut.three))
six.table = as.data.frame.matrix(table(six.months.join$cut.current, six.months.join$cut.six))
nine.table = as.data.frame.matrix(table(nine.months.join$cut.current, nine.months.join$cut.nine))

one.month.join.car = one.month.join %>% filter(segmento == "CAR")
three.months.join.car = three.months.join %>% filter(segmento == "CAR")
six.months.join.car = six.months.join %>% filter(segmento == "CAR")
nine.months.join.car = nine.months.join %>% filter(segmento == "CAR")

one.table.car = as.data.frame.matrix(table(one.month.join.car$cut.current, one.month.join.car$cut.one))
three.table.car = as.data.frame.matrix(table(three.months.join.car$cut.current, three.months.join.car$cut.three))
six.table.car = as.data.frame.matrix(table(six.months.join.car$cut.current, six.months.join.car$cut.six))
nine.table.car = as.data.frame.matrix(table(nine.months.join.car$cut.current, nine.months.join.car$cut.nine))

one.month.join.mot = one.month.join %>% filter(segmento == "MOT")
three.months.join.mot = three.months.join %>% filter(segmento == "MOT")
six.months.join.mot = six.months.join %>% filter(segmento == "MOT")
nine.months.join.mot = nine.months.join %>% filter(segmento == "MOT")

one.table.mot = as.data.frame.matrix(table(one.month.join.mot$cut.current, one.month.join.mot$cut.one))
three.table.mot = as.data.frame.matrix(table(three.months.join.mot$cut.current, three.months.join.mot$cut.three))
six.table.mot = as.data.frame.matrix(table(six.months.join.mot$cut.current, six.months.join.mot$cut.six))
nine.table.mot = as.data.frame.matrix(table(nine.months.join.mot$cut.current, nine.months.join.mot$cut.nine))

fwrite(one.table, file = "one_table.csv", sep = ";", row.names = TRUE)
fwrite(three.table, file = "three_table.csv", sep = ";", row.names = TRUE)
fwrite(six.table, file = "six_table.csv", sep = ";", row.names = TRUE)
fwrite(nine.table, file = "nine_table.csv", sep = ";", row.names = TRUE)

fwrite(one.table.car, file = "one_table_4W.csv", sep = ";", row.names = TRUE)
fwrite(three.table.car, file = "three_table_4W.csv", sep = ";", row.names = TRUE)
fwrite(six.table.car, file = "six_table_4W.csv", sep = ";", row.names = TRUE)
fwrite(nine.table.car, file = "nine_table_4W.csv", sep = ";", row.names = TRUE)

fwrite(one.table.mot, file = "one_table_2W.csv", sep = ";", row.names = TRUE)
fwrite(three.table.mot, file = "three_table_2W.csv", sep = ";", row.names = TRUE)
fwrite(six.table.mot, file = "six_table_2W.csv", sep = ";", row.names = TRUE)
fwrite(nine.table.mot, file = "nine_table_2W.csv", sep = ";", row.names = TRUE)


