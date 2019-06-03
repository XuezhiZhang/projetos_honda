require(dplyr)
require(data.table)
require(tidyverse)
require(janitor)

setwd("R:/CNH/SERASA/2019/Abr'19/bate serasa")
consolidado <- fread("consolidado.txt", sep = "\t", dec = ",", header = TRUE, na.strings = "NA", check.names = TRUE)
serasa <- fread("serasa.txt", sep = "\t", dec = ",", header = TRUE, na.strings = "NA", check.names = TRUE)

serasa = serasa %>% clean_names() %>% mutate(id = paste0(grupo, cota, r, d))
consolidado = consolidado %>% clean_names() %>% mutate(id = paste0(grupo, cota, r, d))

serasa.fiador = serasa %>% group_by(id) %>% summarise(n = n()) %>% mutate(fiador = ifelse(n > 1, "sim", "não"))
consolidado.fiador = consolidado %>% group_by(id) %>% summarise(n = n()) %>% mutate(fiador = ifelse(n > 1, "sim", "não"))

serasa = left_join(serasa, serasa.fiador, by = "id")
consolidado = left_join(consolidado, consolidado.fiador, by = "id")

serasa.not.consolidado = anti_join(serasa, consolidado, by = "id") #all id in serasa that are not matching id in consolidado
consolidado.not.serasa = anti_join(consolidado, serasa, by = "id") #all id in consolidado that are not matching id in serasa

fwrite(serasa.not.consolidado, file = "serasa_not_consolidado.csv", sep = ";", dec = ",")
fwrite(consolidado.not.serasa, file = "consolidado_not_serasa.csv", sep = ";", dec = ",")

# 3658382038

