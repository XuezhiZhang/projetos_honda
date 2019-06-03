require(data.table)
require(dplyr)

CAR <- fread("2W.txt", sep = "\t", colClasses = c("Contrato" = "character"))
MOT <- fread("4W.txt", sep = "\t", colClasses = c("Contrato" = "character"))

MARCELL <- bind_rows(CAR, MOT)

out.marcell = MARCELL %>% filter(Dias <= 360 & Sit %in% c("CCA","CCJ","CEA","CED")) %>% group_by(Seg, Contrato) %>% summarise(outstanding = sum(Qtd, na.rm= TRUE))
out.igb = bhb.final %>% filter(qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% group_by(segmento, cod_contrato) %>% summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
names(out.marcell)[2] = "cod_contrato"; out.marcell$cod_contrato = as.character(out.marcell$cod_contrato)


out <- anti_join(out.igb, out.marcell, by = "cod_contrato")

out.2 <- left_join(out, bhb.final, by = "cod_contrato")
