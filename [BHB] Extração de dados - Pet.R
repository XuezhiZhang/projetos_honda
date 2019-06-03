list = fread("list_contratos.txt", header = TRUE, colClasses = c("cod_contrato" = "character"))

list = list %>% distinct()

agrup = agrup %>% rename("modelo" = "nome_modelo")
bhb.final = left_join(bhb.final, agrup, by = "modelo")

base_fecha_filt = bhb.final %>% select(cod_contrato, nome_hda, nome_est_loja, modelo, familia_bem, segmento.x) %>% filter(cod_contrato %in% list$cod_contrato) %>% distinct() %>% mutate(id = rownames(base_fecha_filt))
x = duplicated(base_fecha_filt$cod_contrato)
z = base_fecha_filt[x,]

base_fecha_filt = base_fecha_filt %>% filter(!id %in% z$id)

cod.fecha = c(base_fecha_filt %>% select(cod_contrato) %>% distinct())$cod_contrato
cod.list = c(list$cod_contrato)

##################

base_fecha_filt = db.04.2019 %>% select(cod_contrato, nome_est_loja, nome_modelo, familia_bem, segmento) %>% filter(cod_contrato %in% list$cod_contrato) %>% distinct()

anti_join(list, base_fecha_filt, by = "cod_contrato")

Encoding(base_fecha_filt$modelo) <- "UTF-8"
fwrite(base_fecha_filt, file = "base_fecha_filt.csv", sep = ";")
write.csv(base_fecha_filt,"base_fecha_filt.csv",fileEncoding = "UTF-8")

miss.contracts = anti_join(list, base_fecha_filt, by = "cod_contrato")
fwrite(miss.contracts, file = "miss.csv", sep = ";")

out <- file("base_fecha_filt.csv", "w", encoding="UTF-8") 
write.table(base_fecha_filt, out, row.names=FALSE) 
close(out) 
