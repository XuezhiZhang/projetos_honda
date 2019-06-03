require(data.table)
require(janitor)
require(dplyr)
require(stringr)

setwd("~/")

fourw = fread("COTAS_ATIVAS_CP_4W_INAD_VALOR.txt", dec = ",")
twow = fread("COTAS_ATIVAS_CP_2W_INAD_VALOR.txt", dec = ",")

names(fourw) = names(twow) = c("Especie", "modelo", "empresa", "status", "contemplacao", "data da venda", "escritorio", "estado", "municipio", "concessionaria", "Plano", "Dias atraso", "Grupo", "Cota", "R", "D", "amortizacao", "Atraso", "Mensal", "Pagamento mes", "Parcelas pagas", "Prazo", "Tipo de cota", "Valor atraso", "Categoria", "Vinculdado", "% a Vencer", "Inad_Valor")

all = bind_rows(fourw, twow) %>% clean_names()
all = all %>% mutate(cota = str_pad(cota, 3, pad = "0"),
                     id_full = str_trim(paste0(grupo, cota, r, d)))

deliq.fecha = all %>% filter(dias_atraso > 30) %>% summarise(n = n())

tem.cadoc.nao.tem.fecha = anti_join(deliq.cadoc, deliq.fecha, by = "id_full")
        

