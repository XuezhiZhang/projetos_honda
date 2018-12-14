require(dplyr)
require(data.table)

cnh_2w_nov <- fread("cnh_nov_mot.txt", dec = ",", sep = "\t")
cnh_4w_nov <- fread("cnh_nov_car.txt", dec = ",", sep = "\t")

cnh_nov <- bind_rows(cnh_2w_nov, cnh_4w_nov)
cnh_nov = cnh_nov %>% mutate(id = paste0(Grupo, Cota, R, D))

out = cnh_nov %>% filter(amortizacao <= 99.5) %>% group_by(estado, municipio, concessionaria, Especie) %>% summarise(outstanding = n())
del = cnh_nov %>% filter(`Dias atraso` > 30 & status %in% c("CS", "PL", "QP")) %>% group_by(estado, municipio, concessionaria, Especie) %>% summarise(delinquents = n())

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

vlr_31_90 = cnh_nov %>% filter(`Dias atraso` >= 31 & `Dias atraso` <= 90) %>% group_by(estado, municipio, concessionaria) %>% summarise(vlr_vencido_31_90 = sum(`Valor atraso`, na.rm = TRUE))
vlr_91_180 = cnh_nov %>% filter(`Dias atraso` >= 91 & `Dias atraso` <= 180) %>% group_by(estado, municipio, concessionaria) %>% summarise(vlr_vencido_91_180 = sum(`Valor atraso`, na.rm = TRUE))
vlr_181_360 = cnh_nov %>% filter(`Dias atraso` >= 181 & `Dias atraso` <= 360) %>% group_by(estado, municipio, concessionaria) %>% summarise(vlr_vencido_181_360 = sum(`Valor atraso`, na.rm = TRUE))
vlr_360_mais = cnh_nov %>% filter(`Dias atraso` > 360) %>% group_by(estado, municipio, concessionaria) %>% summarise(vlr_vencido_360_mais = sum(`Valor atraso`, na.rm = TRUE))

vlr = full_join(vlr_31_90, vlr_91_180)
vlr = full_join(vlr, vlr_181_360)
vlr = full_join(vlr, vlr_360_mais)

geral = cnh_nov %>% group_by(estado, municipio, concessionaria, Especie) %>% summarise(active_contracts = n(),
                                                                                           vlr_total_vencido = sum(`Valor atraso`, na.rm = TRUE),
                                                                                           ticket_medio = vlr_total_vencido/active_contracts)

ticket_mun <- cnh_nov %>% group_by(municipio, Especie) %>% summarise(active_contracts_mun = n(),
                                                                            vlr_total_vencido_mun = sum(`Valor atraso`, na.rm = TRUE),
                                                                            ticket_medio_mun = vlr_total_vencido_mun/active_contracts_mun)

final = full_join(all, vlr)
final = full_join(final, geral)
final = full_join(final, ticket_mun)

final = final %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
final = final %>% rename("segmento" = Especie)

fwrite(final, file = "CNH - Concessionarias.csv", dec = ",", sep = ";")

ggplot(final, aes(x = index, fill = segmento)) + geom_density(alpha=.3) + 
  geom_vline(xintercept=c(1.95, 2.05, 14.5, 15.25),
             color=c("yellow", "red", "yellow", "red"), linetype="dashed", size=0.8) +
  scale_x_continuous(name = "Delinquency index (%)", limits = c(0,40), breaks = c(seq(0,40, by = 2.5))) +
  scale_y_continuous(name = "Dealers distribution") +
  ggtitle("BHB - Delinquency index by dealer")

z  = final %>% group_by(Especie) %>% summarise(outstanding = sum(outstanding, na.rm = TRUE),
                                                delinquents = sum(delinquents, na.rm = TRUE),
                                                index = delinquents/outstanding)

