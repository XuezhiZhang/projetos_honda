require(data.table)
require(dplyr)
require(BBmisc)
require(ggplot2)
require(StatMeasures)

setwd("D:/Users/sb044936/Desktop/Modelling databases R/Judge")
initial_costs = fread("initial_costs.txt", header = TRUE, dec = ",", check.names = TRUE)
posterior_costs = fread("posterior_costs.txt", header = TRUE, dec = ",", check.names = TRUE)
fees = fread("fees.txt", header = TRUE, dec = ",", check.names = TRUE)

# vlr_presente = fread("bhbvlrprs_1028180000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character"))

y = list()
setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge")
for(i in 1:length(desired_model)){
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge/Março"))
  files = list.files(pattern=paste0("score_by_contract.*csv"))
  y = bind_rows(lapply(files, fread, colClasses = c(cod_contrato = "character", stat_model_update = "as.Date"), dec = ",", header = TRUE))
}

all_judge_contracts_scored = bind_rows(y) %>% arrange(desc(stat_model_update)) %>% group_by(cod_contrato) %>% top_n(n = 1, wt = stat_model_update)
# pred = bind_rows(pred_car, pred_mot) %>% select(cod_contrato, segmento, nome_estado_cli, nome_municipio_cli, vlr_vencido, prob_bad, prob_good, score, stat_model, stat_model_update)

# names(vlr_presente)[6] = "cod_contrato"
# x = left_join(bhb.fecha.dez.18, vlr_presente, by = "cod_contrato")

names(initial_costs)[1] = "nome_estado_cli"

join = left_join(all_judge_contracts_scored, initial_costs, by = "nome_estado_cli")
join = left_join(join, posterior_costs, by = "segmento")

join = join %>% mutate(vlr_vnc_mais_enc = vlr_vencido + vlr_medio_ajuiz + vlr_medio_despachante + vlr_medio_leiloeiro)
join = join %>% mutate(perc_vlr_vnc_mais_enc_renda = vlr_vnc_mais_enc/vlr_renda_mensal_cli)

join = join %>% mutate_if(is.numeric, funs(replace(., is.infinite(.), 0))) %>%
                mutate_if(is.numeric, funs(replace(., is.nan(.), NA)))

join = join %>% mutate(var = 1 - vlr_vencido/vlr_vnc_mais_enc)

join$scl_vlr_vencido = normalize(join$vlr_vencido, method = "range", range = c(0,1))
join$scl_vlr_vnc_mais_enc = normalize(join$vlr_vnc_mais_enc, method = "range", range = c(0,1))
join$scl_perc_vlr_vnc_mais_enc_renda = normalize(join$perc_vlr_vnc_mais_enc_renda, method = "range", range = c(0,1))
join$scl_var = normalize(join$var, method = "range", range = c(0,1))

join = join %>% mutate(ranking_ajuiz = round((0.5*(1 - scl_perc_vlr_vnc_mais_enc_renda) +
                                       0.2*scl_vlr_vencido +              
                                       0.2*prob_good + 
                                       0.1*(1-scl_var))*1000),0)

join = join %>% select("cod_contrato", "segmento", "qtd_dias_em_atraso", 
                       "nome_estado_cli", "nome_municipio_cli", "prob_bad",             
                       "prob_good","score", "cluster", "stat_model",           
                       "stat_model_update", "descricao_uf","vlr_medio_ajuiz",      
                       "vlr_medio_despachante", "vlr_medio_leiloeiro", "perc_vlr_vnc_mais_enc_renda" ,"vlr_vencido", "vlr_a_vencer", 
                       "vlr_total_financiado","modelo_cc", "modelo", "parcela_atual", "prazo_contrato",
                       "vlr_vnc_mais_enc", "var", "ranking_ajuiz") %>% arrange(desc(ranking_ajuiz)) %>% group_by(cod_contrato) %>% top_n(n = 1, wt = stat_model_update)

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge"))
fwrite(join, file = paste0("judge_ranking_by_contract_daily_", Sys.Date(),".csv"), sep = ";", dec = ",")

breaks = seq(0,1000,50)
join$ranking_break = cut(join$ranking_ajuiz, breaks = breaks)
join$ranking_break_cod = cut(join$ranking_ajuiz, breaks = breaks, labels = FALSE)

view = join %>% group_by(ranking_break) %>% 
                dplyr::summarise(n = n()) %>% filter(!is.na(ranking_break)) %>%
                mutate(prop = (cumsum(n/sum(n))), prop.n = cumsum(n), id = as.numeric(rownames(.))) %>%
                arrange(desc(id)) %>%
                mutate(prop2 = (cumsum(n/sum(n))), prop2.n = cumsum(n)) %>%
                arrange(id) %>% select(-id)

fwrite(view, file = paste0("cutoff_judge_ranking_",Sys.Date(),".csv"), sep = ";", dec = ",")

#########
# plots #
#########

plot <- ggplot(join, aes(ranking_ajuiz, perc_vlr_vnc_mais_enc_renda*100)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | 61-360",
       x = "judge ranking", 
       y = "% (owed value + charges) / income",
       color = "cluster") +
  theme(legend.position="bottom") +
  facet_wrap(~ segmento) +
  geom_hline(yintercept=100, linetype="dashed", color = "red") +
  scale_y_continuous(breaks = round(seq(min(join$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, 
                                        max(join$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, by = 100),1))
plot

plot2 <- ggplot(join, aes(ranking_ajuiz, vlr_vencido)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | 61-360",
       x = "judge ranking", 
       y = "owed value",
       color = "cluster") +
  #facet_wrap(~ segmento) +
  theme(legend.position="bottom")
plot2

plot3 <- ggplot(join, aes(ranking_ajuiz, score)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | 61-360",
       x = "judge ranking", 
       y = "collection score",
       color = "cluster") +
  facet_wrap(~ segmento) +
  theme(legend.position="bottom")
plot3

plot4 <- ggplot(join, aes(ranking_ajuiz, var)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | 61-360",
       x = "judge ranking", 
       y = "variation between (owed value) and (owed value + charges)",
       color = "cluster") +
  facet_wrap(~ segmento) +
  theme(legend.position="bottom")
plot4

#################################

plot_join = grid.arrange(plot, plot2, plot3, plot4, ncol = 2)
ggsave(file=paste0("plot_judge_ranking_",Sys.Date(),".tiff"), plot_join, width = 20, height = 10)

##############

join = fread("judge_ranking_by_contract_daily_2019-04-30.csv", dec = ",", encoding = "UTF-8")

join = join %>% mutate(saldo_devedor = vlr_vencido + vlr_a_vencer,
                       parcelas_restantes = prazo_contrato - parcela_atual,
                       saldo_amortizado = parcela_atual/prazo_contrato,
                       elegibility = ifelse(saldo_devedor <= 1500 | str_detect(modelo, paste(c("POP","TRX","CRF"), collapse = "|")) | parcelas_restantes <= 6 | saldo_amortizado >= 0.8 | segmento == "CAR" | modelo_cc >= 500, FALSE, TRUE),
                       ele_saldo_devedor = ifelse(saldo_devedor <= 1500, FALSE, TRUE),
                       ele_modelo = ifelse(str_detect(modelo, paste(c("POP","TRX","CRF","P O P"), collapse = "|")), FALSE, TRUE),
                       ele_parc_rest = ifelse(parcelas_restantes <= 6, FALSE, TRUE),
                       ele_saldo_amor = ifelse(saldo_amortizado >= 0.8, FALSE, TRUE),
                       ele_segmento = ifelse(segmento == "CAR", FALSE, TRUE),
                       ele_modelo_cc = ifelse(modelo_cc >= 500, FALSE, TRUE),
                       proposal1 = ifelse(score > 450, "ajuizamento", ifelse( 
                                          score <= 450 & score > 300, "protesto", "campanha")),
                       proposal2 = ifelse(score > 450, "protesto", ifelse( 
                                          score <= 450 & score > 300, "ajuizamento", "campanha")))

elegibility_true = join %>% filter(elegibility == TRUE)

prop.table(table(join$elegibility))
prop.table(table(elegibility_true$proposal1))
prop.table(table(elegibility_true$proposal2))
prop.table(table(join$modelo_cc, join$elegibility))

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge"))
fwrite(join, file = paste0("judge_ranking_by_contract_daily_", Sys.Date(),".csv"), sep = ";", dec = ",")

view = join %>% group_by(ranking_break) %>% 
  dplyr::summarise(n = n()) %>% filter(!is.na(ranking_break)) %>%
  mutate(prop = (cumsum(n/sum(n))), prop.n = cumsum(n), id = as.numeric(rownames(.))) %>%
  arrange(desc(id)) %>%
  mutate(prop2 = (cumsum(n/sum(n))), prop2.n = cumsum(n)) %>%
  arrange(id) %>% select(-id)

fwrite(view, file = paste0("cutoff_judge_ranking_",Sys.Date(),".csv"), sep = ";", dec = ",")
