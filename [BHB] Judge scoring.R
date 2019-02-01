require(data.table)
require(dplyr)
require(BBmisc)
require(ggplot2)

setwd("D:/Users/sb044936/Desktop/Modelling databases R/Judge")
initial_costs = fread("initial_costs.txt", header = TRUE, dec = ",", check.names = TRUE)
posterior_costs = fread("posterior_costs.txt", header = TRUE, dec = ",", check.names = TRUE)
fees = fread("fees.txt", header = TRUE, dec = ",", check.names = TRUE)

# vlr_presente = fread("bhbvlrprs_1028180000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character"))

setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Predictions")
pred_car = fread("score_by_contract_jurid_61_360_CAR_2019-01-23.csv", dec = ",", encoding = "UTF-8", colClasses = c("cod_contrato" = "character")) 
pred_mot = fread("score_by_contract_jurid_61_360_MOT_2019-01-23.csv", dec = ",", encoding = "UTF-8", colClasses = c("cod_contrato" = "character"))

# pred = bind_rows(pred_car, pred_mot) %>% select(cod_contrato, segmento, nome_estado_cli, nome_municipio_cli, vlr_vencido, prob_bad, prob_good, score, stat_model, stat_model_update)

# names(vlr_presente)[6] = "cod_contrato"
# x = left_join(bhb.fecha.dez.18, vlr_presente, by = "cod_contrato")

names(initial_costs)[1] = "nome_estado_cli"

join_car = left_join(pred_car, initial_costs, by = "nome_estado_cli")
join_car = left_join(join_car, posterior_costs, by = "segmento")

join_car = join_car %>% mutate(vlr_vnc_mais_enc = vlr_vencido + vlr_medio_ajuiz + vlr_medio_despachante +vlr_medio_leiloeiro)
join_car = join_car %>% mutate(perc_vlr_vnc_mais_enc_renda = vlr_vnc_mais_enc/vlr_renda_mensal_cli)

join_car = join_car %>% mutate_if(is.numeric, funs(replace(., is.infinite(.), 0))) %>%
                        mutate_if(is.numeric, funs(replace(., is.nan(.), NA)))

join_car = join_car %>% mutate(var = 1 - vlr_vencido/vlr_vnc_mais_enc)

# join_car$scl_prob_good = normalize(join_car$prob_good, method = "range")
# join_car$scl_vlr_medio_ajuiz = normalize(join_car$vlr_medio_ajuiz, method = "range")
# join_car$scl_vlr_medio_despachante = normalize(join_car$vlr_medio_despachante, method = "range")
# join_car$scl_vlr_medio_leiloeiro = normalize(join_car$vlr_medio_leiloeiro, method = "range")

join_car$scl_vlr_vencido = normalize(join_car$vlr_vencido, method = "range", range = c(0,1))
join_car$scl_vlr_vnc_mais_enc = normalize(join_car$vlr_vnc_mais_enc, method = "range", range = c(0,1))
join_car$scl_perc_vlr_vnc_mais_enc_renda = normalize(join_car$perc_vlr_vnc_mais_enc_renda, method = "range", range = c(0,1))
join_car$scl_var = normalize(join_car$var, method = "range", range = c(0,1))

join_car = join_car %>% mutate(ranking_ajuiz = 0.4*(1 - scl_perc_vlr_vnc_mais_enc_renda) +
                                               0.3*scl_vlr_vencido +              
                                               0.2*prob_good + 
                                               0.1*(1-scl_var))

join_car = join_car %>% select("cod_contrato", "segmento", "qtd_dias_em_atraso", 
                               "nome_estado_cli", "nome_municipio_cli", "prob_bad",             
                               "prob_good","score", "cluster", "stat_model",           
                               "stat_model_update", "descricao_uf","vlr_medio_ajuiz",      
                               "vlr_medio_despachante", "vlr_medio_leiloeiro", "perc_vlr_vnc_mais_enc_renda" ,"vlr_vencido", 
                               "vlr_vnc_mais_enc", "var", "ranking_ajuiz")

##############

plot <- ggplot(join_car, aes(ranking_ajuiz, perc_vlr_vnc_mais_enc_renda*100)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | CAR | 61-360",
       x = "judge ranking", 
       y = "% (owed value + charges) / income",
       color = "cluster") +
  theme(legend.position="bottom") +
  geom_hline(yintercept=100, linetype="dashed", color = "red") +
  scale_y_continuous(breaks = round(seq(min(join_car$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, 
                                        max(join_car$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, by = 100),1))

plot <- ggplot(join_car, aes(ranking_ajuiz, vlr_vencido)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | CAR | 61-360",
       x = "judge ranking", 
       y = "owed value",
       color = "cluster") +
  theme(legend.position="bottom")

plot <- ggplot(join_car, aes(ranking_ajuiz, score)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | CAR | 61-360",
       x = "judge ranking", 
       y = "collection score",
       color = "cluster") +
  theme(legend.position="bottom")

plot <- ggplot(join_car, aes(ranking_ajuiz, var)) + 
  geom_point(aes(colour = factor(cluster))) + 
  scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
  labs(title = "Judge Ranking | CAR | 61-360",
       x = "judge ranking", 
       y = "variation between (owed value) and (owed value + charges)",
       color = "cluster") +
  theme(legend.position="bottom")

plot

################

join_mot = left_join(pred_mot, initial_costs, by = "nome_estado_cli")
join_mot = left_join(join_mot, posterior_costs, by = "segmento")

join_mot = join_mot %>% mutate(vlr_vnc_mais_enc = vlr_vencido + vlr_medio_ajuiz + vlr_medio_despachante +vlr_medio_leiloeiro)
join_mot = join_mot %>% mutate(perc_vlr_vnc_mais_enc_renda = vlr_vnc_mais_enc/vlr_renda_mensal_cli)

join_mot = join_mot %>% mutate_if(is.numeric, funs(replace(., is.infinite(.), 0))) %>%
                        mutate_if(is.numeric, funs(replace(., is.nan(.), NA)))

join_mot = join_mot %>% mutate(var = 1 - vlr_vencido/vlr_vnc_mais_enc)

# join_mot$scl_prob_good = normalize(join_mot$prob_good, method = "range")
# join_mot$scl_vlr_medio_ajuiz = normalize(join_mot$vlr_medio_ajuiz, method = "range")
# join_mot$scl_vlr_medio_despachante = normalize(join_mot$vlr_medio_despachante, method = "range")
# join_mot$scl_vlr_medio_leiloeiro = normalize(join_mot$vlr_medio_leiloeiro, method = "range")

join_mot$scl_vlr_vencido = normalize(join_mot$vlr_vencido, method = "range", range = c(0,1))
join_mot$scl_vlr_vnc_mais_enc = normalize(join_mot$vlr_vnc_mais_enc, method = "range", range = c(0,1))
join_mot$scl_perc_vlr_vnc_mais_enc_renda = normalize(join_mot$perc_vlr_vnc_mais_enc_renda, method = "range", range = c(0,1))
join_mot$scl_var = normalize(join_mot$var, method = "range", range = c(0,1))

join_mot = join_mot %>% mutate(ranking_ajuiz = 0.4*(1 - scl_perc_vlr_vnc_mais_enc_renda) +
                                               0.3*scl_vlr_vencido +              
                                               0.2*prob_good + 
                                               0.1*(1-scl_var))

join_mot = join_mot %>% select("cod_contrato", "segmento", "nome_estado_cli",      
                               "nome_municipio_cli", "prob_bad",             
                               "prob_good","score", "cluster", "stat_model",           
                               "stat_model_update", "descricao_uf","vlr_medio_ajuiz",      
                               "vlr_medio_despachante", "vlr_medio_leiloeiro", "perc_vlr_vnc_mais_enc_renda" ,"vlr_vencido", 
                               "vlr_vnc_mais_enc", "var", "ranking_ajuiz")

plot <- ggplot(join_mot, aes(ranking_ajuiz, perc_vlr_vnc_mais_enc_renda*100)) + 
        geom_point(aes(colour = factor(cluster))) + 
        scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
        labs(title = "Judge Ranking | MOT | 61-360",
             x = "judge ranking", 
             y = "% (owed value + charges) / income",
             color = "cluster") +
        theme(legend.position="bottom") +
        geom_hline(yintercept=100, linetype="dashed", color = "red") +
        scale_y_continuous(breaks = round(seq(min(join_mot$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, 
                                              max(join_mot$perc_vlr_vnc_mais_enc_renda, na.rm = TRUE)*100, by = 100),1))

plot <- ggplot(join_mot, aes(ranking_ajuiz, vlr_vencido)) + 
        geom_point(aes(colour = factor(cluster))) + 
        scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
        labs(title = "Judge Ranking | MOT | 61-360",
             x = "judge ranking", 
             y = "owed value (R$)",
             color = "cluster") +
        theme(legend.position="bottom")

plot <- ggplot(join_mot, aes(ranking_ajuiz, score)) + 
        geom_point(aes(colour = factor(cluster))) + 
        scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
        labs(title = "Judge Ranking | MOT | 61-360",
             x = "judge ranking", 
             y = "collection score",
             color = "cluster") +
        theme(legend.position="bottom")

plot <- ggplot(join_mot, aes(ranking_ajuiz*1000, var)) + 
        geom_point(aes(colour = factor(cluster))) + 
        scale_color_manual(values=c("darkorange1", "yellow2", "brown2", "green4")) +
        labs(title = "Judge Ranking | MOT | 61-360",
             x = "judge ranking (greater, better to ", 
             y = "variation between (owed value) and (owed value + charges)",
             color = "cluster") +
        theme(legend.position="bottom")

plot

# Pattern of top ranking = Pessoas com alta probabilidade de pagar, 
#                          que possuem poder financeiro pra pagar, 
#                          possuem altos valores vencido e 
#                          custos de encargos no ajuizamento mais baixo

####################
