require(lubridate)

march.db = db.2019 # %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")) #&
                                                # qtd_dias_em_atraso > 0 &
                                                # qtd_dias_em_atraso <= 30)

march.db = march.db %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                           list(~as.Date(as.character(.), 
                                                         format = "%d/%m/%Y")))

march.db = left_join(march.db, count.all)

march.db$vlr_renda_mensal_cli = ifelse(march.db$vlr_renda_mensal_cli == 1, NA, 
                                         march.db$vlr_renda_mensal_cli) 

march.db$max_dif_entre_pgto = as.numeric(march.db$max_dif_entre_pgto)

# march.db.201607.201706$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), march.db$data_nascimento), "years"))
march.db$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date(as.character("2019-03-31"), format = "%Y-%m-%d"), march.db$data_contrato), "years"))
march.db$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date(as.character("2019-03-31"), format = "%Y-%m-%d"), march.db$data_contrato), "months"))
march.db$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date(as.character("2019-03-31"), format = "%Y-%m-%d"), march.db$data_ult_pgt), "days"))

march.db$`perc_parc_pagas` = march.db$qtd_parc_pagas / (march.db$qtd_parc_pagas + march.db$qtd_parc_restantes)
march.db$`perc_vnc_finan` = march.db$vlr_vencido / march.db$vlr_total_financiado
march.db$`perc_vnc_renda` = march.db$vlr_vencido / march.db$vlr_renda_mensal_cli
march.db$`perc_vnc_bens` = march.db$vlr_vencido / march.db$vlr_total_bens
march.db$`perc_ult_pgt_parc` = march.db$vlr_ult_pgt / march.db$vlr_parcela
march.db$`perc_a_vencer_finan` = march.db$vlr_a_vencer / march.db$vlr_total_financiado

march.db$`perc_pg_atr_1_10` = march.db$qtd_pg_atr_1_10 / march.db$qtd_pg_atr_12_meses
march.db$`perc_pg_atr_1_15` = march.db$qtd_pg_atr_1_15 / march.db$qtd_pg_atr_12_meses
march.db$`perc_pg_atr_16_30` = march.db$qtd_pg_atr_16_30/ march.db$qtd_pg_atr_12_meses
march.db$`perc_pg_atr_1_60` = march.db$qtd_pg_atr_1_60 / march.db$qtd_pg_atr_12_meses
march.db$`perc_pg_atr_11_60` = (march.db$qtd_pg_atr_11_30 + march.db$qtd_pg_atr_31_60) / (march.db$qtd_pg_atr_12_meses)
march.db$`perc_pg_atr_61_360` = march.db$qtd_pg_atr_61_360 / march.db$qtd_pg_atr_12_meses
march.db$`perc_pg_atr_360_mais` = march.db$qtd_pg_atr_360_mais/ march.db$qtd_pg_atr_12_meses

march.db$`perc_parc_renda` = march.db$vlr_parcela/march.db$vlr_renda_mensal_cli
march.db$`perc_pg_finan` = (march.db$vlr_parcela*march.db$qtd_parc_pagas)/march.db$vlr_total_financiado

#3 above lines are generating -inf values :(
march.db$perc_pg_atr_12_1 = march.db$qtd_pg_atr_12_meses/march.db$qtd_pg_atr_1_mes
march.db$perc_pg_atr_7_1 = march.db$qtd_pg_atr_7_meses/march.db$qtd_pg_atr_1_mes
march.db$perc_pg_atr_4_1 = march.db$qtd_pg_atr_4_meses/march.db$qtd_pg_atr_1_mes

march.db$tempo_ate_primeiro_atr = as.integer(march.db$data_primeiro_atraso - march.db$data_contrato)

march.db = march.db %>% mutate_at(vars( qtd_pg_atr_1_mes, vlr_pg_atr_1_mes,      
                                            qtd_pg_atr_4_meses, vlr_pg_atr_4_meses, qtd_pg_atr_7_meses, 
                                            vlr_pg_atr_7_meses, qtd_pg_atr_12_meses, vlr_pg_atr_12_meses,   
                                            qtd_pg_1_mes, valor_pg_1_mes, qtd_pg_4_meses,       
                                            valor_pg_4_meses, qtd_pg_7_meses, valor_pg_7_meses,      
                                            qtd_pg_12_meses, valor_pg_12_meses, qtd_pg_atr_1_10,
                                            vlr_pg_atr_1_10, qtd_pg_atr_1_15, vlr_pg_atr_1_15, 
                                            qtd_pg_atr_16_30, vlr_pg_atr_16_30,
                                            qtd_pg_atr_1_30, vlr_pg_atr_1_30, 
                                            qtd_pg_atr_1_60,qtd_pg_atr_360_mais, vlr_pg_atr_360_mais,
                                            vlr_pg_atr_1_60, qtd_pg_atr_11_30, vlr_pg_atr_11_30, 
                                            qtd_pg_atr_11_60, qtd_pg_atr_31_60, vlr_pg_atr_31_60, 
                                            qtd_pg_atr_61_360, vlr_pg_atr_61_360, qtd_pg_atr_1_360, 
                                            vlr_pg_atr_1_360), list(~replace(., is.na(.), 0)))

# march.db = march.db %>% mutate_at(vars(starts_with("perc_")), list(~replace(., is.na(.), 0)))

db_to_predict = march.db %>% select(-c(cpf_cnpj, data_safra, 
                                         data_contrato, data_primeiro_atraso, 
                                         data_ult_pgt, data_vencimento, status_contrato, 
                                         num_chassi, tabela, assessoria, assessoria_cob, 
                                         cod_hda))

#######
#SCORE#
#######

require(stringr)
require(dplyr)
require(broom)
require(scorecard)
require(caret)
require(foreach)
require(glmnet)
require(gridExtra)
require(tibble)
require(reshape2)
require(plyr)
require(ROCR)
require(mice)

db_to_predict = db_to_predict %>% filter(segmento == segment)
db_to_predict = db_to_predict %>% select(-c(starts_with("data_")))

# setting to null because these columns are duplicating levels
bins_mod$perc_parc_renda = NULL
bins_mod$perc_vnc_renda = NULL

#WOE
db_woe = woebin_ply(db_to_predict, bins_mod)

db_woe$score_woe = db_woe$perc_vnc_renda_woe = db_woe$perc_parc_renda_woe =  0
db_to_predict = db_woe %>% select(lasso.model[["beta"]]@Dimnames[[1]])

registerDoSEQ()
# preproc = preProcess(db_to_predict, method = "knnImpute", k = 5)
# preproc$method$center = NULL
# preproc$method$scale = NULL
# db_to_predict = predict(preproc, db_to_predict)

db_to_predict = mice(db_to_predict, m=3, seed = 123)
db = complete(db_to_predict, 1)

db_to_predict_matrix = data.matrix(db)

# Predicting model in original database
probabilities = predict(lasso.model, newx = db_to_predict_matrix, type="response")

db_to_predict = march.db %>% select(-c(cpf_cnpj, data_safra, 
                                       data_contrato, data_primeiro_atraso, 
                                       data_ult_pgt, data_vencimento, status_contrato, 
                                       num_chassi, tabela, assessoria, assessoria_cob, 
                                       cod_hda)) %>% filter(segmento == segment)
db_to_predict = db_to_predict %>% select(-c(starts_with("data_")))

pred = predict(lasso.model, newx = db_to_predict_matrix)
resp = predict(lasso.model, newx = db_to_predict_matrix, type = "response")

# Creating dataframe with probabilities
probs_by_contract <- data.frame(select(db_to_predict, c("cod_contrato", selected_var[!selected_var %in% "target"])), bad_payer_prob = probabilities)#, predicted_target = predicted.classes)

final_db = probs_by_contract %>% mutate(logit = c(pred),
                                        odds = c(exp(pred)),
                                        prob_good = 1 - (odds/(odds + 1)),
                                        prob_bad =  (odds/(odds + 1)),
                                        prob_ctrl = c(resp))

final_db$score = round(final_db$prob_good * 1000,0)

score_metrics = c("logit", "odds", "prob_good", "prob_bad", "prob_ctrl", "score_ctrl", "score")

set.seed(123)
centers <- stats::kmeans(final_db$score, centers = 4, iter.max = 500, nstart = 100)$centers
centers <- sort(centers)
set.seed(123)
clusters <- kmeans(final_db$score, centers = centers, iter.max = 500, nstart = 100)
final_db$cluster = as.factor(clusters$cluster)

final_db$cluster = revalue(final_db$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))

median_cluster_db_to_predict <- final_db %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(tb = median(score, na.rm = TRUE)) 

p2 <- final_db %>% 
  ggplot(aes(score, color = factor(cluster), fill = factor(cluster))) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
  geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
  geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
  geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
  guides(fill = "legend", colour = "none") +
  scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  theme(legend.position="bottom") +
  labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for validation data [observation: 201807 - 201812 / target: 201901]",
       caption = paste0("Based on data from IGB: ", segment, " | ", " | ", "version: ", Sys.Date()))

final_db = final_db %>% select(cod_contrato, prob_good, prob_bad, score, cluster)

setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/MOT")
# save(final_db, file = "final_db_MOT.RData")
load("final_db_MOT.RData")
final_db_mot = final_db

setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/CAR")
# save(final_db, file = "final_db_CAR.RData")
load("final_db_CAR.RData")
final_db_car = final_db

##################
#ANOTHER ANALYSIS#
##################

final_db = bind_rows(final_db_car, final_db_mot)
rm(final_db_car, final_db_mot)

march.db = left_join(march.db, final_db, by = "cod_contrato")

march.db = march.db %>% mutate(cut_atraso = cut(qtd_dias_em_atraso, breaks = c(seq(0, 1000000, by = 300)), include.lowest = TRUE))
march.db = march.db %>% mutate(cut_balance = cut(vlr_vencido, breaks = c(seq(0,100000000, by = 5000)), include.lowest = TRUE))

# march.db.out = march.db %>% filter(qtd_dias_em_atraso > 0 & qtd_dias_em_atraso <= 360)
march.db.loss = march.db %>% filter(qtd_dias_em_atraso > 360 & status_cob == "A - AMG" & situacao_contrato == "PRJ" & 
                                      assessoria_cob %in% c("COBCRED", "LOCALCRED_", "FLEX CONTACT"))

cuts = march.db.out %>% group_by(cut_atraso, cut_balance, cluster, segmento) %>% dplyr::summarise(n = n())
cut.atraso = march.db.out %>% group_by(cut_atraso) %>% dplyr::summarise(n = n())
cut.balance = march.db.out %>% group_by(cut_balance) %>% dplyr::summarise(n = n())

# cuts = spread(cuts, cut_atraso, n)

analysis = march.db.loss %>% group_by(assessoria, segmento) %>% dplyr::summarise(contracts = n(),
                                                           balance = sum(vlr_vencido, na.rm = TRUE),
                                                           dealers = n_distinct(nome_hda),
                                                           median_balance = median(vlr_vencido, na.rm = TRUE),
                                                           median_delay = median(qtd_dias_em_atraso, na.rm = TRUE))

fwrite(analysis, file = "analysis_actual.csv", sep  = ";", dec = ",")

analysis_shuffle = join %>% group_by(assessoria_reshuffle, segmento_reshuffle) %>% dplyr::summarise(contracts = n(),
                                                                                balance = sum(vlr_vencido, na.rm = TRUE),
                                                                                dealers = n_distinct(nome_hda_reshuffle),
                                                                                median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                                median_delay = median(qtd_dias_em_atraso, na.rm = TRUE))

fwrite(analysis_shuffle, file = "analysis_reshuffle.csv", sep  = ";", dec = ",")

# inserting actual distribution of contracts for each collection agency
share_cesec_mot_ini = 0.3659; share_cesec_car_ini = 0.3453
share_paschoa_mot_ini = 0.2880; share_pachoa_car_ini = 0.3408
share_flex_mot_ini = 0.2013; share_flex_car_ini = 0.3026
share_localcred_mot_ini = 0.1448; share_localcred_car_ini = 0.0113

share_cesec_mot = share_cesec_mot_ini
share_cesec_car = share_cesec_car_ini

# recreating the distribution for segment: car or mot
left = 1 - share_cesec_mot_ini
share_paschoa_mot = share_paschoa_mot_ini/left

left = 1 - share_cesec_car_ini
share_paschoa_car = share_pachoa_car_ini/left

left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini) 
share_flex_mot = share_flex_mot_ini/left

left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini) 
share_flex_car = share_flex_car_ini/left

left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini, share_flex_mot_ini) 
share_localcred_mot = share_localcred_mot_ini/left

left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini, share_flex_car_ini) 
share_localcred_car = share_localcred_car_ini/left

# creating contract shuffle by cut_atraso, cut_balance and risk cluster
set.seed(888)
cesec_mot = march.db.out %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
cesec_car = march.db.out %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
cesec = bind_rows(cesec_mot, cesec_car)

paschoa_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
paschoa_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
paschoa = bind_rows(paschoa_mot, paschoa_car)

flex_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
flex_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)
flex = bind_rows(flex_mot, flex_car)

localcred = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato, flex$cod_contrato),] %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, segmento, nome_hda)

cesec$assessoria = "CESEC"
paschoa$assessoria = "PASCHOALOTTO"
flex$assessoria = "FLEX"
localcred$assessoria = "LOCALCRED"

setwd("D:/Users/sb044936/Desktop/Reshuffle")
all_labeled = bind_rows(cesec, paschoa, flex, localcred)
save(all_labeled, file = "all_assessorias_shuffle.RData")

join = left_join(march.db.out, all_labeled, by = "cod_contrato", suffix = c("_actual", "_reshuffle"))
save(join, file = "all_data_assessorias_shuffle.RData")

setwd("D:/Users/sb044936/Desktop/Reshuffle")
load("all_data_assessorias_shuffle.RData")
load("all_assessorias_shuffle.RData")

join = join %>% mutate(cut_atraso_pgto = cut(dias_atrasados, breaks = c(seq(0, 360, by = 30))),
                       cut_atraso = cut(qtd_dias_em_atraso, breaks = c(seq(0, 360, by = 30))))

# fwrite(cuts, file = "cuts.csv", sep  =";", dec = ",")

plot.table.est = all_labeled %>% group_by(assessoria, nome_est_loja, segmento) %>% dplyr::summarise(n = n())
plot.table.days = all_labeled %>% group_by(assessoria, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance = all_labeled %>% group_by(assessoria, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster = all_labeled %>% group_by(assessoria, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda = all_labeled %>% group_by(assessoria, nome_hda, segmento) %>% dplyr::summarise(n = n())

# add balance (owed value), dealer (with and without), score, 

est.plot = ggplot(data=plot.table.est, aes(x=nome_est_loja, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by state",
       fill = "", x = "state", y = "contracts (%)") +
       #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot = ggplot(data=plot.table.days, aes(x=cut_atraso, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
       #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot = ggplot(data=plot.table.balance, aes(x=cut_balance, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts shuffle by balance",
       fill = "", x = "balance", y = "contracts (%)") +
       #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot = ggplot(data=plot.table.hda, aes(x=nome_hda, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by dealer",
       fill = "", x = "dealer", y = "contracts (%)") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot = ggplot(data=plot.table.cluster, aes(x=cluster, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)", 
       caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay <- rbind(c(1,2,3),
              c(4,4,5))

both_plot_shuffle <- grid.arrange(est.plot, delay.plot, balance.plot, hda.plot, cluster.plot, layout_matrix = hlay)
ggsave(paste0("plots_shuffle","_", Sys.Date(),".tiff"), both_plot_shuffle, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")

ggsave(paste0("plots_shuffle_all","_", Sys.Date(),".tiff"), both_plot_shuffle, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_shuffle_est","_", Sys.Date(),".tiff"), est.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_shuffle_delay","_", Sys.Date(),".tiff"), delay.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_shuffle_balance","_", Sys.Date(),".tiff"), balance.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_shuffle_hda","_", Sys.Date(),".tiff"), hda.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_shuffle_cluster","_", Sys.Date(),".tiff"), cluster.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")

sum_balance = sum(march.db.out$vlr_vencido, na.rm = TRUE)

macro.view = join %>% group_by(assessoria_reshuffle) %>% dplyr::summarise(contracts = n(),
                                                                balance = sum(vlr_vencido, na.rm = TRUE),
                                                                median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                median_days = median(qtd_dias_em_atraso, na.rm= TRUE))
save(macro.view, file = "macro_view.RData")
fwrite(macro.view, file = "macro_view_reshuffle.csv", sep = ";", dec = ",")

#check distribution of numeric variables

density.plot.balance = ggplot(join, aes(x=vlr_vencido, color=assessoria_reshuffle, fill=assessoria_reshuffle)) +
  geom_density(alpha = 0.1) + xlim(0,10000)

density.plot.balance = ggplot(join, aes(x=vlr_vencido, color=assessoria_actual, fill=assessoria_actual)) +
  geom_density(alpha = 0.1) + xlim(0,10000)

density.plot.score = ggplot(join, aes(x=score, color=assessoria_reshuffle, fill=assessoria_reshuffle)) +
  geom_density(alpha = 0.1)

density.plot.score = ggplot(join, aes(x=score, color=assessoria_actual, fill=assessoria_actual)) +
  geom_density(alpha = 0.1)

density.plot.delay = ggplot(join, aes(x=qtd_dias_em_atraso, color=assessoria_reshuffle, fill=assessoria_reshuffle)) +
  geom_density(alpha = 0.1)

density.plot.delay = ggplot(join, aes(x=qtd_dias_em_atraso, color=assessoria_actual, fill=assessoria_actual)) +
  geom_density(alpha = 0.1)

ggsave(paste0("plot_density_balance","_", Sys.Date(),".tiff"), density.plot.balance, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plot_density_score","_", Sys.Date(),".tiff"), density.plot.score, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plot_density_delay","_", Sys.Date(),".tiff"), density.plot.delay, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")


###


plot.table.est = join %>% group_by(assessoria_actual, nome_est_loja_actual, segmento_actual) %>% dplyr::summarise(n = n())
plot.table.days = join %>% group_by(assessoria_actual, cut_atraso_actual, segmento_actual) %>% dplyr::summarise(n = n())
plot.table.balance = join %>% group_by(assessoria_actual, cut_balance_actual, segmento_actual) %>% dplyr::summarise(n = n())
plot.table.cluster = join %>% group_by(assessoria_actual, cluster_actual, segmento_actual) %>% dplyr::summarise(n = n())
plot.table.hda = join %>% group_by(assessoria_actual, nome_hda_actual, segmento_actual) %>% dplyr::summarise(n = n())

est.plot = ggplot(data=plot.table.est, aes(x=nome_est_loja_actual, y=n, fill=assessoria_actual)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento_actual) + coord_flip() +
  labs(subtitle =  "Actual accounts by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot = ggplot(data=plot.table.days, aes(x=cut_atraso_actual, y=n, fill=assessoria_actual)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento_actual) + coord_flip() +
  labs(subtitle =  "Actual accounts by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot = ggplot(data=plot.table.balance, aes(x=cut_balance_actual, y=n, fill=assessoria_actual)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento_actual) + coord_flip() +
  labs(subtitle = "Actual accounts by balance",
       fill = "", x = "balance", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot = ggplot(data=plot.table.hda, aes(x=nome_hda_actual, y=n, fill=assessoria_actual)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento_actual) + coord_flip() +
  labs(subtitle =  "Actual accounts by dealer",
       fill = "", x = "dealer", y = "contracts (%)") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot = ggplot(data=plot.table.cluster, aes(x=cluster_actual, y=n, fill=assessoria_actual)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento_actual) + coord_flip() +
  labs(subtitle =  "Actual accounts by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)", 
       caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay <- rbind(c(1,2,3),
              c(4,4,5))

both_plot_actual <- grid.arrange(est.plot, delay.plot, balance.plot, hda.plot, cluster.plot, layout_matrix = hlay)
ggsave(paste0("plots_actual","_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")

ggsave(paste0("plots_actual_all","_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_est","_", Sys.Date(),".tiff"), est.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_delay","_", Sys.Date(),".tiff"), delay.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_balance","_", Sys.Date(),".tiff"), balance.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_hda","_", Sys.Date(),".tiff"), hda.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")
ggsave(paste0("plots_actual_cluster","_", Sys.Date(),".tiff"), cluster.plot, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/")

macro.view.actual = join %>% group_by(assessoria_actual) %>% dplyr::summarise(contracts = n(),
                                                                          balance = sum(vlr_vencido, na.rm = TRUE),
                                                                          median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                          median_days = median(qtd_dias_em_atraso, na.rm= TRUE))

fwrite(macro.view.actual, file = "macro_view_actual.csv", sep = ";", dec = ",")
fwrite(cuts, file = "cuts.csv", sep = ";", dec = ",")


####

random_dealers = march.db.out %>% group_by(nome_hda, cut_balance) %>% sample_frac(0.3)


out = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360) %>% dplyr::group_by(nome_hda, segmento) %>% dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360) %>% dplyr::group_by(nome_hda, segmento) %>% dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

distribution = march.db.loss %>% group_by(assessoria, segmento) %>% summarise(contracts = n(),)

################################
################################
################################

dealers.vision = march.db.loss %>%
  group_by(cod_hda, nome_hda, segmento, nome_est_loja) %>% 
  dplyr::summarise(median_atraso = median(qtd_dias_em_atraso, na.rm = TRUE),
                   median_balance = median(vlr_vencido, na.rm = TRUE),
                   #median_score = median(score, na.rm = TRUE),
                   contracts = n()) %>%
  mutate(cut_median_atraso = cut(median_atraso, breaks = c(seq(0, 100000, by = 5)), include.lowest = TRUE),
         cut_median_balance = cut(median_balance, breaks = c(seq(0, 100000, by = 200)), include.lowest = TRUE),
         #cut_median_score = cut(median_score, breaks = c(seq(0, 100000, by = 50))),
         cut_contracts = cut(contracts, breaks = c(seq(0, 100000, by = 100))))

state.vision = march.db.loss %>% 
  group_by(nome_est_loja, segmento) %>% 
  dplyr::summarise(median_atraso = median(qtd_dias_em_atraso, na.rm = TRUE),
                   median_balance = median(vlr_vencido, na.rm = TRUE),
                   median_score = median(score, na.rm = TRUE),
                   contracts = n()) %>%
  mutate(cut_median_atraso = cut(median_atraso, breaks = c(seq(0, 100000, by = 5))),
         cut_median_balance = cut(median_balance, breaks = c(seq(0, 100000, by = 200))),
         # cut_median_score = cut(median_score, breaks = c(seq(0, 100000, by = 50))),
         cut_contracts = cut(contracts, breaks = c(seq(0, 100000, by = 100))))

out = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del = march.db.out %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(delinquents = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

all = all %>% mutate(index = delinquents/outstanding)

dealers.vision = left_join(dealers.vision, all)
dealers.vision = dealers.vision %>% mutate(cut_index = cut(index, breaks = c(seq(0, 1, by = 0.05)), include.lowest = TRUE))

# # inserting actual distribution of contracts for each collection agency
# share_cesec_mot_ini = 0.3659; share_cesec_car_ini = 0.3453
# share_paschoa_mot_ini = 0.2880; share_pachoa_car_ini = 0.3408
# share_flex_mot_ini = 0.2013; share_flex_car_ini = 0.3026
# share_localcred_mot_ini = 0.1448; share_localcred_car_ini = 0.0113
# 
# share_cesec_mot = share_cesec_mot_ini
# share_cesec_car = share_cesec_car_ini
# 
# # recreating the distribution for segment: car or mot
# left = 1 - share_cesec_mot_ini
# share_paschoa_mot = share_paschoa_mot_ini/left
# 
# left = 1 - share_cesec_car_ini
# share_paschoa_car = share_pachoa_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini) 
# share_flex_mot = share_flex_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini) 
# share_flex_car = share_flex_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini, share_flex_mot_ini) 
# share_localcred_mot = share_localcred_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini, share_flex_car_ini) 
# share_localcred_car = share_localcred_car_ini/left
# 
# # creating contract shuffle by cut_atraso, cut_balance and risk cluster
# set.seed(888)
# cesec_mot = march.db.out %>% filter(segmento == "MOT") %>% group_by(nome_hda) %>% sample_frac(share_cesec_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec_car = march.db.out %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec = bind_rows(cesec_mot, cesec_car)
# 
# paschoa_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# paschoa_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_paschoa_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# paschoa = bind_rows(paschoa_mot, paschoa_car)
# 
# flex_mot = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "MOT") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_mot) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# flex_car = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato),] %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_flex_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# flex = bind_rows(flex_mot, flex_car)
# 
# localcred = march.db.out[!march.db.out$cod_contrato %in% c(cesec$cod_contrato, paschoa$cod_contrato, flex$cod_contrato),] %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# 
# cesec$assessoria = "CESEC"
# paschoa$assessoria = "PASCHOALOTTO"
# flex$assessoria = "FLEX"
# localcred$assessoria = "LOCALCRED"
# 
# all_labeled = bind_rows(cesec, paschoa, flex, localcred)

summar.assess = march.db.loss %>% group_by(assessoria_cob, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                              contracts = n(),
                                                                              states = n_distinct(nome_est_loja)) %>%
  mutate(perc_dealers = dealers/n_distinct(march.db.loss$cod_hda),
         perc_contracts = contracts/nrow(march.db.loss))

summar.est = march.db.loss %>% group_by(nome_est_loja, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                               contracts = n(),
                                                                               states = n_distinct(nome_est_loja)) %>%
  mutate(perc_dealers = dealers/n_distinct(march.db.loss$cod_hda),
         perc_contracts = contracts/nrow(march.db.loss))

# 
# 
# set.seed(20)
# clusters <- kmeans(, 5)
# 
# ##################
# # MANUAL SHUFFLE # 
# ##################
# 
# # inserting actual distribution of contracts for each collection agency
# share_cesec_mot_ini = 0.3659; share_cesec_car_ini = 0.3453
# share_paschoa_mot_ini = 0.2880; share_pachoa_car_ini = 0.3408
# share_flex_mot_ini = 0.2013; share_flex_car_ini = 0.3026
# share_localcred_mot_ini = 0.1448; share_localcred_car_ini = 0.0113
# 
# share_cesec_mot = share_cesec_mot_ini
# share_cesec_car = share_cesec_car_ini
# 
# # recreating the distribution for segment: car or mot
# left = 1 - share_cesec_mot_ini
# share_paschoa_mot = share_paschoa_mot_ini/left
# 
# left = 1 - share_cesec_car_ini
# share_paschoa_car = share_pachoa_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini) 
# share_flex_mot = share_flex_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini) 
# share_flex_car = share_flex_car_ini/left
# 
# left = 1 - sum(share_cesec_mot_ini, share_paschoa_mot_ini, share_flex_mot_ini) 
# share_localcred_mot = share_localcred_mot_ini/left
# 
# left = 1 - sum(share_cesec_car_ini, share_pachoa_car_ini, share_flex_car_ini) 
# share_localcred_car = share_localcred_car_ini/left
# 
# set.seed(888)
# cesec_mot = dealers.vision %>% dplyr::filter(segmento == "MOT") %>% dplyr::group_by(cut_median_atraso, cut_median_balance, cut_median_score, cut_contracts) %>% 
#                                dplyr::sample_n(150) %>% select(nome_hda, nome_est_loja, segmento, 
#                                                                    contracts, median_atraso, median_balance, 
#                                                                    median_score, contracts); sum(cesec_mot$contracts); sum(cesec_mot$contracts)/sum(dealers.vision$contracts)
# 
# cesec_car = march.db.out %>% filter(segmento == "CAR") %>% group_by(cut_atraso, cut_balance, cluster) %>% sample_frac(share_cesec_car) %>% select(cod_contrato, cut_atraso, cut_balance, cluster, segmento, nome_est_loja, nome_hda)
# cesec = bind_rows(cesec_mot, cesec_car)

####################
# inserting actual distribution of contracts for each collection agency
share_cobc_mot = 27244; share_cobc_car = 0
share_loca_mot = 47943; share_loca_car = 142
share_flex_mot = 53267; share_flex_car = 69

assessoria.db = data.frame()
list_files = list()

assessorias = c("COBCRED", "FLEX", "LOCALCRED")
segments = c("MOT", "CAR")

dealers.vision.filt = dealers.vision %>% select(cod_hda, nome_hda, segmento, contracts, cut_median_atraso, cut_median_balance, nome_est_loja)
dealers.vision.filt = dealers.vision.filt %>% mutate(id = paste0(cod_hda,"#",segmento))

# states.vision.filt = state.vision %>% select(nome_est_loja, segmento, contracts, cut_median_atraso, cut_median_balance, cut_median_score, nome_est_loja, median_score)
# dealers.vision.filt = dealers.vision.filt %>% mutate(id = paste0(cod_hda,"#",segmento))

# r = 1
# random_rate = c(0.6, 0.65, 0.70, 0.75, 0.80, 0.85, 0.9, 1)

for(i in 1:length(assessorias)){
  for(j in 1:length(segments)){
    
    if(assessorias[i] == "COBCRED" & segments[j] == "MOT"){share = share_cobc_mot}
    if(assessorias[i] == "COBCRED" & segments[j] == "CAR"){share = share_cobc_car}
    if(assessorias[i] == "FLEX" & segments[j] == "MOT"){share = share_flex_mot}
    if(assessorias[i] == "FLEX" & segments[j] == "CAR"){share = share_flex_car}
    if(assessorias[i] == "LOCALCRED" & segments[j] == "MOT"){share = share_loca_mot}
    if(assessorias[i] == "LOCALCRED" & segments[j] == "CAR"){share = share_loca_car}
    
    assessoria.db <- dealers.vision.filt %>% dplyr::filter(segmento == segments[j])
    
    set.seed(2333)
    assessoria.db <- assessoria.db %>% group_by(cut_median_atraso, cut_median_balance) %>% sample_frac(1)
    set.seed(22222)
    assessoria.db <- assessoria.db[sample(nrow(assessoria.db)),]
    assessoria.db$contracts_cumsum <- cumsum(assessoria.db$contracts)
    assessoria.db <- assessoria.db[assessoria.db$contracts_cumsum <= share,]
    print(paste0("share of ", segments[j], " for ", assessorias[i], " is: ", round(sum(assessoria.db$contracts)/sum(summar.assess$contracts),6)*100, "% (", sum(assessoria.db$contracts),") of total contracts - "))
    
    assessoria.db <- assessoria.db %>% mutate(assessoria = assessorias[i])
    file.name <- paste0(assessorias[i],"_",segments[j])
    list_files[[file.name]] <- assessoria.db
    dealers.vision.filt <<- dealers.vision.filt %>% filter(!id %in% c(assessoria.db$id))
    
    # r <<- r+1
  }}

dealers.final = bind_rows(list_files)
dealers.final = bind_rows(dealers.final, dealers.vision.filt)
# states.final = bind_rows(list_files)
# states.final = bind_rows(states.final, state.vision.filt)

dealers.final$assessoria[dealers.final$cod_hda %in% c(1635915, 1012487)] <- "FLEX"
dealers.final$assessoria[dealers.final$cod_hda %in% c(1646612)] <- "COBCRED"

contracts.final = left_join(march.db.loss, dealers.final, by = c("cod_hda", "nome_hda", "nome_est_loja", "segmento"))
# contracts.final = left_join(march.db.loss, states.final, by = c("nome_est_loja","segmento"))

table(is.na(contracts.final$assessoria.y))

summar.shuffle =  contracts.final %>% group_by(assessoria.y, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                                            contracts = n(),
                                                                                            states = n_distinct(nome_est_loja)) %>%
                                                                                  mutate(perc_dealers = dealers/n_distinct(march.db.loss$nome_hda),
                                                                                         perc_contracts = contracts/nrow(march.db.loss))

names(summar.shuffle)[1] = "assessoria_cob"
summar.assess$assessoria_cob = gsub("_","", summar.assess$assessoria_cob)
summar.assess$assessoria_cob = gsub(" CONTACT", "", summar.assess$assessoria_cob)

summar.final = left_join(summar.shuffle, summar.assess, by = c("assessoria_cob", "segmento"), suffix = c("_before", "_after"))
summar.final = summar.final %>% mutate(dif_perc_contracts = abs(perc_contracts_after - perc_contracts_before),
                                       dif_contracts = contracts_after - contracts_before)

# summar.final = left_join(summar.assess, summar.shuffle, by = c("nome_est_loja", "segmento"), suffix = c("_before", "_after"))
# summar.final = summar.final %>% mutate(dif_perc_contracts = abs(perc_contracts_after - perc_contracts_before),
#                                        dif_contracts = contracts_after - contracts_before)

fwrite(dealers.vision.filt, file = "dealers_vision_remain.csv", dec = ",", sep = ";")
fwrite(summar.final, file = "summar_final_loss.csv", dec = ",", sep = ";")

##############################

plot.table.est.actual = contracts.final %>% group_by(assessoria_cob, nome_est_loja, segmento, ) %>% dplyr::summarise(n = n())
plot.table.days.actual = contracts.final %>% group_by(assessoria_cob, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance.actual = contracts.final %>% group_by(assessoria_cob, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster.actual = contracts.final %>% group_by(assessoria_cob, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda.actual = contracts.final %>% group_by(assessoria_cob, nome_hda, segmento) %>% dplyr::summarise(n = n())
# plot.table.index.actual = contracts.final %>% group_by(assessoria.x, cut_index, segmento) %>% dplyr::summarise(n = n())

plot.table.est.shuffle = contracts.final %>% group_by(assessoria.y, nome_est_loja, segmento) %>% dplyr::summarise(n = n())
plot.table.days.shuffle = contracts.final %>% group_by(assessoria.y, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance.shuffle = contracts.final %>% group_by(assessoria.y, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster.shuffle = contracts.final %>% group_by(assessoria.y, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda.shuffle = contracts.final %>% group_by(assessoria.y, nome_hda, segmento) %>% dplyr::summarise(n = n())
# plot.table.index.shuffle = contracts.final %>% group_by(assessoria.y, cut_index, segmento) %>% dplyr::summarise(n = n())

# add balance (owed value), dealer (with and without), score, 

est.plot.actual = ggplot(data=plot.table.est.actual, aes(x=nome_est_loja, y=n, fill=assessoria_cob)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot.actual = ggplot(data=plot.table.days.actual, aes(x=cut_atraso, y=n, fill=assessoria_cob)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot.actual = ggplot(data=plot.table.balance.actual, aes(x=cut_balance, y=n, fill=assessoria_cob)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts actual by balance",
       fill = "", x = "balance", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot.actual = ggplot(data=plot.table.hda.actual, aes(x=nome_hda, y=n, fill=assessoria_cob)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by dealer",
       fill = "", x = "dealer", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot.actual = ggplot(data=plot.table.cluster.actual, aes(x=cluster, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

index.plot.actual = ggplot(data=plot.table.index.actual, aes(x=cut_index, y=n, fill=assessoria.x)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts actual by index",
       fill = "", x = "delinquency index", y = "contracts (%)") + 
  #       caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay <- rbind(c(1,2,3),
              c(4,5,6))

both_plot_actual <- grid.arrange(est.plot, delay.plot, balance.plot, hda.plot, cluster.plot, index.plot, layout_matrix = hlay)

##################
#CONTRACTS VISION#
##################

est.plot.shuffle = ggplot(data=plot.table.est.shuffle, aes(x=nome_est_loja, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot.shuffle = ggplot(data=plot.table.days.shuffle, aes(x=cut_atraso, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot.shuffle = ggplot(data=plot.table.balance.shuffle, aes(x=cut_balance, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts shuffle by balance",
       fill = "", x = "balance", y = "contracts (%)",
       caption = "Source: IGB | march, 2019 | active contracts [> 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot.shuffle = ggplot(data=plot.table.hda.shuffle, aes(x=nome_hda, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by dealer",
       fill = "", x = "dealer", y = "contracts (%)",
       caption = "source: IGB | march, 2019 | active contracts [> 360]") +
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot.shuffle = ggplot(data=plot.table.cluster.shuffle, aes(x=cluster, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)") + 
  #      caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

index.plot.shuffle = ggplot(data=plot.table.index.shuffle, aes(x=cut_index, y=n, fill=assessoria.y)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts shuffle by index",
       fill = "", x = "delinquency index", y = "contracts (%)", 
       caption = "source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay1 <- rbind(c(1,2,3),
               c(4,5,6))

hlay2 <- rbind(c(1,1),
               c(2,2))

both_plot_shuffle <- grid.arrange(est.plot.shuffle, delay.plot.shuffle, balance.plot.shuffle, cluster.plot.shuffle, index.plot.shuffle, layout_matrix = hlay1)
both_plot_1 = grid.arrange(est.plot.actual, delay.plot.actual, balance.plot.actual, est.plot.shuffle, delay.plot.shuffle, balance.plot.shuffle, layout_matrix = hlay1)
both_plot_2 = grid.arrange(hda.plot.actual, hda.plot.shuffle, layout_matrix = hlay2)

ggsave(paste0("plots_actual_vs_shuffle_1_loss","_", Sys.Date(),".tiff"), both_plot_1, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/LOSS")
ggsave(paste0("plots_actual_vs_shuffle_2_loss","_", Sys.Date(),".tiff"), both_plot_2, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Reshuffle/LOSS")

analysis_shuffle = contracts.final %>% group_by(assessoria.y, segmento) %>% dplyr::summarise(contracts = n(),
                                                                                             balance = sum(vlr_vencido, na.rm = TRUE),
                                                                                             dealers = n_distinct(nome_hda),
                                                                                             median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                                             median_delay = median(qtd_dias_em_atraso, na.rm = TRUE))
                                                                                             #median_score = median(score, na.rm = TRUE),
                                                                                             #outstanding = sum(outstanding, na.rm = TRUE),
                                                                                             #delinquents = sum(delinquents, na.rm = TRUE)) %>% mutate(index = outstanding/delinquents)

analysis_actual = contracts.final %>% group_by(assessoria_cob, segmento) %>% dplyr::summarise(contracts = n(),
                                                                                            balance = sum(vlr_vencido, na.rm = TRUE),
                                                                                            dealers = n_distinct(nome_hda),
                                                                                            median_balance = median(vlr_vencido, na.rm = TRUE),
                                                                                            median_delay = median(qtd_dias_em_atraso, na.rm = TRUE))
                                                                                            # median_score = median(score, na.rm = TRUE),
                                                                                            # outstanding = sum(outstanding, na.rm = TRUE),
                                                                                            # delinquents = sum(delinquents, na.rm = TRUE)) %>% mutate(index = outstanding/delinquents)

dealers_shuffle = contracts.final %>% select(cod_hda, nome_hda, nome_est_loja, segmento, assessoria.x, assessoria.y) %>% distinct()
dealers_shuffle = dealers_shuffle %>% mutate(ass_actual_equal_shuffle = ifelse(assessoria.x == assessoria.y, TRUE, FALSE))
prop.table(table(dealers_shuffle$ass_actual_equal_shuffle))

contracts.final = left_join(contracts.final, all)

fwrite(analysis_actual, file = "analysis_actual_loss.csv", sep = ";", dec = ",")
fwrite(analysis_shuffle, file = "analysis_shuffle_loss.csv", sep = ";", dec= ",")
save(contracts.final, file = "contracts_final_loss.RData")

load("contracts_final.RData")
analysis_shuffle = fread("analysis_shuffle_loss.csv", sep = ",")
analysis_actual = fread("analysis_actual_loss.csv", sep = ",")

save(est.plot.shuffle, file = "est_plot_shuffle_loss.RData") 
save(delay.plot.shuffle, file = "delay_plot_shuffle_loss.RData")
save(balance.plot.shuffle, file = "balance_plot_shuffle_loss.RData") 
# save(cluster.plot.shuffle, file = "cluster_plot_shuffle_loss.RData") 
# save(index.plot.shuffle, file = "index_plot_shuffle_loss.RData")

save(est.plot.actual, file = "est_plot_actual_loss.RData") 
save(delay.plot.actual, file = "delay_plot_actual_loss.RData")
save(balance.plot.actual, file = "balance_plo_actual_loss.RData") 
# save(cluster.plot.actual, file = "cluster_plot_actual_loss.RData") 
# save(index.plot.actual, file = "index_plot_actual_loss.RData")