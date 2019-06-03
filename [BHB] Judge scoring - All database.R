setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge")
jurid = fread("listagem_juridico_012019.txt", header = TRUE, dec = ",", sep = "\t", colClasses = c("CONTRATO" = "character"))
jurid = jurid %>% dplyr::rename("cod_contrato" = "CONTRATO")

contr = jurid %>% select(cod_contrato)
contr = c(contr[[1]])

z = bhb.final %>% filter(cod_contrato %in% contr)
target_creation_jurid(z)

# COLLECTION SCORING FOR JUDGE RANKING
for(j in 1:length(segment)){
  
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Models"))
  load(paste0("lasso_model_61_360", "_", segment[j], "_FINAL.RData"))
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Databases"))
  load(paste0("bins_mod_61_360", "_", segment[j], ".RData"))
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Predictions"))
  load(paste0("selected_var_61_360", "_", segment[j],"_FINAL.RData"))
  
  paste0("Starting judge daily modelling process for 61_360 | ", segment[j], ".\n") %>% cat()
  
  modelling_judge(mod_61_360, "61_360", segment[j])
  
  paste0("Judge daily modelling process for 61_360 | ", segment[j], " done.\n") %>% cat()
  "######################################################\n\n" %>% cat()
}

target_creation_jurid = function(database){
  
  require(dplyr)
  require(data.table)
  
  mod_61_360 <- database %>% select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
                                  -cep_loja,-vlr_tx_banco,-vlr_taxa_cliente,-cod_tabela,-nome_placa,-analista_c,
                                  -data_contrato, -cod_hda, -vlr_vrg_antecipado, -vlr_vrg_diluido, -vlr_saldo_inicial,
                                  -vlr_liberado, -cod_inst_financ, -cod_marca, -data_risco_contabil, -vlr_seguri_casco,
                                  -vlr_tac, -data_ult_pgt, -data_vencimento, -data_nascimento_cli, -data_baixa, -cod_banco,
                                  -data_ult_alt, -proposta, -cod_plano, -vlr_subs_conc, -vlr_subs_marca, -vlr_taxa_subs_conc, -vlr_desp_finan,
                                  -car, -cod_pessoa, -data_ult_vencimento, -vlr_tx_subs_marc, -re, -data_ini_seguro, -data_fim_seguro,
                                  -data_prim_vencimento, -nome_renavam, -`for`, -contrato_cedido, -numero_contrato_cessao,
                                  -coobrigacao_sem_n, -qtd_pg_atr_em_1_ano, -valor_pg_atr_em_1_ano, -qtd_pg_atr_1_10_em_1_ano,  
                                  -vlr_pg_atr_1_10_em_1_ano, -qtd_pg_atr_11_30_em_1_ano, -vlr_pg_atr_11_30_em_1_ano, -qtd_pg_atr_1_30_em_1_ano,    
                                  -vlr_pg_atr_1_30_em_1_ano, -qtd_pg_atr_1_60_em_1_ano, -vlr_pg_atr_1_60_em_1_ano, -qtd_pg_atr_31_60_em_1_ano,   
                                  -vlr_pg_atr_31_60_em_1_ano, -qtd_pg_atr_61_360_em_1_ano, -vlr_pg_atr_61_360_em_1_ano, -qtd_pg_atr_360_mais_em_1_ano,
                                  -vlr_pg_atr_360_mais_em_1_ano, -qtd_pg_atr_11_60_em_1_ano, -vlr_pg_atr_11_60_em_1_ano, -qtd_pg_atr_1_360_em_1_ano, -vlr_pg_atr_1_360_em_1_ano,
                                  -perc_pg_atr_1_60, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score,
                                  -pnad_versao, -pnad_ano, -descricao_uf)
  
  mod_61_360$perc_pg_atr_11_60 = replace(mod_61_360$perc_pg_atr_11_60, is.na(mod_61_360$perc_pg_atr_11_60), 0)
  mod_61_360$perc_pg_atr_61_360 = replace(mod_61_360$perc_pg_atr_61_360, is.na(mod_61_360$perc_pg_atr_61_360), 0)
  
  ###################################
  #PROCESS TO CREATE TARGET VARIABLE#
  ###################################

  output.df.61.360 <- data.frame() 
  counter = 0
  loop_61_360 = function(base, segment){
    for(j in seq(0.05,0.95,0.05)){
      db = subset(base, base$segmento == segment) %>%
        mutate(target =  ifelse(tempo_contrato_meses <= 6 |
                                  perc_pg_atr_61_360 <= j,
                                1, 0))
      perc = table(db$target)[2]/(table(db$target)[1]+table(db$target)[2])
      counter = counter + 1
      output.df.61.360[counter, "loop"] <- counter
      output.df.61.360[counter, "perc"] <- j
      output.df.61.360[counter, "bad_dist"] <- perc
      output.df.61.360[counter, "segment"] <- segment
    }
    return(output.df.61.360)}
  
  output_61_360_car = loop_61_360(mod_61_360, "CAR")
  output_61_360_mot = loop_61_360(mod_61_360, "MOT")
  
  ##############
  
  filter_out = output_61_360_car %>% filter(perc >= 0.2 & perc <= 0.8)
  cutoff = filter_out$perc[which.min(abs(0.5-filter_out$bad_dist))]

  mod_61_360.car = mod_61_360 %>% filter(segmento == "CAR") %>%
    mutate(target = ifelse(tempo_contrato_meses <= 6 |
                             perc_pg_atr_61_360 <= cutoff, 1, 0)) 
  
  filter_out = output_61_360_mot %>% filter(perc >= 0.2 & perc <= 0.8)
  cutoff = filter_out$perc[which.min(abs(0.5-filter_out$bad_dist))]
  
  mod_61_360.mot = mod_61_360 %>% filter(segmento == "MOT") %>%
    mutate(target = ifelse(tempo_contrato_meses <= 6 |
                             perc_pg_atr_61_360 <= cutoff, 1, 0)) 
  
  mod_61_360 <<- bind_rows(mod_61_360.car, mod_61_360.mot)
  
}

modelling_judge = function(base_desired_model, desired_model, segment){
  
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
  
  db_to_predict = base_desired_model %>% filter(segmento == segment)
  
  #WOE
  # db_woe_bin = woebin(select(db_to_predict, -cod_contrato), y = "target")
  db_woe = woebin_ply(db_to_predict, bins_mod)
  
  registerDoSEQ()
  db_to_predict = db_woe %>% select(lasso.model[["beta"]]@Dimnames[[1]])
  
  preproc = preProcess(db_to_predict, method = "knnImpute", k = 5)
  db_to_predict <- predict(preproc, db_to_predict)
  
  db_to_predict = db_to_predict %>% mutate_all(.,as.numeric)
  
  db_to_predict_matrix = data.matrix(db_to_predict)
  
  "Starting step 4: Predictions and model performance measures.\n\n" %>% cat()
  # Predicting model in test database
  # probabilities <- lasso.model %>% predict(db_to_predict_matrix, type="response")
  probabilities = predict(lasso.model, newx = db_to_predict_matrix, type="response")
  
  db_to_predict = base_desired_model %>% filter(segmento == segment)
  # Checking probabilities x target in test database
  pred <- prediction(probabilities, db_to_predict$target)
  acc.perf = performance(pred, "sens", "spec")
  
  # Find probabilities cutoff with greater sum of specificity and sensitivity
  cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]
  
  predicted.classes <- c(ifelse(probabilities > cutoff, 1, 0))
  observed.classes <- db_woe$target
  
  # Calculating KS, auc, gini
  evaluation <- scorecard::perf_eva(label = observed.classes, pred = probabilities, show_plot = TRUE)
  df <- melt(data.frame(evaluation$binomial_metric$dat))
  names(df) <- c("statistic", "value")
  
  selected_var = coefs <- tidy(lasso.model)
  coefs <- coefs[-1,]
  selected_var_woe = paste(coefs$term, sep = "\n"); selected_var_woe <<- c(selected_var_woe, "target")
  selected_var = str_sub(coefs$term, end = -5); selected_var <<- c("cod_contrato", selected_var, "target")
  
  # Creating dataframe with probabilities
  db_to_predict = base_desired_model %>% filter(segmento == segment)
  
  paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()
  
  conf.matrix = caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes))
  
  "######################################################\n\n" %>% cat()
  print(evaluation)
  "######################################################\n\n" %>% cat()
  print(conf.matrix)
  "######################################################\n\n" %>% cat()
  
  db_to_predict$target = as.factor(db_to_predict$target)
  
  points0 = 600
  odds0 = 50
  pdo = 20
  
  pred = predict(lasso.model, newx = db_to_predict_matrix)
  resp = predict(lasso.model, newx = db_to_predict_matrix, type = "response")
  
  factor = pdo/log(2)
  offset = points0 - factor * log(odds0)
  
  final_db = db_to_predict %>% mutate(logit = c(pred),
                                      odds = c(exp(pred)),
                                      prob_bad = c(odds/(odds + 1)),
                                      prob_good =  c(1 - (odds/(odds + 1))),
                                      prob_ctrl = c(resp))
  
  final_db$score_ctrl = offset - (factor*final_db$logit)
  final_db$score = round(final_db$prob_good * 1000,0)
  
  score_metrics = c("logit", "odds", "prob_good", "prob_bad", "prob_ctrl", "score_ctrl", "score")
  # #########################
  # 
  # final_db %>% 
  #   group_by(target) %>% 
  #   summarise(tb = mean(score)) %>% 
  #   ungroup() -> mean_score_db_to_predict
  # 
  # final_db$target = as.factor(final_db$target)
  # final_db$target <- revalue(final_db$target, c("0"="good payer", "1"="bad payer"))
  # 
  # mean_score_db_to_predict <- final_db %>%
  #   dplyr::group_by(target) %>%
  #   dplyr::summarise(tb = mean(score))
  # 
  # p1 <- final_db %>% 
  #   ggplot(aes(score, color = factor(target), fill = factor(target))) + 
  #   geom_density(alpha = 0.3) + 
  #   geom_vline(aes(xintercept = mean_score_db_to_predict$tb[1]), linetype = "dashed", color = "green4") +
  #   geom_vline(aes(xintercept = mean_score_db_to_predict$tb[2]), linetype = "dashed", color = "brown2") +
  #   scale_color_manual(values=c("green4", "brown2")) +
  #   scale_fill_manual(values=c("green4", "brown2")) +
  #   guides(fill = "legend", colour = "none") +
  #   theme(legend.position="bottom") +
  #   labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 1: Scorecard Distribution by two original collection targets for today's data", 
  #        subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")
  # 
  set.seed(123)
  centers <- stats::kmeans(final_db$score, centers = 4, iter.max = 500, nstart = 100)$centers
  centers <- sort(centers)
  set.seed(123)
  clusters <- kmeans(final_db$score, centers = centers, iter.max = 500, nstart = 100)
  final_db$cluster = as.factor(clusters$cluster)
  
  final_db$cluster = revalue(final_db$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  # 
  # mean_cluster_db_to_predict <- final_db %>% 
  #   dplyr::group_by(cluster) %>% 
  #   dplyr::summarise(tb = mean(score)) 
  # 
  # p2 <- final_db %>% 
  #   ggplot(aes(score, color = factor(cluster), fill = factor(cluster))) + 
  #   geom_density(alpha = 0.3) + 
  #   geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
  #   geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
  #   geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
  #   geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
  #   guides(fill = "legend", colour = "none") +
  #   scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  #   scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  #   theme(legend.position="bottom") +
  #   labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for today's data",
  #        caption = paste0("Based on data from IGB: ", segment, " | ", desired_model, " | ", "daily result: ", "2019-02-06"))
  # 
  # both_plot <- grid.arrange(p1, p2, ncol = 1)
  # 
  # db_to_predict = final_db %>% select(c(cod_contrato, selected_var, cluster, score))
  # db_to_predict$cluster = as.numeric(db_to_predict$cluster)
  # geom.mean = db_to_predict %>% mutate_if(is.numeric, funs(`+1` = .+1))
  # x = geom.mean %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
  # x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`score_+1_-1`)
  # 
  # names <- as.data.frame(names(x))
  # names <- str_sub(names$`names(x)`, end = -7)
  # names(x) <- names
  # 
  # x$cluster = as.factor(x$cluster)
  # x$cluster = revalue(x$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  # 
  # df2 <- melt(x,  id.vars = c("score","cluster"), variable.name = "variable")
  # cluster_prob <- ggplot(df2, aes(`score`, value)) + geom_line() + 
  #   facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
  #   geom_point(df2, mapping = aes(`score`, value, color=factor(cluster), size = 5)) +
  #   scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  #   theme(legend.position="bottom") +
  #   labs(title=paste0("Collection model clusters | ", desired_model, " | ", segment), x="collection score", y="geometric mean of selected variable", col = "cluster",
  #        caption = paste0("Based on data from IGB: ", segment, " | ", desired_model, " | ", "daily result: ", "2019-02-06")) +
  #   guides(size = "none", colour = "legend")
  # 
  # #####################################
  # 
  # # plotting both plots together
  # ggsave(paste0("score_dist_desired_model_daily", "_", segment,"_", "2019-02-06",".tiff"), both_plot, units="in", width=12, height=8, 
  #        path = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Daily Predictions - Judge/"))
  # 
  # filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Daily Predictions - Judge/clusters_distribution_daily_",desired_model,"_",segment,"_","2019-02-06",".tiff")
  # tiff(filename, units="in", width=12, height=8, res=500)
  # print(cluster_prob)
  # dev.off()
  # 
  conf_db = as.data.frame(as.matrix(conf.matrix, what = "overall")); conf_db = rownames_to_column(conf_db, "statistic")
  conf_db2 = as.data.frame(as.matrix(conf.matrix, what = "classes")); conf_db2 = rownames_to_column(conf_db2, "statistic")
  conf = bind_rows(conf_db, conf_db2); colnames(conf) <- c("statistic", "value"); conf$value = round(conf$value, 4)
  perf_stats1 = bind_rows(conf, df); perf_stats1 = perf_stats1 %>% mutate(stat_date = "2019-02-06", segment = segment, model = "61_360")
  perf_stats2 = as.data.frame(conf.matrix$table); perf_stats2 = perf_stats2 %>% dplyr::rename("prediction" = "Prediction",
                                                                                              "reference" = "Reference",
                                                                                              "freq" = "Freq"); perf_stats2 = perf_stats2 %>% mutate(stat_date = "2019-02-06", segment = segment, model = "61_360")
  
  #creating final database (new contracts)
  db_to_predict = final_db %>% select(cod_contrato, segmento, qtd_dias_em_atraso, vlr_vencido, nome_estado_cli, 
                                      nome_municipio_cli, vlr_renda_mensal_cli, prob_bad, prob_good, score, cluster) %>% 
                                      mutate(stat_model = paste0("61_360", " | ", segment),
                                             stat_model_update = "2019-02-06")
  
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", "61_360", "/Daily Predictions - Judge"))
  fwrite(perf_stats1, file = paste0("performance_stats_daily_1_", "61_360", "_", segment,"_", "2019-02-06",".csv"), sep = ";", dec = ",")
  fwrite(perf_stats2, file = paste0("performance_stats_daily_2_", "61_360", "_", segment,"_", "2019-02-06",".csv"), sep = ";", dec = ",")
  fwrite(db_to_predict, file = paste0("score_by_contract_daily_", "61_360", "_", segment,"_", "2019-02-06",".csv"), sep = ";", dec = ",")
}

y = list()

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Daily Predictions - Judge"))
files = list.files(pattern="score_by_contract.*csv")
y = bind_rows(lapply(files, fread, colClasses = c(cod_contrato = "character", stat_model_update = "as.Date"), dec = ",", header = TRUE))

all_judge_contracts_scored = bind_rows(y) %>% arrange(desc(stat_model_update))
