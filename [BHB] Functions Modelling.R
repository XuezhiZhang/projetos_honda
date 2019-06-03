preparing <- function(mod_segment, desired_model, segment){
  
  require(dplyr)
  require(scorecard)
  require(ggplot2)
  require(stringr)
  require(caret)
  require(foreach)

  db = mod_segment %>% filter(segmento == segment) %>% select(-cod_contrato)
  
  perc = table(db$target)[2]/(table(db$target)[1]+table(db$target)[2])
  paste0("The percentage of bad payers is: ", round(perc, 4), ". ") %>% cat()
  
  registerDoSEQ()
  # removing columns with variance = 0 (no discrimination values)
  zerovar = nearZeroVar(db, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db = db[,-zerovar]
  
  # missing data imputation with 5 nearest neighbourhoods
  preproc = preProcess(db, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  db <- predict(preproc, db)

  db$target = as.factor(db$target)
  
  set.seed(123)
  index <- caret::createDataPartition(db$target, p = 0.7, list = FALSE)
  train_db <- db[index, ]
  test_db  <- db[-index, ]
  
  # resampling method: ROSE
  
  # train_db = train_db %>% mutate_if(is.character, factor)
  # db_rose <- ROSE(target ~ ., data = train_db, seed = 1, p = 0.5, hmult.majo = 0.25, hmult.mino = 0.5)$data #in case of concerning about extremely distant neighbourhoods bias important data (or most common data), set hmult.majo = 0.25 and hmult.mino = 0.5 (it will shrunk the space of creation for artificial data)
  # if(perc > 0.7){train_db_selected = db_rose} else {train_db_selected = train_db}

  perc = table(train_db$target)[2]/(table(train_db$target)[1]+table(train_db$target)[2])
  # paste0("After balancing data with ROSE sampling method, the percentage of bad payer is: ", round(perc,4), ".\n\n") %>% cat()
  
  if(desired_model == "11_60"){
  breaks_adj = list(perc_pg_atr_11_60 = c(0.25,0.5,0.75))} else
  {breaks_adj = list(perc_pg_atr_61_360 =c(0.25,0.5,0.75))}
  
  bins_mod <<- scorecard::woebin(train_db, y = "target", breaks_list = breaks_adj)

  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model ,"/Databases"))
  save(bins_mod, file = paste0("bins_mod_", desired_model,"_", segment, ".RData"))
  
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model,"/Plots"))
  plotlist = scorecard::woebin_plot(bins_mod)
  
  # save binning plot
  for (i in 1:length(plotlist)){
    ggplot2::ggsave(
      paste0(segment, "_", desired_model, "_", 
             names(plotlist[i]),"_",Sys.Date(),".png"), plotlist[[i]],
      width = 15, height = 9, units="cm")
  }
  
  train_db_woe = as.data.frame(woebin_ply(train_db, bins_mod))
  test_db_woe = as.data.frame(woebin_ply(test_db, bins_mod))
  
  train_db_woe$target = as.factor(train_db_woe$target)
  test_db_woe$target = as.factor(test_db_woe$target)
  
  registerDoSEQ()
  
  zerovar = nearZeroVar(train_db_woe, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  train_db_woe = train_db_woe[,-zerovar]
  test_db_woe = test_db_woe[,-zerovar]

  # imputing missing data again because in test_woe there are some classes 
  # of the variables that did not existed when producing bins from train dataset
  preproc = preProcess(test_db_woe, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  test_db_woe <- predict(preproc, test_db_woe)
  
  dbs <<- list("train_db_woe" = train_db_woe, 
               "test_db_woe" = test_db_woe, 
               "train_db" = train_db, 
               "test_db" = test_db)
  
 }

clustering <- function(probs_by_contract, desired_model, segment){

  require(reshape2)
  require(ggplot2)
  require(dplyr)
  require(stringr)
  
  # Clustering probabilities
  centers <- kmeans(probs_by_contract$bad_payer_prob, centers = 4, iter.max = 500, nstart = 100)$centers
  centers <- sort(centers)
  clusters <- kmeans(probs_by_contract$bad_payer_prob, centers = centers, iter.max = 100, nstart = 100)
  probs_by_contract$cluster = clusters$cluster
    
  # Development of cluster plots by selected variables
    
  ####################################
  #numeric variables (geometric mean)#
  ####################################
  
  geom.mean = probs_by_contract %>% mutate_if(is.numeric, funs(`+1` = .+1))
  x = geom.mean %>% select_if(is.numeric) %>% group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
  x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`bad_payer_prob_+1_-1`)
  
  names <- as.data.frame(names(x))
  names <- str_sub(names$`names(x)`, end = -7)
  names(x) <- names
  
  x$bad_payer_prob = round(x$bad_payer_prob, 4)
  x$cluster = as.factor(x$cluster)
  
  df <- melt(x,  id.vars = c("bad_payer_prob","cluster"), variable.name = "variable")
  cluster_prob <- ggplot(df, aes(`bad_payer_prob`, value)) + geom_line() + 
    facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
    geom_point(df, mapping = aes(`bad_payer_prob`, value, color=factor(cluster), size = 5)) +
    scale_color_manual(values=c("green4", "yellow2", "darkorange1", "brown2")) +
    theme(legend.position="bottom") +
    labs(title=paste0("Collection model clusters | ", desired_model, " | ", segment), x="Bad payer probability", y="Geometric mean of selected variable", col = "cluster") +
    guides(size = "none", colour = "legend")
  
  ###############################
  #character variables (boxplot)#
  ###############################
  
  var_character = probs_by_contract %>% mutate_if(is.character, as.factor)
  var_character = var_character %>% select_if(is.factor) 
  var_character = var_character %>% select_if(~ nlevels(.) < 200) #retaining just variables with less than 28 levels
  
  nm <- names(var_character)
  
  plot_list = list()
  for(i in seq_along(nm)){
    p <- ggplot(probs_by_contract, aes_string(x=paste0("reorder(", nm[i],", bad_payer_prob, FUN = median)"), y="bad_payer_prob")) +
         geom_boxplot() + coord_flip() + labs(x = nm[i], y = "bad_payer_prob")
    plot_list[[i]] = p
  }
  
  ##############
  #saving plots#
  ##############
  
  # Save char cluster plots to tiff. Makes a separate file for each plot.
  for (i in seq_along(nm)) {
    file_name = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_char_",desired_model,"_",segment,"_", Sys.Date(),"_", nm[i], ".tiff")
    tiff(file_name, units="in", width=12, height=8, res=500)
    print(plot_list[[i]])
    dev.off()
  }

  # Save numeric cluster plot to tiff. Unique plot (comparing all clusters).
  filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_numeric_",desired_model,"_",segment,"_",Sys.Date(),".tiff")
  tiff(filename, units="in", width=12, height=8, res=500)
  print(cluster_prob)
  dev.off()
}

writing <- function(lasso.model, selected_var, probs_by_contract, imp, desired_model, segment, perf_stats1, perf_stats2){

require(data.table)
require(stringr)
  
  # Automatically creating files for desired outputs
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model, "/Models"))
  save(lasso.model, file = paste0("lasso_model_", desired_model,"_",segment,"_", Sys.Date(),".RData"))
    
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model, "/Predictions"))
  save(selected_var, file = paste0("selected_var_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
  fwrite(probs_by_contract, file = paste0("lasso_model_predictions_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  save(probs_by_contract, file = paste0("probs_by_contract_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
  fwrite(imp, file = paste0("var_importance_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats1, file = paste0("performance_stats_1_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats2, file = paste0("performance_stats_2_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
}

writing <- function(lasso.model, selected_var, probs_by_contract, bins_mod, imp, segment, perf_stats1, perf_stats2, x){
  
  require(data.table)
  require(stringr)
  
  # Automatically creating files for desired outputs
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/", segment))
  save(lasso.model, file = paste0("lasso_model_", segment,"_", Sys.Date(),".RData"))
  save(bins_mod, file = paste0("bins_", segment,"_", Sys.Date(),".RData"))
  save(selected_var, file = paste0("selected_var_", segment,"_", Sys.Date(),".RData"))
  save(probs_by_contract, file = paste0("probs_by_contract_", segment,"_", Sys.Date(),".RData"))
  fwrite(probs_by_contract, file = paste0("lasso_model_predictions_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(imp, file = paste0("var_importance_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats1, file = paste0("performance_stats_1_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats2, file = paste0("performance_stats_2_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(x, file = paste0("bins_splits_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")}



#score = standard scorecard
# develop_scorecard <- function(selected_var, desired_model, segment){
#   
#   require(dplyr)
#   require(ggplot2)
#   require(scorecard)
#   require(gridExtra)
#   require(reshape2)
#   require(stringr)
#   require(psych)
#   require(tidyverse)
#   require(rlang)
#   
#   db_to_predict = dbs$test_db_woe
#   
#   #db_to_predict$target = ifelse(db_to_predict$target == 1, "bad payer", "good payer")
#   db_to_predict$target = as.factor(db_to_predict$target)
#   db_to_predict = model.matrix(target ~ ., db_to_predict)[,-1]
#   
#   points0 = 600
#   odds0 = 50
#   pdo = 20
#   
#   pred = glmnet::predict.glmnet(lasso.model, newx = db_to_predict)
#   resp = glmnet::predict.glmnet(lasso.model, newx = db_to_predict, type = "response")
#   
#   db_to_predict = dbs$test_db_woe
#   
#   factor = pdo/log(2)
#   offset = points0 - factor * log(odds0)
#   
#   final_db = db_to_predict %>% mutate(logit = c(pred),
#                                       odds = c(exp(pred)),
#                                       prob = c(odds/(odds + 1)),
#                                       prob_ctrl = c(resp))  
# 
#   final_db$score_ctrl = offset - (factor*final_db$logit)
#   final_db$score = round(final_db$score_ctrl,0)
#   
#   score_metrics = c("logit", "odds", "prob", "prob_ctrl", "score_ctrl", "score")
#   
#   ##############################
#   # 
#   # card <<- scorecard(bins_mod, model.glm, 
#   #                    points0 = points0, 
#   #                    odds0 = 1/odds0, 
#   #                    pdo = pdo)
#   
#   # sc = scorecard_ply(db_to_predict, card, only_total_score = TRUE)
# 
#   final_scorecard <<- bind_rows(bins_mod) %>% filter(variable %in% selected_var) %>% dplyr::select(-count, -count_distr, -good, -bad, -is_special_values, -breaks)
#   
#   db_to_predict = dbs$test_db
#   db_to_predict <- data.frame(dplyr::select(db_to_predict, c(selected_var, target)), dplyr::select(final_db, score_metrics))
# 
#   set.seed(123)
#   centers <- kmeans(db_to_predict$score, centers = 4, iter.max = 500, nstart = 100)$centers
#   centers <- sort(centers)
#   set.seed(123)
#   clusters <- kmeans(db_to_predict$score, centers = centers, iter.max = 500, nstart = 100)
#   db_to_predict$cluster = as.factor(clusters$cluster)
#   db_to_predict <<- db_to_predict
# 
#   mean_score_db_to_predict <- db_to_predict %>%
#     dplyr::group_by(target) %>%
#     dplyr::summarise(tb = mean(score))
#   
#   p1 <- ggplot(db_to_predict, aes_string(x = "score", color = "target", fill = "target"), environment = environment()) + 
#         geom_density(alpha = 0.3) + 
#     geom_vline(aes(xintercept = mean_score_db_to_predict$tb[1]), linetype = "dashed", color = "green4") +
#     geom_vline(aes(xintercept = mean_score_db_to_predict$tb[2]), linetype = "dashed", color = "brown2") +
#     guides(fill = "legend", colour = "none") +
#     theme(legend.position="bottom") +
#     scale_color_manual(values=c("green4", "brown2")) +
#     scale_fill_manual(values=c("green4", "brown2")) +
#     labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 1: Scorecard Distribution by two original collection targets for test data", 
#          subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")
#   
#   mean_cluster_db_to_predict <- db_to_predict %>%
#     dplyr::group_by(cluster) %>%
#     dplyr::summarise(tb = mean(score))
#   
#   p2 <- ggplot(db_to_predict, aes_string(x = "score", color = "cluster", fill = "cluster"), environment = environment()) +
#     geom_density(alpha = 0.3) + 
#     geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
#     geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
#     geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
#     geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
#     guides(fill = "legend", colour = "none") +
#     scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
#     scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
#     theme(legend.position="bottom") +
#     labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for test data",
#          caption = paste0("Based on data from IGB: ", segment, " | ", desired_model))
#   
#   both_plot <- grid.arrange(p1, p2, ncol = 1)
#   
#   # plotting both plots together
#   ggsave(paste0("Score_dist_desired_model", "_", segment,"_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
#          path = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/"))
# 
#   db_to_predict$cluster = as.numeric(db_to_predict$cluster)
#   geom.mean = db_to_predict %>% select(-c(logit, odds, prob, prob_ctrl, score_ctrl)) %>% mutate_if(is.numeric, funs(`+1` = .+1))
#   x = geom.mean %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
#   x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`score_+1_-1`)
#   
#   names <- as.data.frame(names(x))
#   names <- str_sub(names$`names(x)`, end = -7)
#   names(x) <- names
#   
#   x$cluster = as.factor(x$cluster)
#   
#   df <- melt(x,  id.vars = c("score","cluster"), variable.name = "variable")
#   cluster_prob <- ggplot(df, aes(`score`, value)) + geom_line() + 
#     facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
#     geom_point(df, mapping = aes(`score`, value, color=factor(cluster), size = 5)) +
#     scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
#     theme(legend.position="bottom") +
#     labs(title=paste0("Collection model clusters | ", desired_model, " | ", segment), x="score", y="geometric mean of selected variable", col = "cluster") +
#     guides(size = "none", colour = "legend")
#   
#   ###############################
#   #character variables (boxplot)#
#   ###############################
#   
#   var_character = db_to_predict %>% mutate_if(is.character, as.factor)
#   var_character = var_character %>% select_if(is.factor) 
#   var_character = var_character %>% select_if(~ nlevels(.) < 200) #retaining just variables with less than 28 levels
#   
#   nm <- names(var_character)
#   
#   plot_list = list()
#   for(i in seq_along(nm)){
#     p <- ggplot(db_to_predict, aes_string(x=paste0("reorder(", nm[i],", score, FUN = median)"), y="score")) +
#       geom_boxplot() + coord_flip() + labs(x = nm[i], y = "score")
#     plot_list[[i]] = p
#   }
#   
#   ##############
#   #saving plots#
#   ##############
#   
#   # Save char cluster plots to tiff. Makes a separate file for each plot.
#   for (i in seq_along(nm)) {
#     file_name = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_char_",desired_model,"_",segment,"_", Sys.Date(),"_", nm[i], ".tiff")
#     tiff(file_name, units="in", width=12, height=8, res=500)
#     print(plot_list[[i]])
#     dev.off()
#   }
#   
#   # Save numeric cluster plot to tiff. Unique plot (comparing all clusters).
#   filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_distribution_",desired_model,"_",segment,"_",Sys.Date(),".tiff")
#   tiff(filename, units="in", width=12, height=8, res=500)
#   print(cluster_prob)
#   dev.off()
#   
#   fwrite(db_to_predict, file = paste0("predictions_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
#   save(db_to_predict, file = paste0("predictions_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
# }

#score = prob_good * 1000
develop_scorecard <- function(selected_var, desired_model, segment){
  
  require(dplyr)
  require(ggplot2)
  require(scorecard)
  require(gridExtra)
  require(reshape2)
  require(stringr)
  require(psych)
  require(tidyverse)
  require(rlang)
  library(plyr)
  
  db_to_predict = dbs$test_db_woe
  
  #db_to_predict$target = ifelse(db_to_predict$target == 1, "bad payer", "good payer")
  db_to_predict$target = as.factor(db_to_predict$target)
  db_to_predict = model.matrix(target ~ ., db_to_predict)[,-1]
  
  points0 = 600
  odds0 = 50
  pdo = 20
  
  pred = glmnet::predict.glmnet(lasso.model, newx = db_to_predict)
  resp = glmnet::predict.glmnet(lasso.model, newx = db_to_predict, type = "response")
  
  db_to_predict = dbs$test_db_woe
  
  factor = pdo/log(2)
  offset = points0 - factor * log(odds0)
  
  final_db = db_to_predict %>% mutate(logit = c(pred),
                                      odds = c(exp(pred)),
                                      prob_bad = c(odds/(odds + 1)),
                                      prob_good = 1 - prob_bad,
                                      prob_ctrl = c(resp))  
  
  final_db$score_ctrl = offset - (factor*final_db$logit)
  final_db$score = final_db$prob_good*1000
  final_db$score = round(final_db$score,0)
  
  score_metrics = c("logit", "odds", "prob_good", "prob_bad", "prob_ctrl", "score_ctrl", "score")
  
  ##############################
  # 
  # card <<- scorecard(bins_mod, model.glm, 
  #                    points0 = points0, 
  #                    odds0 = 1/odds0, 
  #                    pdo = pdo)
  
  # sc = scorecard_ply(db_to_predict, card, only_total_score = TRUE)
  
  final_scorecard <<- bind_rows(bins_mod) %>% filter(variable %in% selected_var) %>% dplyr::select(-count, -count_distr, -good, -bad, -is_special_values, -breaks)
  
  db_to_predict = dbs$test_db
  db_to_predict <- data.frame(dplyr::select(db_to_predict, c(selected_var, target)), dplyr::select(final_db, score_metrics))
  
  set.seed(123)
  centers <- kmeans(db_to_predict$score, centers = 4, iter.max = 500, nstart = 100)$centers
  centers <- sort(centers)
  set.seed(123)
  clusters <- kmeans(db_to_predict$score, centers = centers, iter.max = 500, nstart = 100)
  db_to_predict$cluster = as.factor(clusters$cluster)
  db_to_predict <<- db_to_predict
  db_to_predict$target <- revalue(db_to_predict$target, c("0"="good payer", "1"="bad payer"))
  db_to_predict$cluster = revalue(db_to_predict$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  
  mean_score_db_to_predict <- db_to_predict %>%
    dplyr::group_by(target) %>%
    dplyr::summarise(tb = mean(score))
  
  p1 <- ggplot(db_to_predict, aes_string(x = "score", color = "target", fill = "target"), environment = environment()) + 
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = mean_score_db_to_predict$tb[1]), linetype = "dashed", color = "green4") +
    geom_vline(aes(xintercept = mean_score_db_to_predict$tb[2]), linetype = "dashed", color = "brown2") +
    guides(fill = "legend", colour = "none") +
    theme(legend.position="bottom") +
    scale_color_manual(values=c("green4", "brown2")) +
    scale_fill_manual(values=c("green4", "brown2")) +
    labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 1: Scorecard Distribution by two original collection targets for test data", 
         subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")
  
  mean_cluster_db_to_predict <- db_to_predict %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(tb = mean(score))
  
  p2 <- ggplot(db_to_predict, aes_string(x = "score", color = "cluster", fill = "cluster"), environment = environment()) +
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
    geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
    geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
    geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
    guides(fill = "legend", colour = "none") +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    theme(legend.position="bottom") +
    labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for test data",
         caption = paste0("Based on data from IGB: ", segment, " | ", desired_model))
  
  both_plot <- grid.arrange(p1, p2, ncol = 1)
  
  # plotting both plots together
  ggsave(paste0("Score_dist_desired_model", "_", segment,"_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
         path = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/"))
  
  db_to_predict$cluster = as.numeric(db_to_predict$cluster)
  geom.mean = db_to_predict %>% select(-c(logit, odds, prob_good, prob_bad, prob_ctrl, score_ctrl)) %>% mutate_if(is.numeric, funs(`+1` = .+1))
  x = geom.mean %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
  x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`score_+1_-1`)
  
  names <- as.data.frame(names(x))
  names <- str_sub(names$`names(x)`, end = -7)
  names(x) <- names
  
  x$cluster = as.factor(x$cluster)
  x$cluster = revalue(x$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  
  df <- melt(x,  id.vars = c("score","cluster"), variable.name = "variable")
  cluster_prob <- ggplot(df, aes(`score`, value)) + geom_line() + 
    facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
    geom_point(df, mapping = aes(`score`, value, color=factor(cluster), size = 5)) +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    theme(legend.position="bottom") +
    labs(title=paste0("Collection model clusters | ", desired_model, " | ", segment), x="score", y="geometric mean of selected variable", col = "cluster") +
    guides(size = "none", colour = "legend")
  
  ###############################
  #character variables (boxplot)#
  ###############################
  
  var_character = db_to_predict %>% mutate_if(is.character, as.factor)
  var_character = var_character %>% select_if(is.factor) 
  var_character = var_character %>% select_if(~ nlevels(.) < 200) #retaining just variables with less than 28 levels
  
  nm <- names(var_character)
  
  plot_list = list()
  for(i in seq_along(nm)){
    p <- ggplot(db_to_predict, aes_string(x=paste0("reorder(", nm[i],", score, FUN = median)"), y="score")) +
      geom_boxplot() + coord_flip() + labs(x = nm[i], y = "score")
    plot_list[[i]] = p
  }
  
  ##############
  #saving plots#
  ##############
  
  # Save char cluster plots to tiff. Makes a separate file for each plot.
  for (i in seq_along(nm)) {
    file_name = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_char_",desired_model,"_",segment,"_", Sys.Date(),"_", nm[i], ".tiff")
    tiff(file_name, units="in", width=12, height=8, res=500)
    print(plot_list[[i]])
    dev.off()
  }
  
  # Save numeric cluster plot to tiff. Unique plot (comparing all clusters).
  filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_distribution_",desired_model,"_",segment,"_",Sys.Date(),".tiff")
  tiff(filename, units="in", width=12, height=8, res=500)
  print(cluster_prob)
  dev.off()
  
  fwrite(db_to_predict, file = paste0("predictions_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  save(db_to_predict, file = paste0("predictions_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
}

