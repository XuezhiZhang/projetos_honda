preparing <- function(mod_segment, desired_model, segment){
  
  require(dplyr)
  require(scorecard)
  require(ggplot2)
  require(stringr)
  require(caret)
  require(ROSE)
  
  db = mod_segment %>% filter(segmento == segment) %>% select(-cod_contrato)
  
  perc = table(db$target)[2]/(table(db$target)[1]+table(db$target)[2])
  paste0("The percentage of bad payers is: ", round(perc, 4), ".") %>% cat()
  
  # removing columns with variance = 0 (no discrimination values)
  zerovar = nearZeroVar(db, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db = db[,-zerovar]
  
  # missing data imputation with 5 nearest neighbourhoods
  preproc = preProcess(db, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  db <- predict(preproc, db)
  
  # resampling method: ROSE
  
  db = db %>% mutate_if(is.character, factor)
  db_rose <- ROSE(target ~ ., data = db, seed = 1, p = 0.5, hmult.majo = 0.25, hmult.mino = 0.5)$data #in case of concerning about extremely distant neighbourhoods bias important data (or most common data), set hmult.majo = 0.25 and hmult.mino = 0.5 (it will shrunk the space of creation for artificial data)
  if(perc > 0.5){db_selected = db_rose} else {db_selected = db}
  
  perc = table(db_rose$target)[2]/(table(db_rose$target)[1]+table(db_rose$target)[2])
  paste0("After balancing data with ROSE sampling method, the percentage of bad payer is: ", round(perc,4), ".\n\n") %>% cat()
  
  bins_mod <<- scorecard::woebin(db_selected, y = "target")
  
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
  
  db_woe = woebin_ply(db_selected, bins_mod)
  db_woe$target = as.factor(db_woe$target)
  
  set.seed(123)
  index <- caret::createDataPartition(db_woe$target, p = 0.7, list = FALSE)
  train_db_woe <- db_woe[index, ]
  test_db_woe  <- db_woe[-index, ]
  
  train_db <- db_selected[index, ]
  test_db  <- db_selected[-index, ]
  
  dbs <<- list("train_db_woe" = train_db_woe, "test_db_woe" = test_db_woe, "train_db" = train_db, "test_db" = test_db)
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
  x = geom.mean %>% select_if(is.numeric) %>% group_by(cluster) %>% summarise_all(geometric.mean) %>% select(ends_with("+1"))
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

writing <- function(model, coefs, probs_by_contract, imp, desired_model, segment){

require(data.table)
require(stringr)
  
  # Automatically creating files for desired outputs
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model, "/Models"))
  save(model, file = paste0("glm_model_", desired_model,"_",segment,"_", Sys.Date(),".RData"))
    
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model, "/Predictions"))
  save(selected_var, file = paste0("selected_var_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
  fwrite(probs_by_contract, file = paste0("glm_model_predictions_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  save(probs_by_contract, file = paste0("probs_by_contract_", desired_model, "_", segment,"_", Sys.Date(),".RData"))
  fwrite(imp, file = paste0("var_importance_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  }

develop_scorecard <- function(db, selected_var, desired_model, segment){
  
  require(dplyr)
  require(ggplot2)
  require(scorecard)
  require(gridExtra)
  
  db_to_predict = dbs$test_db
  db_to_predict$target = ifelse(db_to_predict$target == 1, "bad payer", "good payer")
  
  points0 = 600
  odds0 = 50
  pdo = 20
  
  card <<- scorecard(bins_mod, glm.model, 
                     points0 = points0, 
                     odds0 = 1/odds0, 
                     pdo = pdo)
  
  sc = scorecard_ply(db_to_predict, card, only_total_score = FALSE)
  
  final_scorecard <<- bind_rows(card) %>% select(-count, -count_distr, -good, -bad, -is_special_values, -breaks)
  
  db_to_predict <<- data.frame(select(db_to_predict, selected_var), sc)
  
  db_to_predict %>% 
    group_by(target) %>% 
    summarise(tb = mean(score)) %>% 
    ungroup() -> mean_score_db_to_predict
  
  p1 <- db_to_predict %>% 
    ggplot(aes(score, color = factor(target), fill = factor(target))) + 
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = mean_score_db_to_predict$tb[1]), linetype = "dashed", color = "red") + 
    geom_vline(aes(xintercept = mean_score_db_to_predict$tb[2]), linetype = "dashed", color = "blue") + 
    guides(fill = "legend", colour = "none") +
    theme(legend.position="bottom") +
    labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 1: Scorecard Distribution by two original collection targets for test data", 
         subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")
  
  set.seed(123)
  centers <- kmeans(db_to_predict$score, centers = 4, iter.max = 500, nstart = 100)$centers
  centers <- sort(centers)
  set.seed(123)
  clusters <- kmeans(db_to_predict$score, centers = centers, iter.max = 500, nstart = 100)
  db_to_predict$cluster = clusters$cluster
  
  db_to_predict %>% 
    group_by(cluster) %>% 
    summarise(tb = mean(score)) %>% 
    ungroup() -> mean_cluster_db_to_predict
  
  p2 <- db_to_predict %>% 
    ggplot(aes(score, color = factor(cluster), fill = factor(cluster))) + 
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = 600), linetype = "dashed", color = "black") + 
    # geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "yellow2") + 
    # geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "darkorange1") + 
    # geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "brown2") + 
    guides(fill = "legend", colour = "none") +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    theme(legend.position="bottom") +
    labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for test data",
         caption = paste0("Based on data from IGB: ", segment, " | ", desired_model))
  
  both_plot <- grid.arrange(p1, p2, ncol = 1)

  filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_score",desired_model,"_",segment,"_",Sys.Date(),".tiff")
  tiff(filename, units="in", width=12, height=8, res=500)
  print(both_plot)
  dev.off()
}
