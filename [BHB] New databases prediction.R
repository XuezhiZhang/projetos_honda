require(stringr)
require(dplyr)
require(broom)
require(scorecard)
require(caret)
require(foreach)
require(glmnet)
require(gridExtra)
require(tibble)

mod_11_60_valid <- bhb.final %>% filter(qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 60) %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
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
         -perc_pg_atr_1_60, -perc_pg_atr_61_360, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score,
         -pnad_versao, -pnad_ano, -descricao_uf) %>%
  mutate(target = ifelse(
    tempo_contrato <= 1 |
      perc_pg_atr_11_60 <= median(perc_pg_atr_11_60), 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>% 
  select(-perc_pg_atr_11_60, -tempo_contrato)

mod_61_360_valid <- bhb.final %>% filter(qtd_dias_em_atraso >= 61 & qtd_dias_em_atraso <= 360) %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
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
         -perc_pg_atr_1_60, -perc_pg_atr_11_60, -perc_pg_atr_1_10, -perc_pg_atr_360_mais, -vlr_seg_gara_estend, -status_contrato, -qtd_parcelas_pagas, -score,
         -pnad_versao, -pnad_ano, -descricao_uf) %>%
  mutate(target = ifelse(
    tempo_contrato <= 1 |
      perc_pg_atr_61_360 <= median(perc_pg_atr_61_360), 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>%
  select(-perc_pg_atr_61_360, -tempo_contrato)

mod_61_360_2W_valid = mod_61_360_valid %>% filter(segmento == "MOT")
mod_61_360_4W_valid = mod_61_360_valid %>% filter(segmento == "CAR")

########################################

setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Models")
load("lasso_model_61_360_MOT_2019-01-16(no-overfitted).RData")
setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Databases")
load("bins_mod_61_360_MOT.RData")
setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Predictions")
load("selected_var_61_360_MOT_2019-01-16(no-overfitted).RData")

db_to_predict = mod_61_360 %>% filter(segmento == "MOT")

#WOE
# db_woe_bin = woebin(select(db_to_predict, -cod_contrato), y = "target")
db_woe = woebin_ply(db_to_predict, bins_mod)

registerDoSEQ()
zerovar = nearZeroVar(db_woe, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
db_to_predict = db_woe %>% select(-zerovar)

preproc = preProcess(db_to_predict, method = "knnImpute", k = 5)
db_to_predict <- predict(preproc, db_to_predict)

db_to_predict$target = factor(db_to_predict$target)

db_to_predict_matrix = as.matrix(select(db_to_predict, -c(cod_contrato, target, perc_pg_atr_61_360)))

"Starting step 4: Predictions and model performance measures.\n\n" %>% cat()
# Predicting model in test database
# probabilities <- glm.model %>% predict(test_db_woe, type="response")
probabilities = predict(lasso.model, newx = db_to_predict_matrix, type="response")
                        
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
names(df) <- c("Statistic", "Value")

selected_var = coefs <- tidy(lasso.model)
coefs <- coefs[-1,]
selected_var_woe = paste(coefs$term, sep = "\n"); selected_var_woe <<- c(selected_var_woe, "target")
selected_var = str_sub(coefs$term, end = -5); selected_var <<- c("cod_contrato", selected_var, "target")

# Creating dataframe with probabilities
db_to_predict = mod_61_360 %>% filter(segmento == "MOT")

probs_by_contract <<- data.frame(dplyr::select(db_to_predict, selected_var), bad_payer_prob = c(probabilities), predicted_target = predicted.classes)

paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()

conf.matrix = caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes))

print(summary(lasso.model))
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
#########################

final_db %>% 
  group_by(target) %>% 
  summarise(tb = mean(score)) %>% 
  ungroup() -> mean_score_db_to_predict

final_db$target = as.factor(final_db$target)
final_db$target <- revalue(final_db$target, c("0"="good payer", "1"="bad payer"))

mean_score_db_to_predict <- final_db %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(tb = mean(score))

p1 <- final_db %>% 
  ggplot(aes(score, color = factor(target), fill = factor(target))) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_score_db_to_predict$tb[1]), linetype = "dashed", color = "green4") +
  geom_vline(aes(xintercept = mean_score_db_to_predict$tb[2]), linetype = "dashed", color = "brown2") +
  scale_color_manual(values=c("green4", "brown2")) +
  scale_fill_manual(values=c("green4", "brown2")) +
  guides(fill = "legend", colour = "none") +
  theme(legend.position="bottom") +
  labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 1: Scorecard Distribution by two original collection targets for validation data", 
       subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")

set.seed(123)
centers <- stats::kmeans(final_db$score, centers = 4, iter.max = 500, nstart = 100)$centers
centers <- sort(centers)
set.seed(123)
clusters <- kmeans(final_db$score, centers = centers, iter.max = 500, nstart = 100)
final_db$cluster = as.factor(clusters$cluster)

final_db$cluster = revalue(final_db$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))

mean_cluster_db_to_predict <- final_db %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(tb = mean(score)) 

p2 <- final_db %>% 
  ggplot(aes(score, color = factor(cluster), fill = factor(cluster))) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
  geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
  geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
  geom_vline(aes(xintercept = mean_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
  guides(fill = "legend", colour = "none") +
  scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  theme(legend.position="bottom") +
  labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for validation data",
       caption = paste0("Based on data from IGB: ", segment, " | ", desired_model))

both_plot <- grid.arrange(p1, p2, ncol = 1)

db_to_predict = final_db %>% select(c(selected_var, cluster, score))
db_to_predict$cluster = as.numeric(db_to_predict$cluster)
geom.mean = db_to_predict %>% mutate_if(is.numeric, funs(`+1` = .+1))
x = geom.mean %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`score_+1_-1`)

names <- as.data.frame(names(x))
names <- str_sub(names$`names(x)`, end = -7)
names(x) <- names

x$cluster = as.factor(x$cluster)
x$cluster = revalue(x$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))

df2 <- melt(x,  id.vars = c("score","cluster"), variable.name = "variable")
cluster_prob <- ggplot(df2, aes(`score`, value)) + geom_line() + 
  facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
  geom_point(df2, mapping = aes(`score`, value, color=factor(cluster), size = 5)) +
  scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  theme(legend.position="bottom") +
  labs(title=paste0("Collection model clusters | ", desired_model, " | ", segment), x="collection score", y="geometric mean of selected variable", col = "cluster",
       caption = paste0("Based on data from IGB: ", segment, " | ", desired_model)) +
  guides(size = "none", colour = "legend")

#####################################

# plotting both plots together
ggsave(paste0("Score_dist_desired_model_validation", "_", segment,"_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
       path = paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/"))

filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_distribution_validation_",desired_model,"_",segment,"_",Sys.Date(),".tiff")
tiff(filename, units="in", width=12, height=8, res=500)
print(cluster_prob)
dev.off()

conf_db = as.data.frame(as.matrix(conf.matrix, what = "overall")); conf_db = rownames_to_column(conf_db, "Statistic")
conf_db2 = as.data.frame(as.matrix(conf.matrix, what = "classes")); conf_db2 = rownames_to_column(conf_db2, "Statistic")
conf = bind_rows(conf_db, conf_db2); colnames(conf) <- c("Statistic", "Value"); conf$Value = round(conf$Value, 4)
perf_stats1 = bind_rows(conf, df)
perf_stats2 = as.data.frame(conf.matrix$table)

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model, "/Predictions"))
fwrite(perf_stats1, file = paste0("performance_stats_1_valid_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
fwrite(perf_stats2, file = paste0("performance_stats_2_valid_", desired_model, "_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")

