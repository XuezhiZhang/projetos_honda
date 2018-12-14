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
      perc_pg_atr_11_60 <= 0.75, 1, 0)) %>%
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
      perc_pg_atr_61_360 <= 0.75, 1, 0)) %>%
  mutate(target = ifelse(is.na(target), 1, target)) %>%
  select(-perc_pg_atr_61_360, -tempo_contrato)

mod_61_360_2W_valid = mod_61_360_valid %>% filter(segmento == "MOT")
mod_61_360_4W_valid = mod_61_360_valid %>% filter(segmento == "CAR")

########################################

setwd("D:/Users/sb044936/Desktop/Modelling databases R/11_60/Models")
load("lasso_model_11_60_MOT_2018-12-13.RData")
setwd("D:/Users/sb044936/Desktop/Modelling databases R/11_60/Databases")
load("bins_mod_11_60_MOT.RData")

db_to_predict = mod_11_60_valid %>% filter(segmento == "MOT")

zerovar = nearZeroVar(db_to_predict, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
db_to_predict = db_to_predict[,-zerovar]

preproc = preProcess(select(db_to_predict, -target), method = "bagImpute", k = 5)
db_to_predict <- predict(preproc, db_to_predict)

db_to_predict$target = factor(db_to_predict$target)

#WOE
db_woe_bin = woebin(select(db_to_predict, -cod_contrato), y = "target")
db_woe = woebin_ply(db_to_predict, db_woe_bin)

"Starting step 4: Predictions and model performance measures.\n\n" %>% cat()
# Predicting model in test database
# probabilities <- glm.model %>% predict(test_db_woe, type="response")
probabilities = predict(glm.model, newdata = db_woe, type="response")

# Checking probabilities x target in test database
pred <- prediction(probabilities, db_woe$target)
acc.perf = performance(pred, "sens", "spec")

# Find probabilities cutoff with greater sum of specificity and sensitivity
cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]

predicted.classes <- ifelse(probabilities > cutoff, 1, 0)
observed.classes <- db_woe$target

# Calculating KS, auc, gini
evaluation <- perf_eva(observed.classes, c(predicted.classes), show_plot = TRUE)

# Creating dataframe with probabilities
probs_by_contract <<- data.frame(select(db_to_predict, selected_var), bad_payer_prob = c(probabilities), predicted_target = predicted.classes)

"Step 4: Predictions and model performance measures done.\n\n" %>% cat()
# Results printing on console
paste0("Results from lasso model for", " ", names(cv.lasso)[10],".\n\n") %>% cat()
paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()

print(summary(glm.model))
"######################################################\n\n" %>% cat()
print(imp)
"######################################################\n\n" %>% cat()
print(evaluation)
"######################################################\n\n" %>% cat()
print(caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes)))
"######################################################\n\n" %>% cat()

points0 = 600
odds0 = 50
pdo = 20

card <<- scorecard(bins_mod, model, 
                   points0 = points0, 
                   odds0 = 1/odds0, 
                   pdo = pdo)

sc = scorecard_ply(db_to_predict, card, only_total_score = FALSE)

final_scorecard <<- bind_rows(card) %>% select(-count, -count_distr, -good, -bad, -is_special_values, -breaks)

db_to_predict <<- data.frame(db_to_predict, sc)

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
centers <- stats::kmeans(db_to_predict$score, centers = 4, iter.max = 500, nstart = 100)$centers
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
  labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for test data")#,
       #caption = paste0("Based on data from IGB: ", segment, " | ", desired_model))

both_plot <- grid.arrange(p1, p2, ncol = 1)

filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Clusters_score",desired_model,"_",segment,"_",Sys.Date(),".tiff")
tiff(filename, units="in", width=12, height=8, res=500)
print(both_plot)
dev.off()





