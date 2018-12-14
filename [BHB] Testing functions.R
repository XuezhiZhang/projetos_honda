require(dplyr)
require(scorecard)
require(data.table)
require(foreach)
require(ROSE)
require(elasticnet)

# Function to get name of an object and transform it in a character string
myfunc <- function(v1){
  deparse(substitute(v1))
}

#creating final database and target variable
mod_11_60 <- bhb.final %>% filter(qtd_dias_em_atraso >= 11 & qtd_dias_em_atraso <= 60) %>%
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

setwd("D:/Users/sb044936/Desktop/Modelling databases R/11_60/Databases")
#save(mod_11_60, file = "mod_11_60.RData")
load("mod_11_60.RData")

setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Databases")
#save(mod_61_360, file = "mod_61_360.RData")
load("mod_61_360.RData")

# filtering 2W
mod_11_60_2W = mod_11_60 %>% filter(segmento == "MOT")
mod_11_60_4W = mod_11_60 %>% filter(segmento == "CAR")

mod_61_360_2W = mod_61_360 %>% filter(segmento == "MOT")
mod_61_360_4W = mod_61_360 %>% filter(segmento == "CAR")

#verifying the percentage of 1 (bad payer)
table(mod_11_60_2W$target)[2]/(table(mod_11_60_2W$target)[1]+table(mod_11_60_2W$target)[2])
table(mod_11_60_4W$target)[2]/(table(mod_11_60_4W$target)[1]+table(mod_11_60_4W$target)[2])

table(mod_61_360_2W$target)[2]/(table(mod_61_360_2W$target)[1]+table(mod_61_360_2W$target)[2])
table(mod_61_360_4W$target)[2]/(table(mod_61_360_4W$target)[1]+table(mod_61_360_4W$target)[2])

# creating bins
bins_mod_11_60_2W = woebin(dplyr::select(mod_11_60_2W, -cod_contrato), y = "target")
bins_mod_11_60_4W = woebin(dplyr::select(mod_11_60_4W, -cod_contrato), y = "target")
bins_mod_61_360_2W = woebin(dplyr::select(mod_61_360_2W, -cod_contrato), y = "target")
bins_mod_61_360_4W = woebin(dplyr::select(mod_61_360_4W, -cod_contrato), y = "target")

setwd("D:/Users/sb044936/Desktop/Modelling databases R/61-360/Plots")
plotlist = woebin_plot(bins_mod_61_360_4W)

# save binning plot
for (i in 1:length(plotlist)){
  ggplot2::ggsave(
    paste0("4W_61_360","_",names(plotlist[i]),"_",Sys.Date(),".png"), plotlist[[i]],
    width = 15, height = 9, units="cm" )
}

setwd("D:/Users/sb044936/Desktop/Modelling databases R/11_60/Databases")
# save(bins_mod_11_60_2W, file = "bins_mod_11_60_2W.RData")
# save(bins_mod_11_60_4W, file = "bins_mod_11_60_4W.RData")
load("bins_mod_11_60_2W.RData")
load("bins_mod_11_60_4W.RData")

setwd("D:/Users/sb044936/Desktop/Modelling databases R/61_360/Databases")
# save(bins_mod_61_360_2W, file = "bins_mod_61_360_2W.RData")
# save(bins_mod_61_360_4W, file = "bins_mod_61_360_4W.RData")
load("bins_mod_61_360_2W.RData")
load("bins_mod_61_360_4W.RData")

# creating dataframe with woe bins and adding "cod_contrato"
db_11_60_2W_woe = woebin_ply(dplyr::select(mod_11_60_2W, -cod_contrato), bins_mod_11_60_2W)
db_11_60_4W_woe = woebin_ply(dplyr::select(mod_11_60_4W, -cod_contrato), bins_mod_11_60_4W)

db_61_360_2W_woe = woebin_ply(dplyr::select(mod_61_360_2W, -cod_contrato), bins_mod_61_360_2W)
db_61_360_4W_woe = woebin_ply(dplyr::select(mod_61_360_4W, -cod_contrato), bins_mod_61_360_4W)

db_11_60_2W_woe$target = as.factor(db_11_60_2W_woe$target)
db_11_60_4W_woe$target = as.factor(db_11_60_4W_woe$target)

db_61_360_2W_woe$target = as.factor(db_61_360_2W_woe$target)
db_61_360_4W_woe$target = as.factor(db_61_360_4W_woe$target)

# splitting final database in train and test
set.seed(123)
index_11_60_2W <- caret::createDataPartition(db_11_60_2W_woe$target, p = 0.7, list = FALSE)
train_data_11_60_2W_woe <- db_11_60_2W_woe[index_11_60_2W, ]
test_data_11_60_2W_woe  <- db_11_60_2W_woe[-index_11_60_2W, ]

train_data_11_60_2W <- mod_11_60_2W[index_11_60_2W, ]
test_data_11_60_2W  <- mod_11_60_2W[-index_11_60_2W, ]; rm(index_11_60_2W); gc();gc()

rm(bins_mod_11_60_2W, mod_11_60_2W, bhb.final)

##
set.seed(123)
index_11_60_4W <- caret::createDataPartition(db_11_60_4W_woe$target, p = 0.7, list = FALSE)
train_data_11_60_4W_woe <- db_11_60_2W_woe[index_11_60_4W, ]
test_data_11_60_4W_woe  <- db_11_60_2W_woe[-index_11_60_4W, ]

train_data_11_60_4W <- mod_11_60_2W[index_11_60_4W, ]
test_data_11_60_4W  <- mod_11_60_2W[-index_11_60_4W, ]; rm(index_11_60_4W); gc();gc()

##
set.seed(123)
index_61_360_2W <- caret::createDataPartition(db_61_360_2W_woe$target, p = 0.7, list = FALSE)
train_data_61_360_2W_woe <- db_61_360_2W_woe[index_61_360_2W, ]
test_data_61_360_2W_woe  <- db_61_360_2W_woe[-index_61_360_2W, ]

train_data_61_360_2W <- mod_61_360_2W[index_61_360_2W, ]
test_data_61_360_2W  <- mod_61_360_2W[-index_61_360_2W, ]; rm(index_61_360_2W); gc();gc()

##
set.seed(123)
index_61_360_4W <- caret::createDataPartition(db_61_360_4W_woe$target, p = 0.7, list = FALSE)
train_data_61_360_4W_woe <- db_61_360_4W_woe[index_61_360_4W, ]
test_data_61_360_4W_woe  <- db_61_360_4W_woe[-index_61_360_4W, ]

train_data_61_360_4W <- mod_61_360_4W[index_61_360_4W, ]
test_data_61_360_4W  <- mod_61_360_4W[-index_61_360_4W, ]; rm(index_61_360_4W); gc();gc()

############

train_data_61_360_2W_woe$model_weights = ifelse(train_data_61_360_2W_woe$target == 0,
                                                (1/table(train_data_61_360_2W_woe$target)[1]) * 0.5,
                                                (1/table(train_data_61_360_2W_woe$target)[2]) * 0.5)

#######################################
#######################################
#######################################
require(caret)

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(fmla,  data=train_db_woe, method="glm", family="binomial",
                 trControl = ctrl)

pred = predict(glm.model, test_data_11_60_2W, type = "response")

x <- perf_eva(test_data_11_60_2W$target, pred)

accuracy <- table(pred, test_data_11_60_2W$target)
sum(diag(accuracy))/sum(accuracy)

require(ROCR)

auc <- performance(prediction, test_data_11_60_2W$target, measure = "acc")
A <- perf_eva(c(pred), c(test_data_11_60_2W$target), type = "response")

acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

ind = which.max(slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))
roc = performance(pred, "tpr", "fpr")
plot(roc)

points0 = 900
odds0 = 20
pdo = 25

card = scorecard(bins_mod_11_60_2W, glm.model, 
                 points0 = points0 ,
                 odds0 = 1/odds0, # scorecard wants the inverse
                 pdo = pdo)

sc = scorecard_ply(select(mod_11_60_2W, -cod_contrato), card, only_total_score = FALSE)
mod_11_60_2W$score = sc[[19]]
hist(mod_11_60_2W$score)


df = data.frame(mod_11_60_2W, sc)


load("glm_model_predictions_11_60_2W_2018-11-27.RData")

test_db$probs = probs_by_contract$probability_bad

clusters <- kmeans(probs_by_contract$probability_bad, 4)
test_db$cluster = clusters[[1]]


######################
#CLUSTER TEST
#####################
require(psych)

geom.mean = probs_by_contract %>% mutate_if(is.numeric, funs(`+1` = .+1)) 
x = geom.mean %>% select_if(is.numeric) %>% group_by(cluster) %>% summarise_all(geometric.mean) %>% select(ends_with("+1"))
x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`bad_payer_prob_+1_-1`)

names <- as.data.frame(names(x))
names <- str_sub(names$`names(x)`, end = -7)
names(x) <- names

x$bad_payer_prob = round(x$bad_payer_prob, 4)
x$cluster = as.factor(x$cluster)

df <- melt(x,  id.vars = c("bad_payer_prob","cluster"), variable.name = "variable")
ggplot(df, aes(`bad_payer_prob`, value)) + geom_line() + 
  facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
  geom_point(df, mapping = aes(`bad_payer_prob`, value, color=factor(bad_payer_prob), size = 5)) +
  scale_color_manual(values=c("green", "yellow", "orange", "red")) +
  theme(legend.position="none") +
  labs(title="Collection model clusters | 4W | 61-360\n", x="Bad payer probability", y="Geometric mean of selected variable")

#MELHOR GRÁFICO!!!
#################################

confusionMatrix(as.factor(predicted.classes), as.factor(observed.classes), positive = "1")


ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 3, 
                     returnData = FALSE)

ctrl2 <- trainControl(method="cv", number=5, returnResamp="all",
                      classProbs=TRUE, summaryFunction=twoClassSummary)

set.seed(123)
model_glm <- caret::train(fmla,
                          data = train_db_woe,
                          method = "glm", family = "binomial",
                          trControl = ctrl,
                          model = FALSE)

selected_var = paste(br$term[-1], sep = "\n")
trainX = select(train_db_woe, selected_var)
trainY = as.factor(train_db_woe$target)

trainY = as.factor(ifelse(trainY == "1", "bad.payer", "good.payer"))


X <- model.matrix(target ~ ., select(train_db_woe, -cod_contrato))[,-1]
y <- train_db_woe$target

trainX = make.names(trainX)
trainY = make.names(trainY)

test_class_cv_model <- train(trainX, trainY, method = "glmnet", 
                             trControl = ctrl2, metric = "ROC",
                             tuneGrid = expand.grid(alpha = 1,
                                                    lambda = seq(0.001,0.1,by = 0.001)))

# best parameter
test_class_cv_model$bestTune
coef(test_class_cv_model$finalModel, test_class_cv_model$bestTune$lambda)

pred = predict(test_class_cv_model, newdata=test_db_woe)
predction = prediction()
confusionMatrix(data=pred, test_db_woe$target)

teste = function(train_db, test_db)
  model_lasso_smote <- caret::train(target ~ ., data = train_db,
                                    method = "lasso", metric = "auc",
                                    trControl = ctrl)

final_over <- data.frame(actual = test_db_woe$target,
                         predict(model_glm, newdata = test_db_woe, type = "prob"))


final_over$predict <- ifelse(final_over$X0 > 0.5, 0, 1)

cm_over <- confusionMatrix(as.factor(final_over$predict), as.factor(test_db_woe$target), positive = "1")
print(cm_over)

ggplot(probs_by_contract, aes(,value)) + geom_line() + facet_grid(series ~ .)

x.teste <- x %>% select(`perc_ult_pgt.parc`, `cluster`) 
x.teste <- data.frame(x$`perc_ult_pgt.parc`, x$`cluster`)

names(trainX)


data.rose <- ROSE(target ~ ., data = select(train_data_61_360_4W_woe, -cod_contrato), seed = 1, p = 0.5)$data

db_11_60_4W_woe <- db_11_60_4W_woe[,apply(db_11_60_4W_woe, 2, var, na.rm=TRUE) != 0] #removing constant 0 columns
coefs <- tidy(model)

glm <- glm(target ~ idade_cli + nome_municipio_cli, mod_11_60_2W, family= "binomial")
