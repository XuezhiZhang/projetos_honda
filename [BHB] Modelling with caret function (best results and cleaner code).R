preparing <- function(db_to_predict, segment){
  
  require(dplyr)
  require(scorecard)
  require(ggplot2)
  require(stringr)
  require(caret)
  require(foreach)
  require(ROSE)
  require(DMwR)
  
  db = db_to_predict %>% filter(segmento == segment) %>% select(-cod_contrato)
  
  perc = table(db$target)[1]/(table(db$target)[1]+table(db$target)[2])
  paste0("The percentage of bad payers is: ", round(perc, 4), ". ") %>% cat()
  
  registerDoSEQ()
  # removing columns with variance = 0 (no discrimination values)
  zerovar = nearZeroVar(db, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db = db[,-zerovar]
  
  set.seed(123)
  bins_mod <<- scorecard::woebin(db, y = "target")
  # setting to null because these columns are duplicating levels
  bins_mod$perc_parc_renda = NULL
  bins_mod$perc_vnc_renda = NULL
  
  db_woe = as.data.frame(scorecard::woebin_ply(db, bins_mod))
 
  db_woe$perc_parc_renda = NULL
  db_woe$perc_vnc_renda = NULL
  
  db_woe$target = as.factor(db_woe$target)

  registerDoSEQ()
  
  zerovar = nearZeroVar(db_woe, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db_woe = db_woe[,-zerovar]
  db_woe = db_woe[,-zerovar]
  
  # imputing missing data again because in test_woe there are some classes 
  # of the variables that did not existed when producing bins from train dataset
  preproc = preProcess(db_woe, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  db_woe <- predict(preproc, db_woe)
  
  dbs <<- list("db_woe" = db_woe, 
               "db_raw" = db)
}

preparing(db_to_predict, segment)

# repeated cross validation: 
control <- trainControl(method="repeatedcv",
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "all", number = 10, repeats = 3)
set.seed(7)
lasso.model.caret <- caret::train(target ~ ., data = dbs$db_woe,
                                  method = "glmnet", family = "binomial",
                                  trControl=control,
                                  tuneGrid = expand.grid(alpha = 1,
                                                         lambda = seq(0.001,1, length = 30)))

control$sampling <- "smote"
model.smote <- caret::train(target ~ ., data = dbs$db_woe,
                            method = "glmnet", family = "binomial",
                            trControl=control,
                            tuneGrid = expand.grid(alpha = 1,
                                                   lambda = seq(0.001,1, length = 20)))

control$sampling <- "rose"
model.rose = caret::train(target ~ ., data = dbs$db_woe,
                          method = "glmnet", family = "binomial",
                          trControl=control,
                          tuneGrid = expand.grid(alpha = 1,
                                                 lambda = seq(0.001,1, length = 20)))

control$sampling <- "up"
model.up = caret::train(target ~ ., data = dbs$db_woe,
                        method = "glmnet", family = "binomial",
                        trControl=control,
                        tuneGrid = expand.grid(alpha = 1,
                                               lambda = seq(0.001,1, length = 20)))

control$sampling <- "down"
model.down = caret::train(target ~ ., data = dbs$db_woe,
                          method = "glmnet", family = "binomial",
                          trControl=control,
                          tuneGrid = expand.grid(alpha = 1,
                                                 lambda = seq(0.001,1, length = 20)))

results <- caret::resamples(list(LASSO = lasso.model.caret,
                                 SMOTE = model.smote,
                                 ROSE = model.rose,
                                 UP = model.up,
                                 DOWN = model.down))

# summarise the distributions
summary(results)
bwplot(results)

# importance of variables and definition of lambda (filtering least variables as possible)
imp = varImp(model.up, lambda = model.up$bestTune$lambda*80)$importance 
imp$variable = rownames(imp); imp = imp %>% filter(!Overall == 0) %>% arrange(desc(Overall)) %>% mutate(Overall = round(Overall,2))

# developing validation database
db_raw_valid = db_to_predict %>% filter(segmento == segment & qtd_dias_em_atraso >= min & qtd_dias_em_atraso <= max)
dbs$db_woe_valid = as.data.frame(scorecard::woebin_ply(db_raw_valid, bins_mod)) %>% select(names(dbs$db_woe))

preproc = preProcess(dbs$db_woe_valid, method = "knnImpute", k = 5)
preproc$method$center = NULL
preproc$method$scale = NULL
dbs$db_woe_valid <- predict(preproc, dbs$db_woe_valid)

table(is.na(dbs$db_woe_valid))

pred_results <- predict(model.up, newdata=dbs$db_woe_valid, s =  model.up$bestTune$lambda*80)
prob_results <- predict(model.up, newdata=dbs$db_woe_valid, s =  model.up$bestTune$lambda*80, type = "prob")

dbs$db_woe_valid$target_pred = pred_results
dbs$db_woe_valid$target_prob_bad = prob_results$bad
dbs$db_woe_valid$target_prob_good = prob_results$good

# checking probabilities x target in test database
pred <- prediction(probabilities, dbs$test_db_woe$target)

# creating vectors with 1 to "bad" and 0 to "good" for creating dataframe with perf statistics
observed.classes.num <- ifelse(dbs$db_woe_valid$target == "bad", 1, 0)
predicted.classes.num <- ifelse(dbs$db_woe_valid$target_prob_bad == "bad", 1, 0)

evaluation <- scorecard::perf_eva(label = observed.classes.num, pred = dbs$db_woe_valid$target_prob_bad, show_plot = TRUE)
evaluation$confusion_matrix
df <- melt(data.frame(evaluation$binomial_metric$dat))
names(df) <- c("Statistic", "Value")

# checking probabilities x target in validation database
pred <- prediction(dbs$db_woe_valid$target_prob_good, dbs$db_woe_valid$target)
acc.perf = ROCR::performance(pred, "sens", "spec")
plot(acc.perf)

conf.matrix <<- caret::confusionMatrix(as.factor(dbs$db_woe_valid$target),
                                       as.factor(dbs$db_woe_valid$target_pred), positive = "bad")



