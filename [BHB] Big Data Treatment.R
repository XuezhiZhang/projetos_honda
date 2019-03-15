# loading all functions created for modelling: preparing, writing & develop_scorecard
setwd("R:/Estatística/BHB/R Scripts")
if(!exists("foo", mode="function")) source("[BHB] Functions modelling.R")

# Function to develop models and release outputs
modelling <- function(mod_segment, model, segment){
  
  require(caret)
  require(glmnet)
  require(dplyr)
  require(scorecard)
  require(stringr)
  require(foreach)
  require(ROCR)
  require(broom)
  require(tibble)
  require(DMwR)
  require(ROSE)
  require(psych)
  require(reshape2)
  
  desired_model <<- desired_model
  segment <<- segment
      
      "Starting step 1: processing, binning and splitting data.\n\n" %>% cat()
      # Preparing all binning database automatically. Arguments: database, model, segment.
      preparing(mod_segment = mod_segment, desired_model = desired_model, segment = segment)
      
      "Step 1: data is already processed, binned and splitted.\n" %>% cat()
  
      # Preparing modelling matrixes
      X <- model.matrix(target ~ ., dbs$train_db_woe)[,-1]
      y <- dbs$train_db_woe$target
      # weights = dbs$train_db_woe$model_weights
        
      X.test <- model.matrix(target ~ ., dbs$test_db_woe)[,-1]
      
      # column numbers which are to be forced entering in the model
      pf = which(names(dbs$train_db_woe)[-1] %in% c("perc_parc_renda_woe", "perc_pg_finan_woe", "perc_entrada_financ_woe", "prazo_contrato_woe"))
      penalty.factor=c(rep(1, ncol(dbs$train_db_woe)-1))
      
      # change `penalty factor` to 0 of variables forced to enter the model
      for(i in 1:length(pf)){
        penalty.factor[pf[i]] = 0}
      
      "Starting step 2: cross-validation to find best lambda.\n\n" %>% cat()
      registerDoSEQ() #library to "unregister" a foreach backend, registering the sequential backend
      # Finding best lambda using cross-validation
      set.seed(123)
      cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",  type.measure = "auc")#, parallel = TRUE, penalty.factor = penalty.factor)
      "Step 2: cross-validation to find best lambda done.\n\n" %>% cat()
      
      filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Lambda_", segment, "_", Sys.Date(), ".tiff")
      tiff(filename, units="in", width=12, height=8, res=500)
      plot(cv.lasso)#, main = paste0(desired_model, " | ", segment, " | ", Sys.Date()))
      dev.off()
      
      tmp_coeffs <- coef(cv.lasso, s = "lambda.1se")
      variables = nrow(data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x))
      
          "Starting step 3: running LASSO to select features.\n\n" %>% cat()
          # Creating model with lambda 1se
          lambda = ifelse(variables <= 20, cv.lasso$lambda.1se, 
                          cv.lasso$lambda.min + 2*(cv.lasso$lambda.1se - cv.lasso$lambda.min))
          
          # testing to add weights to model
          # model_weights <- ifelse(dbs$train_db_woe$target == "1",
          #                         (1/table(dbs$train_db_woe$target)[1]) * 0.5,
          #                         (1/table(dbs$train_db_woe$target)[2]) * 0.5)
          
          # cross-validation to obtain best alpha & lambda in a loop (elastic net)
          # a <- seq(0.1, 0.9, 0.05)
          # search <- foreach(i = a, .combine = rbind) %dopar% {
          #   cv <- cv.glmnet(X, y, 
          #                   family = "binomial", nfold = 10, type.measure = "auc", paralle = TRUE, alpha = i)
          #   data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
          # }
          # cv3 <- search[search$cvm == min(search$cvm), ]
          # lasso.model <- glmnet(X, y, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
          
          lasso.model <- glmnet(X, y, family = "binomial", lambda = lambda, alpha = 1)#, penalty.factor = penalty.factor)
          "Step 3: LASSO model done.\n" %>% cat()

          coefs <- tidy(lasso.model)
          coefs <- coefs[-1,]
          selected_var_woe = paste(coefs$term, sep = "\n"); selected_var_woe <<- c(selected_var_woe, "target")
          selected_var = str_sub(coefs$term, end = -5); selected_var <<- c(selected_var, "target")
          
          # development of model using caret: not using because of memory. but it is the best way. :)
          # caret needs lots of memory (cross validation and outputs).
          
          # #glm.model <<- glm(fmla, dbs$train_db_woe, family = "binomial")
          # 
          # model_db = dbs$train_db_woe; model_db$target = ifelse(model_db$target == 1, "badpayer", "goodpayer")
          # model_test = dbs$test_db_woe; model_test$target = ifelse(model_test$target == 1, "badpayer", "goodpayer")
          # 
          # model_weights <- ifelse(model_db$target == "badpayer",
          #                         (1/table(model_db$target)[1]) * 0.5,
          #                         (1/table(model_db$target)[2]) * 0.5)
          # 
          # control <- trainControl(method="repeatedcv",
          #                         classProbs = TRUE,
          #                         summaryFunction = twoClassSummary,
          #                         savePredictions = "all", number = 10, repeats = 3)
          # set.seed(7)
          # lasso.model.caret <- caret::train(target ~ ., data = model_db,
          #                                   method = "glmnet", family = "binomial",
          #                                   trControl=control,
          #                                   tuneGrid = expand.grid(alpha = 1,
          #                                                          lambda = seq(0.001,1, length = 30)))
          # 
          # # levels(model_db$target) <- make.names(levels(factor(model_db$target)))
          # 
          # control$sampling <- "smote"
          # model.smote <- caret::train(target ~ ., data = model_db,
          #                             method = "glmnet", family = "binomial",
          #                             trControl=control,
          #                             tuneGrid = expand.grid(alpha = 1,
          #                                                    lambda = seq(0.001,1, length = 20)))
          # 
          # control$sampling <- "rose"
          # model.rose = caret::train(target ~ ., data = model_db,
          #                           method = "glmnet", family = "binomial",
          #                           trControl=control,
          #                           weights = model_weights,
          #                           tuneGrid = expand.grid(alpha = 1,
          #                                                  lambda = seq(0.001,1, length = 20)))
          # 
          # control$sampling <- "up"
          # model.up = caret::train(target ~ ., data = model_db,
          #                             method="rf",
          #                             trControl=control)
          # 
          # control$sampling <- "down"
          # model.down = caret::train(target ~ ., data = model_db,
          #                           method = "glmnet", family = "binomial",
          #                           trControl=control,
          #                           tuneGrid = expand.grid(alpha = 1,
          #                                                  lambda = seq(0.001,1, length = 20)))
          # 
          # results <- caret::resamples(list(LASSO= lasso.model.caret,
          #                                  ROSE = model.rose))
          # 
          # # summarise the distributions
          # summary(results)
          # bwplot(results, ncol = 3)

              "Starting step 4: predictions and model performance measures.\n\n" %>% cat()
              # Predicting model in test database
              # probabilities <- glm.model %>% predict(test_db_woe, type="response")
              # probabilities = c(predict(lasso.model.caret$finalModel, newx = X.test,
              #                   type="response", s = lasso.model.caret$bestTune$lambda))
              # probabilities = predict(lasso.model.caret, dbs$test_db_woe, type="prob")
              probabilities <- predict(lasso.model, newx = X.test, type="response", s = lambda)

              # Checking probabilities x target in test database
              pred <- prediction(probabilities, dbs$test_db_woe$target)
              # pred <- prediction(probabilities$badpayer, model_test$target)
              
              acc.perf = ROCR::performance(pred, "sens", "spec")

              plot(acc.perf)
              
              # Find probabilities cutoff with greater sum of specificity and sensitivity
              cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]

              predicted.classes <- ifelse(probabilities > cutoff, "badpayer", "goodpayer")
              observed.classes <- ifelse(dbs$test_db_woe$target == "bad", "badpayer", "goodpayer")
              
              observed.classes.num <- ifelse(observed.classes == "badpayer", 1, 0)
              predicted.classes.num <- ifelse(predicted.classes == "badpayer", 1, 0)
              
              # Calculating KS, auc, gini
              evaluation <- scorecard::perf_eva(label = observed.classes.num, pred = probabilities, show_plot = TRUE)
              evaluation$confusion_matrix
              df <- melt(data.frame(evaluation$binomial_metric$dat))
              names(df) <- c("Statistic", "Value")
              
              # Calculating features importance
              imp = caret::varImp(lasso.model, lambda = lambda) %>% tibble::rownames_to_column("variable") %>% arrange(desc(Overall)) %>% dplyr::rename("importance" = "Overall") %>% filter(!importance == 0)
              
              # Creating dataframe with probabilities
              probs_by_contract <<- data.frame(select(dbs$test_db, selected_var), bad_payer_prob = c(probabilities), predicted_target = predicted.classes)
              
              # Creating confusion matrix
              conf.matrix <<- caret::confusionMatrix(as.factor(predicted.classes),
                                                     as.factor(observed.classes), positive = "badpayer")
              
              "Step 4: Predictions and model performance measures done.\n\n" %>% cat()
                  # Results printing on console
                  paste0("Results from lasso model for", " ", names(cv.lasso)[10],"\n\n") %>% cat()
                  paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()
                  
                  print(tidy(lasso.model))
                  "######################################################\n\n" %>% cat()
                  print(imp)
                  "######################################################\n\n" %>% cat()
                  print(evaluation)
                  "######################################################\n\n" %>% cat()
                  print(caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes)))
                  "######################################################\n\n" %>% cat()
                  
                  conf_db = as.data.frame(as.matrix(conf.matrix, what = "overall")); conf_db = rownames_to_column(conf_db, "Statistic")
                  conf_db2 = as.data.frame(as.matrix(conf.matrix, what = "classes")); conf_db2 = rownames_to_column(conf_db2, "Statistic")
                  conf = bind_rows(conf_db, conf_db2); colnames(conf) <- c("Statistic", "Value"); conf$Value = round(conf$Value, 4)
                  perf_stats1 = bind_rows(conf, df)
                  perf_stats2 = as.data.frame(conf.matrix$table)

                  "Starting step 5: Writing all needed files in their paths.\n\n" %>% cat()
                  writing(lasso.model, selected_var, probs_by_contract, imp, desired_model, segment, perf_stats1, perf_stats2)
                  "Step 5: Writing all needed files in their paths done.\n\n" %>% cat()
                  "Starting step 5: Creating clusters and scorecard.\n\n" %>% cat()
                  develop_scorecard(selected_var, desired_model, segment)
                  "Step 5: Clustering and creating scorecard done.\n\n" %>% cat()
                  }