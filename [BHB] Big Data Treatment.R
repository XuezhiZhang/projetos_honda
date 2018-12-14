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
      
      "Starting step 2: cross-validation to find best lambda.\n\n" %>% cat()
      registerDoSEQ() #library to "unregister" a foreach backend, registering the sequential backend
      # Finding best lambda using cross-validation
      set.seed(123)
      cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",  type.measure = "auc", parallel = TRUE)
      "Step 2: cross-validation to find best lambda done\n." %>% cat()
      
      filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Lambda_", segment, "_", Sys.Date(), ".tiff")
      tiff(filename, units="in", width=12, height=8, res=500)
      plot(cv.lasso, main = paste0(desired_model, " | ", segment, " | ", Sys.Date()))
      dev.off()
          
          "Starting step 3: Running LASSO model with selected features.\n\n" %>% cat()
          # Creating model with lambda 1se
          lasso.model <- glmnet(X, y, alpha = 1, family = "binomial",
                                lambda = cv.lasso$lambda.min + 10*(cv.lasso$lambda.1se - cv.lasso$lambda.min))
          "Step 3: logistic model with selected features done.\n" %>% cat()
          
          coefs <- tidy(lasso.model)
          fmla <- as.formula(paste("target ~ ", paste(coefs$term[-1], collapse= "+")))
          selected_var = paste(coefs$term[-1], sep = "\n")
          selected_var = data.frame(selected_var)
          selected_var = str_sub(selected_var$selected_var, end = -5); selected_var <<- c(selected_var, "target")
          
          glm.model <<- glm(fmla, dbs$train_db_woe, family = "binomial")
          
              "Starting step 4: Predictions and model performance measures.\n\n" %>% cat()
              # Predicting model in test database
              # probabilities <- glm.model %>% predict(test_db_woe, type="response")
              probabilities = predict(glm.model, newdata = dbs$test_db_woe, type="response")
              
              # Checking probabilities x target in test database
              pred <- prediction(probabilities, dbs$test_db_woe$target)
              acc.perf = performance(pred, "sens", "spec")

              # Find probabilities cutoff with greater sum of specificity and sensitivity
              cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]
            
              predicted.classes <- ifelse(probabilities > cutoff, 1, 0)
              observed.classes <- dbs$test_db_woe$target
              
              # Calculating KS, auc, gini
              evaluation <- perf_eva(observed.classes, c(predicted.classes), show_plot = TRUE)
             
              # Calculating features importance
              imp = caret::varImp(glm.model) %>% tibble::rownames_to_column("variable") %>% arrange(desc(Overall)) %>% dplyr::rename("importance" = "Overall") %>% filter(!importance == 0)
              
              # Creating dataframe with probabilities
              probs_by_contract <<- data.frame(select(dbs$test_db, selected_var), bad_payer_prob = c(probabilities), predicted_target = predicted.classes)
              
              # Creating confusion matrix
              conf.matrix <<- caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes))
              
              "Step 4: Predictions and model performance measures done.\n\n" %>% cat()
                  # Results printing on console
                  paste0("Results from lasso model for", " ", names(cv.lasso)[10],"\n\n") %>% cat()
                  paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()
                  
                  print(summary(glm.model))
                  "######################################################\n\n" %>% cat()
                  print(imp)
                  "######################################################\n\n" %>% cat()
                  print(evaluation)
                  "######################################################\n\n" %>% cat()
                  print(caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes)))
                  "######################################################\n\n" %>% cat()
                  
                  "Starting step 5: Clustering contracts by bad payer probability.\n\n" %>% cat()
                  clustering(probs_by_contract, desired_model, segment)
                  "Step 5: Clustering contracts by bad payer probability done.\n\n" %>% cat()
                  "Starting step 6: Writing all needed files in their paths.\n\n" %>% cat()
                  writing(glm.model, selected_var, probs_by_contract, imp, desired_model, segment)
                  "Step 6: Writing all needed files in their paths done.\n\n" %>% cat()
                  }

desired_model = "11_60"
desired_model = "61_360"
segment = "CAR"
segment = "MOT"
modelling(mod_11_60, "11_60", "CAR")
modelling(mod_11_60, "11_60", "MOT")
modelling(mod_61_360, "61_360", "CAR")
modelling(mod_61_360, "61_360", "MOT")

########

