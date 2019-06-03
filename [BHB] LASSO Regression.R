library(glmnet)
require(ROCR)
require(glmnetUtils)
require(biglasso)

# Creating dataframe with train database 11-60 4W with woe tranformation

x <- model.matrix(target ~ ., train_11_60_4W_woe)[,-1]
y <- train_11_60_4W_woe$target

# Find the best lambda using cross-validation
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "auc")

# Fit the final model on the training data
model.1se <- glmnet(x, y, alpha = 1, family = "binomial",
                    lambda = cv.lasso$lambda.1se)
model.min <- glmnet(x, y, alpha = 1, family = "binomial",
                    lambda = cv.lasso$lambda.min)

fmla <- as.formula(paste("target ~ ", paste(br$term[-1], collapse= "+")))

# Display regression coefficients
coefs_1se = coef(model.1se)
coefs_min = coef(model.min)

#model comparing
qty_initial_var = length(coefs_1se@Dimnames[[1]])
qty_var_lambda_min = length(coefs_min@x)
qty_var_lambda_1se = length(coefs_1se@x)
qty_var_excluded_min = qty_initial_var - qty_var_lambda_min
qty_var_excluded_1se = qty_initial_var - qty_var_lambda_1se

qty_var_min = data.frame(lambda = "min", qty_initial_var, qty_var = qty_var_lambda_min, qty_var_excluded = qty_var_excluded_min, "perc_var_excluded" = round(qty_var_excluded_min / qty_initial_var, 2))
qty_var_1se = data.frame(lambda = "1se", qty_initial_var, qty_var = qty_var_lambda_1se, qty_var_excluded = qty_var_excluded_1se, "perc_var_excluded" = round(qty_var_excluded_1se / qty_initial_var, 2))

qty_var = bind_rows(qty_var_min, qty_var_1se)
# Make predictions on the test data
x.test <- model.matrix(target ~ ., test_11_60_4W_woe)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")

# Model accuracy
observed.classes <- test_11_60_4W_woe$target
mean(predicted.classes == observed.classes)

br = tidy(model.1se) %>% mutate("abs estimate" = abs(estimate))