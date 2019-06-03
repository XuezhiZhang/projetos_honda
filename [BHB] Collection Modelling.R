require(caret)
require(recipes)
require(dplyr)
require(e1071)
require(MASS)
require(glmnet)
require(`oetteR-master`)
require(broom)

##################
#MODEL 1 (1 - 60)#
##################
set.seed(42)

#2W
mod_11_60_2W$target = as.factor(mod_11_60_2W$target)
index_11_60_2W <- caret::createDataPartition(mod_11_60_2W$target, p = 0.7, list = FALSE)
train_data_11_60_2W <- mod_11_60_2W[index_11_60_2W, ]
test_data_11_60_2W  <- mod_11_60_2W[-index_11_60_2W, ]; rm(index_11_60_2W); gc();gc()

#4W
mod_11_60_4W$target = as.factor(mod_11_60_4W$target)
index_11_60_4W <- caret::createDataPartition(mod_11_60_4W$target, p = 0.7, list = FALSE)
train_data_11_60_4W <- mod_11_60_4W[index_11_60_4W, ]
test_data_11_60_4W  <- mod_11_60_4W[-index_11_60_4W, ]; rm(index_11_60_4W); gc();gc()

#2W
mod_61_360_2W$target = as.factor(mod_61_360_2W$target)
index_61_360_2W <- caret::createDataPartition(mod_61_360_2W$target, p = 0.7, list = FALSE)
train_data_61_360_2W <- mod_61_360_2W[index_61_360_2W, ]
test_data_61_360_2W  <- mod_61_360_2W[-index_61_360_2W, ]; rm(index_61_360_2W); gc();gc()

#4W
mod_61_360_4W$target = as.factor(mod_61_360_4W$target)
index_61_360_4W <- caret::createDataPartition(mod_61_360_4W$target, p = 0.7, list = FALSE)
train_data_61_360_4W <- mod_61_360_4W[index_61_360_4W, ]
test_data_61_360_4W <- mod_61_360_4W[-index_61_360_4W, ]; rm(index_61_360_4W); gc();gc()

##########################################
X <- model.matrix(target ~., train_11_60_2W_woe.test)[,-1]
y <- train_11_60_2W_woe.test$target

glmmod<-glmnet(X,y=as.factor(y), alpha=1, family="binomial")
summary(glmmod)

plot(glmmod,xvar="lambda")

coef(glmmod, cv.glmmod$lambda.1se)

cv.glmmod<-cv.glmnet(X,y=y,alpha=1,family = "binomial", type.measure = "auc", parallel = TRUE)
coef(cv.glmmod, cv.glmmod$lambda.1se)

cv.glmmod$lambda.1se

z = glance(cv.glmmod) #min and 1se lambda from the glmnet model

fit = glmnet(as.matrix(mtcars[-1]), mtcars[,1])
coef(glmmod, s = cv.glmmod$lambda.1se)

z = as.data.frame(as.matrix(coef(glmmod, s = cv.glmmod$lambda.1se)))


require(MASS)
#over sampling
ctrl <- trainControl(method = "cv", 
                     number = 5)

model_glm <- train(target ~ .,
                              data = woe_11_60,
                              method="glm", family="binomial")

(model_glm)

final_over <- data.frame(actual = test_data$classes,
                         predict(model_rf_over, newdata = test_data, type = "prob"))

final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")
cm_over <- confusionMatrix(final_over$predict, test_data$classes)
dput(names(mod_11_60_iv))

train.data.11.60$target = as.numeric(train.data.11.60$target)
teste = downSample(x = select(train.data.11.60, -target),
                   y = train.data.11.60$target)

teste.up <- upSample(x = select(train.data.11.60, -target),
                     y = train.data.11.60$target)

rose = ROSE(target ~ ., data = train.data.11.60, seed = 123)
require(ROSE)

x = SMOTE(target ~ ., data = )

##################

vars_11_60_2w = names(mod_61_360_2W)
vars_11_60_2w = vars[!vars %in% c("target", "cod_contrato")]

formula_11_60_2w = as.formula(paste('target ~', paste(vars, collapse = '+')))

lasso_11_60_2w = oetteR::f_train_lasso(data = mod_11_60_2W
                                     , p = NULL
                                     , formula = formula
                                     , k = 50
                                     , family = 'binomial')


set.seed(123)
class(mod_11_60_2W$target)
x <- model.matrix(target ~ ., mod_11_60_2W)[,-1]

cv.lasso <- cv.glmnet(dplyr::select(mod_11_60_2W, -target), dplyr::select(mod_11_60_2W, target), alpha = 1, family = "binomial")
x = glmnet(, y = "target", family = "binomial", alpha = 1, lambda = NULL)

install.packages("devtools")
library(devtools)
install_github("erblast/oetteR")

require("oetteR-master")

install.packages("D:\Users\sb044936\Documents\R\R-3.5.1\library\oetteR.", repos=NULL, type="source") 

#######################
#LASSO BY CARET
set.seed(100)

tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
glmnet_model_11_60_2W <- train(target ~ ., data = train_11_60_2W_woe.test,
                               method = "glmnet", trControl = tr)

# Caret tested many values of the tuning parameters so we limit output
arrange(glmnet_model$results, RMSE) %>% head

glmnet_model$bestTune

require(foreach)
registerDoSEQ()
options(max.print= 99999, width = 9999)
coef(glmnet_model$finalModel, glmnet_model$bestTune$.lambda)
x <- as.data.frame(unlist(coef(glmnet_model$finalModel, glmnet_model$bestTune$.lambda)))

dfCoef <- data.frame(lambda=model.penalized$finalModel[['lambda']],
                     as.matrix(t(model.penalized$finalModel[['beta']])))
