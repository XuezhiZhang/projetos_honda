coef = data.frame(summary(glm.model)$coefficients)
coef$variable = row.names(coef)
coef = setnames(data.table::setDT(coef)[, c(1, 5), with = FALSE], c("Estimate", 
                                                        "var_woe"))[, `:=`(variable, gsub("_woe$", "", var_woe))][]
len_x = coef[-1, .N]
basepoints = a - b * coef[1, Estimate]
card = list()
if (basepoints_eq0) {
  card[["basepoints"]] = data.table(variable = "basepoints", 
                                    bin = NA, woe = NA, points = 0)
  for (i in coef[-1, variable]) {
    card[[i]] = bins[variable == i][, `:=`(points, round(-b * 
                                                           coef[variable == i, Estimate] * woe + basepoints/len_x))]
  }
  
points0 = 600
odds0 = 50
pdo = 20

db_to_predict = dbs$test_db_woe
db_to_predict = data.matrix(dbs$test_db_woe)
  
pred = predict(lasso.model, newx = X.test)
resp = predict(lasso.model, newx = X.test, type = "response")

factor = pdo/log(2)
offset = points0 - factor * log(odds0)

final_db = dbs$test_db %>% mutate(logit = pred,
                                    odds = exp(pred),
                                    prob = odds/(odds + 1),
                                    prob_ctrl = resp)  
  
factor = pdo / log(2)
offset = points0 - factor * log(odds0)

final_db$score_ctrl = offset - (factor*final_db$logit)
final_db$score = round(final_db$score_ctrl,0)
