require(data.table)
require(dplyr)

# selecting contracys 1 - 30 days overdue 
prod.04.2019 = db.04.2019 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                     qtd_dias_em_atraso > 0 & 
                                     qtd_dias_em_atraso <= 30)

min = 1
max = 30

segment = "CAR"
segment = "MOT"

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("delay_count_by_contr_until_201904.RData")

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/", min, "-", max, "/", segment))
load("bins_CAR_2019-05-03.RData")
load("selected_var_CAR_2019-05-03.RData")
load("lasso_model_CAR_2019-05-03.RData")

setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/", min, "-", max, "/", segment))
load("bins_MOT_2019-04-29.RData")
load("selected_var_MOT_2019-04-29.RData")
load("lasso_model_MOT_2019-04-29.RData")