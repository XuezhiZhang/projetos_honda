require(data.table)
require(dplyr)

# Derived from "[BHB] Reading historical fechamento.R" script
setwd("~/IGB/Histórico Fechamento/2017")
load("2017.RData")
setwd("~/IGB/Histórico Fechamento/2018")
load("2018.RData")
setwd("~/IGB/Histórico Fechamento/2019")
load("2019.RData")

# selecting 201901 to 201903
train.valid.201901.201903 = db.2019 %>% filter(data_safra %in% c("01/02/2019",
                                                                 "01/03/2019",
                                                                 "01/04/2019",
                                                                 "03/05/2019") & 
                                               situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
setwd("~/IGB/Histórico Fechamento")
save(train.valid.201901.201903, file = "train_valid_201901_201903.RData")

# selecting 201807 to 201812
train.valid.201810.201812 = db.2018 %>% filter(data_safra %in% c("01/11/2018",
                                                                 "03/12/2018",
                                                                 "03/01/2019") & 
                                               situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
setwd("~/IGB/Histórico Fechamento")
save(train.valid.201810.201812, file = "train_valid_201810_201812.RData")

# selecting 201801 to 201806 (missing data_safra 072018)
train.valid.201801.201808 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                                 "02/03/2018",
                                                                 "02/04/2018",
                                                                 "02/05/2018",
                                                                 "04/06/2018",
                                                                 "01/08/2018",
                                                                 "03/09/2018") & 
                                               situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
setwd("~/IGB/Histórico Fechamento")
save(train.valid.201801.201808, file = "train_valid_201801_201808.RData")

# selecting 201709 to 201712
train.valid.201709.201712 = db.2017 %>% filter(data_safra %in% c("02/10/2017",
                                                                 "01/11/2017",
                                                                 "01/12/2017",
                                                                 "29/12/2017") & 
                                                 situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
setwd("~/IGB/Histórico Fechamento")
save(train.valid.201709.201712, file = "train_valid_201709_201712.RData")

# joins of timeframes databases
setwd("~/IGB/Histórico Fechamento")
load("train_valid_201709_201712.RData")
load("train_valid_201801_201808.RData")
load("train_valid_201810_201812.RData")
load("train_valid_201901_201903.RData")

train.valid.201709.201808 = bind_rows(train.valid.201709.201712, train.valid.201801.201808); rm(train.valid.201709.201712, train.valid.201801.201808)
train.valid.201810.201903 = bind_rows(train.valid.201810.201812, train.valid.201901.201903); rm(train.valid.201810.201812, train.valid.201901.201903)
save(train.valid.201709.201808, file = "train_valid_201709_201808.RData")
save(train.valid.201810.201903, file = "train_valid_201810_201903.RData")
load("train_valid_201709_201808.RData")
load("train_valid_201810_201903.RData")

# selecting timeframe to filter database
min = 1; max = 30
min = 31; max = 60
min = 61; max = 90
min = 91; max = 180
min = 181; max = 360

# filtering rows that are between 'min' and 'max' days overdue
model_db = train.valid.201709.201808 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                qtd_dias_em_atraso >= min & 
                                                qtd_dias_em_atraso <= max)

# changing all date columns to date format
model_db = model_db %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                         list(~as.Date(as.character(.), 
                                              format = "%d/%m/%Y")))

# loading historic payment files
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("historic_delay_info_till_201808.RData")
load("historic_delay_info_till_201903.RData")

# loading target files
load("target_201809.RData")
load("target_201904.RData")

model_db = left_join(model_db, count.all)
model_db = left_join(model_db, target.201809)
model_db = model_db %>% filter(!is.na(target))

segment = "CAR"
segment = "MOT"

#########################
# Preparing database: excluding zero variance columns and transforming data with weight of evidence (woe)

preparing <- function(model_db, segment){
  
  require(dplyr)
  require(scorecard)
  require(ggplot2)
  require(stringr)
  require(caret)
  require(foreach)
  require(ROSE)
  require(DMwR)
  
  db = model_db %>% filter(segmento == segment) %>% select(-c(cod_contrato, cpf_cnpj, data_safra, 
                                                              data_contrato, data_primeiro_atraso, 
                                                              data_ult_pgt, data_vencimento, status_contrato, 
                                                              num_chassi, tabela, assessoria, assessoria_cob, 
                                                              cod_hda, score))
  
  db_teste = model_db %>% filter(segmento == segment) %>% select()
  
  "The distribution of target level is: " %>% cat()
  prop.table(table(db$target))
  
  registerDoSEQ()
  # removing columns with variance = 0 (no discrimination values)
  zerovar = nearZeroVar(db, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db = db[,-zerovar]
  
  bins_mod <- woebin(db, y = "target")
  # setting to null because these columns are duplicating levels
  bins_mod$perc_parc_renda = NULL
  bins_mod$perc_vnc_renda = NULL
  
  db_woe = as.data.frame(woebin_ply(db, bins_mod))
  db_woe$target = as.factor(db_woe$target)

  registerDoSEQ()
  
  zerovar = nearZeroVar(db_woe, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  db_woe = db_woe[,-zerovar]
  
  # imputing missing data again because in test_woe there are some classes 
  # of the variables that did not existed when producing bins from train dataset
  preproc = preProcess(db_woe, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL

  dbs <<- list("db_woe" = db_woe, 
               "db" = db)
}