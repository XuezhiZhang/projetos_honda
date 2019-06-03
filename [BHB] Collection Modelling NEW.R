require(dplyr)
require(data.table)
require(tidyr)
require(lubridate)

setwd("~/IGB/Histórico Fechamento/2018")
load("2018.RData")

target.201707.201709  = db.2017 %>% filter(data_safra %in% 
                                             c("01/08/2017",
                                               "01/09/2017",
                                               "02/10/2017") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")); rm(db.2017)

target.201701.201706  = db.2017 %>% filter(data_safra %in% 
                                             c("01/02/2017",
                                               "02/03/2017",
                                               "03/04/2017",
                                               "02/05/2017",
                                               "01/06/2017",
                                               "03/07/2017") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")); rm(db.2017)

target.201607.201612 = db.2016 %>% filter(data_safra %in% 
                                             c("01/08/2016",
                                               "01/09/2016",
                                               "03/10/2016",
                                               "01/12/2016",
                                               "30/12/2016") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")); rm(db.2016)

target.201701.201703 = db.2017 %>% filter(data_safra %in% 
                                            c("01/02/2017",
                                              "02/03/2017",
                                              "03/04/2017") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")); rm(db.2017)

target.201801.201812 = db.2018 %>% filter(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))

target.201810.201812 = db.2018 %>% filter(data_safra %in% 
                                            c("01/11/2018",
                                              "03/12/2018",
                                              "03/01/2019") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))

target.201810.201812 = db.2018 %>% filter(data_safra %in% 
                                            c("01/11/2018",
                                              "03/12/2018",
                                              "03/01/2019") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))

target.201807.201812 = db.2018 %>% filter(data_safra %in% 
                                            c("01/08/2018",
                                              "03/09/2018",
                                              "01/10/2018",
                                              "01/11/2018",
                                              "03/12/2018",
                                              "03/01/2019") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))

target.201804.201812 = db.2018 %>% filter(data_safra %in% 
                                            c("02/05/2018",
                                              "04/06/2018",
                                              "02/07/2018",
                                              "01/08/2018",
                                              "03/09/2018",
                                              "01/10/2018",
                                              "01/11/2018",
                                              "03/12/2018",
                                              "03/01/2019") & situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))

save(target.201707.201709, file = "target_201707_201709.RData")
save(target.201701.201706, file = "target_201701_201706.RData")
save(target.201607.201612, file = "target_201607_201612.RData")
save(target.201701.201703, file = "target_201701_201703.RData")
save(target.201801.201812, file = "target_201801_201812.RData")
save(target.201810.201812, file = "target_201810_201812.RData")
save(target.201807.201812, file = "target_201807_201812.RData")
save(target.201804.201812, file = "target_201804_201812.RData")

target.201707.201709 = target.201707.201709  %>% mutate(target = ifelse(qtd_dias_em_atraso > 30, "bad", "good"))
target.201707.201709 = target.201707.201709  %>% group_by(cod_contrato, target) %>% summarise(n = n())

target.201707.201709 = spread(target.201707.201709, target, n)
target.201707.201709 = target.201707.201709 %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
target.201707.201709 = target.201707.201709 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

# selecting 201601 to 201606
train.test.201601.201606 = db.2016 %>% filter(data_safra %in% c("01/02/2016",
                                                                "02/03/2016",
                                                                "01/04/2016",
                                                                "02/05/2016",
                                                                "02/06/2016",
                                                                "04/07/2016") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30); rm(db.2016)

save(train.test.201601.201606, file = "train_test_201601_201606.RData")

# selecting 201507 to 201512
train.test.201507.201512 = db.2015 %>% filter(data_safra %in% c("04/08/2015",
                                                                "02/09/2015",
                                                                "02/10/2015",
                                                                "03/11/2015",
                                                                "01/12/2015",
                                                                "04/01/2016") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30); rm(db.2015)

save(train.test.201507.201512, file = "train_test_201507_201512.RData")

# selecting 201701 to 201706
train.test.201701.201706 = db.2017 %>% filter(data_safra %in% c("01/02/2017",
                                                                "02/03/2017",
                                                                "03/04/2017",
                                                                "02/05/2017",
                                                                "01/06/2017",
                                                                "03/07/2017") & 
                                              situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                              qtd_dias_em_atraso > 0 & 
                                              qtd_dias_em_atraso <= 30); rm(db.2017)
save(train.test.201701.201706, file = "train_test_201701.201706.RData")

train.test.201607.201612 = db.2016 %>% filter(data_safra %in% c("01/08/2016",
                                                                "01/09/2016",
                                                                "03/10/2016",
                                                                "01/12/2016",
                                                                "30/12/2016") & 
                                              situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                              qtd_dias_em_atraso > 0 & 
                                              qtd_dias_em_atraso <= 30); rm(db.2016)

# selecting 201601 to 201612
train.test.201601.201612 = db.2016 %>% filter(data_safra %in% c("01/02/2016",
                                                                "02/03/2016",
                                                                "01/04/2016",
                                                                "02/05/2016",
                                                                "02/06/2016",
                                                                "04/07/2016",
                                                                "01/08/2016",
                                                                "01/09/2016",
                                                                "03/10/2016",
                                                                "01/12/2016",
                                                                "30/12/2016") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30); rm(db.2016)

# selecting 201611 to 201612
train.test.201611.201612 = db.2016 %>% filter(data_safra %in% c("01/12/2016",
                                                                "30/12/2016") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30); rm(db.2016)

save(train.test.201611.201612, file = "train_test_201611_201612.RData")

# selecting 201712
train.test.201701.201710 = db.2017 %>% filter(data_safra %in% c("01/02/2017",
                                                                "02/03/2017",
                                                                "03/04/2017",
                                                                "02/05/2017",
                                                                "01/06/2017",
                                                                "03/07/2017",
                                                                "01/08/2017",
                                                                "01/09/2017",
                                                                "02/10/2017",
                                                                "01/11/2017") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30)

save(train.test.201701.201710, file = "train_test_201701.201710.RData")

# selecting 201801 to 201805
train.test.201701.201710 = db.2017 %>% filter(data_safra %in% c("01/02/2017",
                                                                "02/03/2017",
                                                                "03/04/2017",
                                                                "02/05/2017",
                                                                "01/06/2017",
                                                                "03/07/2017",
                                                                "01/08/2017",
                                                                "01/09/2017",
                                                                "02/10/2017",
                                                                "01/11/2017") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30)

save(train.test.201701.201710, file = "train_test_201701.201710.RData")

# selecting 201707 to 201712
train.test.201706.201712 = db.2017 %>% filter(data_safra %in% c("03/07/2017",
                                                                "01/08/2017",
                                                                "01/09/2017",
                                                                "02/10/2017",
                                                                "01/11/2017",
                                                                "01/12/2017",
                                                                "29/12/2017"))

save(train.test.201706.201712, file = "train_test_201706_201712.RData")
load("train_test_201706_201712.RData")
load("train_test_201801_201805.RData")

# selecting 201707 to 201712
train.test.201801.201805 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                                "02/03/2018",
                                                                "02/04/2018",
                                                                "02/05/2018",
                                                                "04/06/2018"))
save(train.test.201801.201805, file = "train_test_201801_201805.RData")

# selecting 201707 to 201712
train.test.201807.201812 = db.2018 %>% filter(data_safra %in% c("01/08/2018",
                                                                "03/09/2018",
                                                                "01/10/2018",
                                                                "01/11/2018",
                                                                "03/12/2018",
                                                                "03/01/2019"))
save(train.test.201807.201812, file = "train_test_201807_201812.RData")

# selecting 201707 to 201712
train.test.201901 = db.2019 %>% filter(data_safra %in% c("01/02/2019") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30) 

save(train.test.201901, file = "train_test_201901.RData")

# selecting 201712
train.test.201712 = db.2017 %>% filter(data_safra %in% c("29/12/2017") & 
                                         situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")&
                                         qtd_dias_em_atraso > 0 & 
                                         qtd_dias_em_atraso <= 30)

save(train.test.201712, file = "train.test.201712.RData")

save(train.test.201701.201706, file = "train_test_201701.201706.RData")

save(train.test.201601.201612, file = "train.test.201601.201612.RData")

save(train.test.201607.201612, file = "train_test_201607_201612.RData")

train.test.201607.201612 = train.test.201607.201612 %>% select(-assessoria_nova)

train.test.201607.201706 = bind_rows(train.test.201607.201612, train.test.201701.201706); rm(train.test.201607.201612, train.test.201701.201706)

target.201707.201709 = target.201707.201709 %>% select(-c(good, bad))

train.test.201607.201706 = left_join(train.test.201607.201706, target.201707.201709)

# selecting 201801 to 201809
valid.201801.201811 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                           "02/03/2018",
                                                           "02/04/2018",
                                                           "02/05/2018",
                                                           "04/06/2018",
                                                           "02/07/2018",
                                                           "01/08/2018",
                                                           "03/09/2018",
                                                           "01/10/2018",
                                                           "01/11/2018",
                                                           "03/12/2018") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201801.201811, file = "valid_201801_201811.RData")

# selecting 201801 to 201809
valid.201801.201809 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                           "02/03/2018",
                                                           "02/04/2018",
                                                           "02/05/2018",
                                                           "04/06/2018",
                                                           "02/07/2018",
                                                           "01/08/2018",
                                                           "03/09/2018",
                                                           "01/10/2018") & 
                                                situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                                qtd_dias_em_atraso > 0 & 
                                                qtd_dias_em_atraso <= 30)

# selecting 201801 to 201809
valid.201801.201811 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                           "02/03/2018",
                                                           "02/04/2018",
                                                           "02/05/2018",
                                                           "04/06/2018",
                                                           "02/07/2018",
                                                           "01/08/2018",
                                                           "03/09/2018",
                                                           "01/10/2018",
                                                           "01/11/2018",
                                                           "03/12/2018") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201801.201811, file = "valid_201801_201811.RData")
load("valid_201801_201811.RData")
load("valid_201712.RData")

# selecting 201801 to 201806
valid.201801.201806 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                           "02/03/2018",
                                                           "02/04/2018",
                                                           "02/05/2018",
                                                           "04/06/2018",
                                                           "02/07/2018") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201801.201806, file = "valid_201801_201806.RData")

# selecting 201801 to 201803
valid.201801.201803 = db.2018 %>% filter(data_safra %in% c("01/02/2018",
                                                           "02/03/2018",
                                                           "02/04/2018") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201801.201803, file = "valid_201801_201803.RData")

# selecting 201704 to 201712
valid.201704.201712 = db.2017 %>% filter(data_safra %in% c("02/05/2017",
                                                           "01/06/2017",
                                                           "03/07/2017",
                                                           "01/08/2017",
                                                           "01/09/2017",
                                                           "02/10/2017",
                                                           "01/11/2017",
                                                           "01/12/2017",
                                                           "29/12/2017") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201704.201712, file = "valid_201704_201712.RData")

# selecting 201707 to 201712
valid.201707.201712 = db.2017 %>% filter(data_safra %in% c("01/08/2017",
                                                           "01/09/2017",
                                                           "02/10/2017",
                                                           "01/11/2017",
                                                           "01/12/2017",
                                                           "29/12/2017") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201707.201712, file = "valid_201707_201712.RData")

# selecting 201704 to 201712
valid.201710.201712 = db.2017 %>% filter(data_safra %in% c("01/11/2017",
                                                           "01/12/2017",
                                                           "29/12/2017") & 
                                           situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED") &
                                           qtd_dias_em_atraso > 0 & 
                                           qtd_dias_em_atraso <= 30)

save(valid.201710.201712, file = "valid_201710_201712.RData")

#########################################
# join of timeframes databases + target #
#########################################

setwd("~/IGB/Histórico Fechamento/Models")
load("train_test_201611_201612.RData")
load("train_test_201701_201710.RData")
load("valid_201712.RData")
load("valid_201801_201811.RData")
train.test.201611.201710 = bind_rows(train.test.201611.201612, train.test.201701.201710); rm(train.test.201611.201612, train.test.201701.201710)
valid.201712.201811 = bind_rows(train.test.201712, valid.201801.201811); rm(train.test.201712, valid.201801.201811)

setwd("~/IGB/Histórico Fechamento/Models/Completes")
save(train.test.201607.201706, file = "train_test_201607_201706.RData")
save(train.test.201611.201710, file = "train_test_201611_201710.RData")
save(valid.201712.201811, file = "valid_201712_201811.RData")
load("train_test_201611_201710.RData")

# loading target database timeframe
load("target_201707_201709.RData")


target.201711  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2017-11") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201711 = target.201711  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201711 = target.201711  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201711 = target.201711 %>% group_by(cod_contrato, target) %>% summarise(n = n())
# cut = target.201711 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201711 = spread(target.201711, target, n)
target.201711 = target.201711 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201711 = target.201711 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201711 = target.201711 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201711, file = "target_201711.RData")
load("target_201711.RData")


##

target.201806  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2018-06") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201806 = target.201806  %>% mutate(target = ifelse(dias_atrasados > 60, "bad", "good"))
# target.201711 = target.201711  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201806 = target.201806 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())
# cut = target.201711 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201806 = spread(target.201806, target, n)
target.201806 = target.201806 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201806 = target.201806 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201806 = target.201806 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201806, file = "target_201806_1_30.RData")
load("target_201806.RData")
##

##

target.201901  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2019-01") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201901 = target.201901  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201711 = target.201711  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201901 = target.201901 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())
# cut = target.201711 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201901 = spread(target.201901, target, n)
target.201901 = target.201901 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201901 = target.201901 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201901 = target.201901 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201901, file = "target_201901.RData")
load("target_201901.RData")
##

##

target.201902  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2019-02") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201902 = target.201902  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201711 = target.201711  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201711 = target.201711 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201902 = target.201902 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())
# cut = target.201711 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201902 = spread(target.201902, target, n)
target.201902 = target.201902 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201902 = target.201902 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201902 = target.201902 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201902, file = "target_201902.RData")
load("target_201902.RData")
##

target.201812  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2018-12") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201812 = target.201812  %>% mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201812 = target.201812  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201812 = target.201812 %>% group_by(cod_contrato, target) %>% summarise(n = n())
# cut = target.201812 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201812 = spread(target.201812, target, n)
target.201812 = target.201812 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201812 = target.201812 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201812 = target.201812 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201812, file = "target_201812.RData")
load("target_201812.RData")

##

target.201901  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2019-01") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201901 = target.201901  %>% dplyr::mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201812 = target.201812  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201901 = target.201901 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())
# cut = target.201812 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201901 = spread(target.201901, target, n)
target.201901 = target.201901 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201901 = target.201901 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201901 = target.201901 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201901, file = "target_201901.RData")
load("target_201812.RData")

###

target.201901  = bhb.hist.paym %>% filter(format(as.Date(data_pagamento), "%Y-%m") == "2019-04") %>%
  select(cod_contrato, data_pagamento, dias_atrasados)

target.201901 = target.201901  %>% dplyr::mutate(target = ifelse(dias_atrasados > 30, "bad", "good"))
# target.201812 = target.201812  %>% mutate(cut = cut(dias_atrasados, breaks = c(seq(0, 90, by = 30)), include.lowest = FALSE))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados <= 0, "no delay", cut))
# target.201812 = target.201812 %>% mutate(cut = ifelse(dias_atrasados > 90, "90 +", cut))
target.201901 = target.201901 %>% dplyr::group_by(cod_contrato, target) %>% dplyr::summarise(n = n())
# cut = target.201812 %>% arrange(cod_contrato, data_pagamento) %>% 
#                         group_by(cod_contrato, data_pagamento, cut) %>% 
#                         summarise(n = n())

target.201901 = spread(target.201901, target, n)
target.201901 = target.201901 %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
target.201901 = target.201901 %>% mutate(target = ifelse(`bad` != 0, "bad", "good"))

target.201901 = target.201901 %>% select(-c(good, bad))

setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
save(target.201901, file = "target_201901.RData")
load("target_201812.RData")

##

train.test.201611.201710 = left_join(train.test.201611.201710, target.201711)
train.test.201611.201710 = train.test.201611.201710 %>% filter(!is.na(target))

train.test.201611.201710 = train.test.201611.201710 %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                                                         funs(as.Date(as.character(.), 
                                                                                      format = "%d/%m/%Y")))

##

##

valid.201712.201811 = left_join(valid.201712.201811, target.201812)
valid.201712.201811 = valid.201712.201811 %>% filter(!is.na(target))

valid.201712.201811 = valid.201712.201811 %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), 
                                                                         list(~as.Date(as.character(.), 
                                                                                      format = "%d/%m/%Y")))

##
#TRAIN/TEST

# loading column counts (18) from historical payments to create new variables
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("delay_count_by_contr_until_201710.RData")

train.test.201611.201710 = left_join(train.test.201611.201710, count.all) 

train.test.201611.201710$vlr_renda_mensal_cli = ifelse(train.test.201611.201710$vlr_renda_mensal_cli == 1, NA, 
                                                       train.test.201611.201710$vlr_renda_mensal_cli) 

# train.test.201607.201706$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), train.test.201611.201710$data_nascimento), "years"))
train.test.201611.201710$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date(as.character("2017-10-31"), format = "%Y-%m-%d"), train.test.201611.201710$data_contrato), "years"))
train.test.201611.201710$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date(as.character("2017-10-31"), format = "%Y-%m-%d"), train.test.201611.201710$data_contrato), "months"))
train.test.201611.201710$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date(as.character("2017-10-31"), format = "%Y-%m-%d"), train.test.201611.201710$data_ult_pgt), "days"))

train.test.201611.201710$`perc_parc_pagas` = train.test.201611.201710$qtd_parc_pagas / (train.test.201611.201710$qtd_parc_pagas + train.test.201611.201710$qtd_parc_restantes)
train.test.201611.201710$`perc_vnc_finan` = train.test.201611.201710$vlr_vencido / train.test.201611.201710$vlr_total_financiado
train.test.201611.201710$`perc_vnc_renda` = train.test.201611.201710$vlr_vencido / train.test.201611.201710$vlr_renda_mensal
train.test.201611.201710$`perc_vnc_bens` = train.test.201611.201710$vlr_vencido / train.test.201611.201710$vlr_total_bens
train.test.201611.201710$`perc_ult_pgt_parc` = train.test.201611.201710$vlr_ult_pgt / train.test.201611.201710$vlr_parcela
train.test.201611.201710$`perc_a_vencer_finan` = train.test.201611.201710$vlr_a_vencer / train.test.201611.201710$vlr_total_financiado

train.test.201611.201710$`perc_pg_atr_1_10` = train.test.201611.201710$qtd_pg_atr_1_10 / train.test.201611.201710$qtd_pg_atr_12_meses
train.test.201611.201710$`perc_pg_atr_1_15` = train.test.201611.201710$qtd_pg_atr_1_15 / train.test.201611.201710$qtd_pg_atr_12_meses
train.test.201611.201710$`perc_pg_atr_16_30` = train.test.201611.201710$qtd_pg_atr_16_30/ train.test.201611.201710$qtd_pg_atr_12_meses
train.test.201611.201710$`perc_pg_atr_1_60` = train.test.201611.201710$qtd_pg_atr_1_60 / train.test.201611.201710$qtd_pg_atr_12_meses
train.test.201611.201710$`perc_pg_atr_11_60` = (train.test.201611.201710$qtd_pg_atr_11_30 + train.test.201611.201710$qtd_pg_atr_31_60) / (train.test.201611.201710$qtd_pg_atr_12_meses)
train.test.201611.201710$`perc_pg_atr_61_360` = train.test.201611.201710$qtd_pg_atr_61_360 / train.test.201611.201710$qtd_pg_atr_12_meses
train.test.201611.201710$`perc_pg_atr_360_mais` = train.test.201611.201710$qtd_pg_atr_360_mais/ train.test.201611.201710$qtd_pg_atr_12_meses

train.test.201611.201710$`perc_parc_renda` = train.test.201611.201710$vlr_parcela/train.test.201611.201710$vlr_renda_mensal_cli
train.test.201611.201710$`perc_pg_finan` = (train.test.201611.201710$vlr_parcela*train.test.201611.201710$qtd_parc_pagas)/train.test.201611.201710$vlr_total_financiado

#3 above lines are generating -inf values :(
train.test.201611.201710$perc_pg_atr_12_1 = train.test.201611.201710$qtd_pg_atr_12_meses/train.test.201611.201710$qtd_pg_atr_1_mes
train.test.201611.201710$perc_pg_atr_7_1 = train.test.201611.201710$qtd_pg_atr_7_meses/train.test.201611.201710$qtd_pg_atr_1_mes
train.test.201611.201710$perc_pg_atr_4_1 = train.test.201611.201710$qtd_pg_atr_4_meses/train.test.201611.201710$qtd_pg_atr_1_mes

train.test.201611.201710$tempo_ate_primeiro_atr = as.integer(train.test.201611.201710$data_primeiro_atraso - train.test.201611.201710$data_contrato)

train.test.201611.201710 = train.test.201611.201710 %>% mutate_at(vars(cod_hda, qtd_pg_atr_1_mes, vlr_pg_atr_1_mes,      
                                                                  qtd_pg_atr_4_meses, vlr_pg_atr_4_meses, qtd_pg_atr_7_meses, 
                                                                  vlr_pg_atr_7_meses, qtd_pg_atr_12_meses, vlr_pg_atr_12_meses,   
                                                                  qtd_pg_1_mes, valor_pg_1_mes, qtd_pg_4_meses,       
                                                                  valor_pg_4_meses, qtd_pg_7_meses, valor_pg_7_meses,      
                                                                  qtd_pg_12_meses, valor_pg_12_meses, qtd_pg_atr_1_10,
                                                                  vlr_pg_atr_1_10, qtd_pg_atr_1_15, vlr_pg_atr_1_15, 
                                                                  qtd_pg_atr_16_30, vlr_pg_atr_16_30,
                                                                  qtd_pg_atr_1_30, vlr_pg_atr_1_30, 
                                                                  qtd_pg_atr_1_60,qtd_pg_atr_360_mais, vlr_pg_atr_360_mais,
                                                                  vlr_pg_atr_1_60, qtd_pg_atr_11_30, vlr_pg_atr_11_30, 
                                                                  qtd_pg_atr_11_60, qtd_pg_atr_31_60, vlr_pg_atr_31_60, 
                                                                  qtd_pg_atr_61_360, vlr_pg_atr_61_360, qtd_pg_atr_1_360, 
                                                                  vlr_pg_atr_1_360), funs(replace(., is.na(.), 0)))

setwd("~/IGB/Histórico Fechamento/Models/Final databases")
save(train.test.201611.201710, file = "train_test_201611_201710_final.RData")
load("train_test_201611_201710_final.RData")

## VALIDATION

# loading column counts (18) from historical payments to create new variables
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB/All historic")
load("delay_count_by_contr_until_201811.RData")

valid.201712.201811 = left_join(valid.201712.201811, count.all) 

valid.201712.201811$vlr_renda_mensal_cli = ifelse(valid.201712.201811$vlr_renda_mensal_cli == 1, NA, 
                                                       valid.201712.201811$vlr_renda_mensal_cli) 

# valid.201712.201811$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), valid.201712.201811$data_nascimento), "years"))
valid.201712.201811$`tempo_contrato_anos` = lubridate::time_length(difftime(format(as.Date("2018-11-30", "%Y-%m-%d")), valid.201712.201811$data_contrato), "years")
valid.201712.201811$`tempo_contrato_meses` = lubridate::time_length(difftime(format(as.Date("2018-11-30", "%Y-%m-%d")), valid.201712.201811$data_contrato), "months")
valid.201712.201811$`tempo_desde_ult_pgt` = lubridate::time_length(difftime(format(as.Date("2018-11-30", "%Y-%m-%d")), valid.201712.201811$data_ult_pgt), "days")

valid.201712.201811$`perc_parc_pagas` = valid.201712.201811$qtd_parc_pagas / (valid.201712.201811$qtd_parc_pagas + valid.201712.201811$qtd_parc_restantes)
valid.201712.201811$`perc_vnc_finan` = valid.201712.201811$vlr_vencido / valid.201712.201811$vlr_total_financiado
valid.201712.201811$`perc_vnc_renda` = valid.201712.201811$vlr_vencido / valid.201712.201811$vlr_renda_mensal
valid.201712.201811$`perc_vnc_bens` = valid.201712.201811$vlr_vencido / valid.201712.201811$vlr_total_bens
valid.201712.201811$`perc_ult_pgt_parc` = valid.201712.201811$vlr_ult_pgt / valid.201712.201811$vlr_parcela
valid.201712.201811$`perc_a_vencer_finan` = valid.201712.201811$vlr_a_vencer / valid.201712.201811$vlr_total_financiado

valid.201712.201811$`perc_pg_atr_1_10` = valid.201712.201811$qtd_pg_atr_1_10 / valid.201712.201811$qtd_pg_atr_12_meses
valid.201712.201811$`perc_pg_atr_1_15` = valid.201712.201811$qtd_pg_atr_1_15 / valid.201712.201811$qtd_pg_atr_12_meses
valid.201712.201811$`perc_pg_atr_16_30` = valid.201712.201811$qtd_pg_atr_16_30/ valid.201712.201811$qtd_pg_atr_12_meses
valid.201712.201811$`perc_pg_atr_1_60` = valid.201712.201811$qtd_pg_atr_1_60 / valid.201712.201811$qtd_pg_atr_12_meses
valid.201712.201811$`perc_pg_atr_11_60` = (valid.201712.201811$qtd_pg_atr_11_30 + valid.201712.201811$qtd_pg_atr_31_60) / (valid.201712.201811$qtd_pg_atr_12_meses)
valid.201712.201811$`perc_pg_atr_61_360` = valid.201712.201811$qtd_pg_atr_61_360 / valid.201712.201811$qtd_pg_atr_12_meses
valid.201712.201811$`perc_pg_atr_360_mais` = valid.201712.201811$qtd_pg_atr_360_mais/ valid.201712.201811$qtd_pg_atr_12_meses

valid.201712.201811$`perc_parc_renda` = valid.201712.201811$vlr_parcela/valid.201712.201811$vlr_renda_mensal_cli
valid.201712.201811$`perc_pg_finan` = (valid.201712.201811$vlr_parcela*valid.201712.201811$qtd_parc_pagas)/valid.201712.201811$vlr_total_financiado

#3 above lines are generating -inf values :(
valid.201712.201811$perc_pg_atr_12_1 = valid.201712.201811$qtd_pg_atr_12_meses/valid.201712.201811$qtd_pg_atr_1_mes
valid.201712.201811$perc_pg_atr_7_1 = valid.201712.201811$qtd_pg_atr_7_meses/valid.201712.201811$qtd_pg_atr_1_mes
valid.201712.201811$perc_pg_atr_4_1 = valid.201712.201811$qtd_pg_atr_4_meses/valid.201712.201811$qtd_pg_atr_1_mes

valid.201712.201811$tempo_ate_primeiro_atr = as.integer(valid.201712.201811$data_primeiro_atraso - valid.201712.201811$data_contrato)

valid.201712.201811 = valid.201712.201811 %>% mutate_at(vars(cod_hda, qtd_pg_atr_1_mes, vlr_pg_atr_1_mes,      
                                                                       qtd_pg_atr_4_meses, vlr_pg_atr_4_meses, qtd_pg_atr_7_meses, 
                                                                       vlr_pg_atr_7_meses, qtd_pg_atr_12_meses, vlr_pg_atr_12_meses,   
                                                                       qtd_pg_1_mes, valor_pg_1_mes, qtd_pg_4_meses,       
                                                                       valor_pg_4_meses, qtd_pg_7_meses, valor_pg_7_meses,      
                                                                       qtd_pg_12_meses, valor_pg_12_meses, qtd_pg_atr_1_10,
                                                                       vlr_pg_atr_1_10, qtd_pg_atr_1_30, vlr_pg_atr_1_30, 
                                                                       qtd_pg_atr_1_60,qtd_pg_atr_360_mais, vlr_pg_atr_360_mais,
                                                                       vlr_pg_atr_1_60, qtd_pg_atr_11_30, vlr_pg_atr_11_30,
                                                                       qtd_pg_atr_1_15,qtd_pg_atr_1_15, vlr_pg_atr_16_30,
                                                                       vlr_pg_atr_16_30, qtd_pg_atr_11_60, qtd_pg_atr_31_60, 
                                                                       vlr_pg_atr_31_60, 
                                                                       qtd_pg_atr_61_360, vlr_pg_atr_61_360, qtd_pg_atr_1_360, 
                                                                       vlr_pg_atr_1_360), funs(replace(., is.na(.), 0)))

setwd("~/IGB/Histórico Fechamento/Models/Final databases")
save(valid.201712.201811, file = "valid_201712_201811_final.RData")
load("valid_201712_201811_final.RData")

##

db_to_predict = train.test.201611.201710 %>% select(-c(cpf_cnpj, data_safra, 
                                                       data_contrato, data_primeiro_atraso, 
                                                       data_ult_pgt, data_vencimento, status_contrato, 
                                                       num_chassi, tabela, assessoria, assessoria_cob, 
                                                       assessoria_nova, cod_hda))

db_to_predict = valid.201712.201811 %>% select(-c(cpf_cnpj, data_safra, 
                                                       data_contrato, data_primeiro_atraso, 
                                                       data_ult_pgt, data_vencimento, status_contrato, 
                                                       num_chassi, tabela, assessoria, assessoria_cob, 
                                                       cod_hda))

segment = "MOT"
segment = "CAR"
# db_to_predict = train.test.201607.201706 %>% filter(score %in% c("Classe 1", "Classe 1+", "Classe 2", "Classe 2+", "Classe 3", "Classe 3-", "Classe 4", "Classe 4-"))

# require(naniar)
# vis_miss(train.test.201607.201706, warn_large_data = FALSE)

preparing <- function(db_to_predict, segment, sampling){
  
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
  
  # classes = sapply(db, class)
  # 
  # db = db %>% mutate_all(as.factor)
  # 
  # # function that select factor columns and make all levels of them unique 
  # # (because of error )
  # indx <- sapply(db, is.factor)
  # db[indx] <- lapply(db[indx], function(x) {
  #   levels(x) <- make.unique(levels(x))
  #   x })
  # 
  # db = droplevels(db)
  
  # missing data imputation with 5 nearest neighbourhoods
  # preproc = preProcess(db, method = "knnImpute", k = 5)
  # preproc$method$center = NULL
  # preproc$method$scale = NULL
  # db <- predict(preproc, db)
  
  # db$target = as.factor(db$target)
  
  set.seed(123)
  index <- caret::createDataPartition(db$target, p = 0.7, list = FALSE)
  train_db <- db[index, ]
  test_db  <- db[-index, ]
  
  perc = table(train_db$target)[2]/(table(train_db$target)[1]+table(train_db$target)[2])
  
  bins_mod <<- scorecard::woebin(train_db, y = "target")
  # setting to null because these columns are duplicating levels
  bins_mod$perc_parc_renda = NULL
  bins_mod$perc_vnc_renda = NULL
  
  # setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model ,"/Databases"))
  #save(bins_mod, file = paste0("bins_", segment,"_", Sys.Date(), ".RData"))
  # 
  # setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model,"/Plots"))
  # plotlist = scorecard::woebin_plot(bins_mod)
  
  # save binning plot
  # for (i in 1:length(plotlist)){
  #   ggplot2::ggsave(
  #     paste0(segment, "_", desired_model, "_", 
  #            names(plotlist[i]),"_",Sys.Date(),".png"), plotlist[[i]],
  #     width = 15, height = 9, units="cm")
  # }
  
  train_db_woe = as.data.frame(woebin_ply(train_db, bins_mod))
  test_db_woe = as.data.frame(woebin_ply(test_db, bins_mod))
  
  train_db_woe$perc_parc_renda = NULL
  train_db_woe$perc_vnc_renda = NULL
  
  test_db_woe$perc_parc_renda = NULL
  test_db_woe$perc_vnc_renda = NULL
  
  train_db_woe$target = as.factor(train_db_woe$target)
  test_db_woe$target = as.factor(test_db_woe$target)
  
  registerDoSEQ()
  
  zerovar = nearZeroVar(train_db_woe, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
  train_db_woe = train_db_woe[,-zerovar]
  test_db_woe = test_db_woe[,-zerovar]
  
  # imputing missing data again because in test_woe there are some classes 
  # of the variables that did not existed when producing bins from train dataset
  preproc = preProcess(test_db_woe, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  test_db_woe <- predict(preproc, test_db_woe)

#   if(sampling == "ROSE"){train_db_woe = ROSE(target ~ ., data = train_db_woe, seed = 3)$data} else
#   {if(sampling == "SMOTE"){train_db_woe = SMOTE(target ~ ., data = train_db_woe, seed = 3, perc.over = 100,  k = 5)} else 
#                           {train_db_woe = ovun.sample(target ~ ., data = train_db_woe, method = "over", N = max(table(train_db_woe$target))*2)$data}}
  
  dbs <<- list("train_db_woe" = train_db_woe, 
               "test_db_woe" = test_db_woe, 
               "train_db" = train_db, 
               "test_db" = test_db)
  
  perc = table(dbs$train_db_woe$target)[1]/(table(dbs$train_db_woe$target)[1]+table(dbs$train_db_woe$target)[2])
#  paste0("The percentage of bad payers after ", sampling, " is: ", round(perc, 4), ". ") %>% cat()
}

preparing(db_to_predict, segment, sampling)

setwd("~/IGB/Histórico Fechamento/Models/Final databases")
save(dbs, file = "dbs_MOT_1month.RData")
load("dbs_MOT_1month.RData")

# Function to develop models and release outputs
modelling <- function(dbs, segment){
  
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
  
  segment <<- segment
  
  "Starting step 1: processing, binning and splitting data.\n\n" %>% cat()
  # Preparing all binning database automatically. Arguments: database, model, segment.
  preparing(db_to_predict, segment)
  
  "Step 1: data is already processed, binned and splitted.\n" %>% cat()
  
  # Preparing modelling matrixes
  X <- model.matrix(target ~ ., dbs$train_db_woe)[,-1]
  y <- dbs$train_db_woe$target
  # weights = dbs$train_db_woe$model_weights
  
  X.test <- model.matrix(target ~ ., dbs$test_db_woe)[,-1]
  
  # column numbers which are to be forced entering in the model
  # pf = which(names(dbs$train_db_woe)[-1] %in% c("perc_parc_renda_woe", "perc_pg_finan_woe", "perc_entrada_financ_woe", "prazo_contrato_woe"))
  # penalty.factor=c(rep(1, ncol(dbs$train_db_woe)-1))
  
  # change `penalty factor` to 0 of variables forced to enter the model
  # for(i in 1:length(pf)){
  #   penalty.factor[pf[i]] = 0}
  
  "Starting step 2: cross-validation to find best lambda.\n\n" %>% cat()
  registerDoSEQ() #library to "unregister" a foreach backend, registering the sequential backend
  # Finding best lambda using cross-validation
  set.seed(123)
  cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial",  type.measure = "auc", parallel = TRUE)#, penalty.factor = penalty.factor)
  "Step 2: cross-validation to find best lambda done.\n\n" %>% cat()
  
  #filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Plots/Lambda_", segment, "_", Sys.Date(), ".tiff")
  #tiff(filename, units="in", width=12, height=8, res=500)
  plot(cv.lasso)#, main = paste0(desired_model, " | ", segment, " | ", Sys.Date()))
  #dev.off()
  
  tmp_coeffs <- coef(cv.lasso, s = "lambda.1se")
  variables = nrow(data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x))
  
  "Starting step 3: running LASSO to select features.\n\n" %>% cat()
  # Creating model with lambda 1se or "pushing up" the cut off (lambda 1se)
  lambda = ifelse(variables <= 20, cv.lasso$lambda.1se, 
                  cv.lasso$lambda.min + 10*(cv.lasso$lambda.1se - cv.lasso$lambda.min))
  
  lasso.model <- glmnet(X, y, family = "binomial", lambda = lambda, alpha = 1)#, penalty.factor = penalty.factor)
  "Step 3: LASSO model done.\n" %>% cat()
  
  coefs <- tidy(lasso.model)
  coefs <- coefs[-1,]
  selected_var_woe = paste(coefs$term, sep = "\n"); selected_var_woe <<- c(selected_var_woe, "target")
  selected_var = str_sub(coefs$term, end = -5); selected_var <<- c(selected_var, "target")

  "Starting step 4: predictions and model performance measures.\n\n" %>% cat()
  # Predicting model in test database
  probabilities <- predict(lasso.model, newx = X.test, type="response", s = lambda)
  
  # Checking probabilities x target in test database
  pred <- prediction(probabilities, dbs$test_db_woe$target)

  acc.perf = ROCR::performance(pred, "sens", "spec")
  
  plot(acc.perf)
  
  # Find probabilities cutoff with greater sum of specificity and sensitivity
  cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]
  
  predicted.classes <- ifelse(probabilities > cutoff, "goodpayer", "badpayer")
  observed.classes <- ifelse(dbs$test_db_woe$target == "bad", "badpayer", "goodpayer")
  
  observed.classes.num <- ifelse(observed.classes == "badpayer", 1, 0)
  predicted.classes.num <- ifelse(predicted.classes == "badpayer", 1, 0)
  
  # Calculating KS, auc, gini
  evaluation <- scorecard::perf_eva(label = observed.classes.num, pred = c(probabilities), threshold = cutoff,show_plot = TRUE)
  evaluation$confusion_matrix
  df <- melt(data.frame(evaluation$binomial_metric$dat))
  names(df) <- c("Statistic", "Value")
  
  # Calculating features importance
  imp = caret::varImp(lasso.model, lambda = lambda) %>% tibble::rownames_to_column("variable") %>% arrange(desc(Overall)) %>% dplyr::rename("importance" = "Overall") %>% filter(!importance == 0)
  
  imp$variable = str_sub(imp$variable, end = -5)
  x = bind_rows(bins_mod)
  x = left_join(x, imp)
  x <<- x %>% filter(!is.na(importance))
  
  # Creating dataframe with probabilities
  probs_by_contract <<- data.frame(select(dbs$test_db, selected_var), bad_payer_prob = 1 - c(probabilities))#, predicted_target = predicted.classes)

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
  writing(lasso.model, selected_var, probs_by_contract, bins_mod, imp, segment, perf_stats1, perrf_stats2, x)

}

modelling(dbs, segment)

writing <- function(lasso.model, selected_var, probs_by_contract, bins_mod, imp, segment, perf_stats1, perf_stats2, x){
  
  require(data.table)
  require(stringr)
  
  # Automatically creating files for desired outputs
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/",min,"-",max,"/",segment))
  save(lasso.model, file = paste0("lasso_model_", segment,"_", Sys.Date(),".RData"))
  save(bins_mod, file = paste0("bins_", segment,"_", Sys.Date(),".RData"))
  save(selected_var, file = paste0("selected_var_", segment,"_", Sys.Date(),".RData"))
  save(probs_by_contract, file = paste0("probs_by_contract_", segment,"_", Sys.Date(),".RData"))
  fwrite(probs_by_contract, file = paste0("lasso_model_predictions_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(imp, file = paste0("var_importance_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats1, file = paste0("performance_stats_1_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats2, file = paste0("performance_stats_2_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(x, file = paste0("bins_splits_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")

}

writing(lasso.model, selected_var, probs_by_contract, bins_mod, imp, segment, perf_stats1, perf_stats2, x)

#lasso.model, bins_mod

validation = function(base_desired_model, segment){
  
  require(stringr)
  require(dplyr)
  require(broom)
  require(scorecard)
  require(caret)
  require(foreach)
  require(glmnet)
  require(gridExtra)
  require(tibble)
  require(reshape2)
  require(plyr)
  require(ROCR)
  require(mice)
  
  db_to_predict = db_to_predict %>% filter(segmento == segment) #%>% select(-cod_contrato)
  db_to_predict = db_to_predict %>% select(-c(starts_with("data_")))
  
  # db_to_predict = db_to_predict %>% mutate_all(as.factor)
  
  # # function that select factor columns and make all levels of them unique 
  # # (because of error )
  # indx <- sapply(db_to_predict, is.factor)
  # db_to_predict[indx] <- lapply(db_to_predict[indx], function(x) {
  #   levels(x) <- make.unique(levels(x))
  #   x })
  # 
  # db_to_predict = droplevels(db_to_predict)
  
  bins_mod$perc_parc_renda = NULL
  bins_mod$perc_vnc_renda = NULL
  
  #WOE
  db_woe = woebin_ply(db_to_predict, bins_mod)
  # db_woe = db_woe %>% mutate_all(.,as.numeric)
  
  # x = woebin_plot(bins_mod)
  
  # db_woe = db_woe[1:nrow(db_to_predict),]
  db_woe$score_woe = db_woe$perc_vnc_renda_woe = db_woe$perc_parc_renda_woe =  0
  db_to_predict = db_woe %>% select(lasso.model[["beta"]]@Dimnames[[1]])
  
  registerDoSEQ()
  preproc = preProcess(db_to_predict, method = "knnImpute", k = 5)
  preproc$method$center = NULL
  preproc$method$scale = NULL
  db_to_predict = predict(preproc, db_to_predict)
  
  # db_to_predict = mice(db_to_predict, m=3, seed = 123)
  # db = complete(db_to_predict, 1)
  
  db_to_predict_matrix = data.matrix(db_to_predict)
  
  "Starting step 4: Predictions and model performance measures.\n\n" %>% cat()
  # Predicting model in test database
  # probabilities <- lasso.model %>% predict(db_to_predict_matrix, type="response")
  probabilities = predict(lasso.model, newx = db_to_predict_matrix, type="response")
  
  db_to_predict = train.test %>% select(-c(cpf_cnpj, data_safra, 
                                           data_contrato, data_primeiro_atraso, 
                                           data_ult_pgt, data_vencimento, status_contrato, 
                                           num_chassi, tabela, assessoria, assessoria_cob, 
                                           cod_hda, Cod)) %>% filter(segmento == segment)
  
  # Checking probabilities x target in test database
  pred <- prediction(probabilities, db_to_predict$target)
  acc.perf = performance(pred, "sens", "spec")
  plot(acc.perf)
  
  # Find probabilities cutoff with greater sum of specificity and sensitivity
  cutoff <<- acc.perf@alpha.values[[1]][which.max(acc.perf@x.values[[1]]+acc.perf@y.values[[1]])]
  
  predicted.classes <- c(ifelse(probabilities < cutoff, "good", "bad"))
  observed.classes <- db_woe$target
  
  # Calculating KS, auc, gini
  evaluation <- scorecard::perf_eva(label = observed.classes, pred = probabilities, threshold = cutoff, show_plot = TRUE)
  df <- melt(data.frame(evaluation$binomial_metric$dat))
  names(df) <- c("statistic", "value")
  
  selected_var = coefs <- tidy(lasso.model)
  coefs <- coefs[-1,]
  selected_var_woe = paste(coefs$term, sep = "\n"); selected_var_woe <<- c(selected_var_woe, "target")
  selected_var = str_sub(coefs$term, end = -5); selected_var <<- c("cod_contrato", selected_var, "target")

    # Creating dataframe with probabilities
  
  db_to_predict = train.test %>% select(-c(cpf_cnpj, data_safra, 
                                           data_contrato, data_primeiro_atraso, 
                                           data_ult_pgt, data_vencimento, status_contrato, 
                                           num_chassi, tabela, assessoria, assessoria_cob, 
                                           cod_hda, Cod))%>% filter(segmento == segment)
  
  paste0("The cutoff probability point that produces greater sensibility+specificity: ", round(cutoff,4), "\n\n") %>% cat()
  
  conf.matrix = caret::confusionMatrix(as.factor(observed.classes), as.factor(predicted.classes))
  
  "######################################################\n\n" %>% cat()
  print(evaluation)
  "######################################################\n\n" %>% cat()
  print(conf.matrix)
  "######################################################\n\n" %>% cat()
  

  ##### CREATION OF CLUSTERS, SCORE AND PLOTS
  
  db_to_predict$target = as.factor(db_to_predict$target)
  
  points0 = 600
  odds0 = 50
  pdo = 20
  
  pred = predict(lasso.model, newx = db_to_predict_matrix)
  resp = predict(lasso.model, newx = db_to_predict_matrix, type = "response")
  
  factor = pdo/log(2)
  offset = points0 - factor * log(odds0)
  
  # Creating dataframe with probabilities
  probs_by_contract <<- data.frame(select(db_to_predict, c("cod_contrato", selected_var)), bad_payer_prob = probabilities)#, predicted_target = predicted.classes)
  
  final_db = probs_by_contract %>% mutate(logit = c(pred),
                                      odds = c(exp(pred)),
                                      prob_good = 1 - (odds/(odds + 1)),
                                      prob_bad =  (odds/(odds + 1)),
                                      prob_ctrl = c(resp))
  
  final_db$score = round(final_db$prob_good * 1000,0)
  
  score_metrics = c("logit", "odds", "prob_good", "prob_bad", "prob_ctrl", "score_ctrl", "score")
  #########################
  
  final_db %>% 
    group_by(target) %>% 
    summarise(tb = mean(score)) %>% 
    ungroup() -> mean_score_db_to_predict
  
  final_db$target = as.factor(final_db$target)

  median_score_db_to_predict <- final_db %>%
    dplyr::group_by(target) %>%
    dplyr::summarise(tb = median(score, na.rm = TRUE))
  
  p1 <- final_db %>% 
    ggplot(aes(score, color = factor(target), fill = factor(target))) + 
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = median_score_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
    geom_vline(aes(xintercept = median_score_db_to_predict$tb[2]), linetype = "dashed", color = "green4") +
    scale_color_manual(values=c("brown2", "green4")) +
    scale_fill_manual(values=c("brown2", "green4")) +
    guides(fill = "legend", colour = "none") +
    theme(legend.position="bottom") +
    labs(x = "collection score", y = "density", fill = "original target", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for validation data [observation: 201807 - 201812 / target: 201901]", 
         subtitle = "The scorecard point is a numeric expression measuring collectionworthiness. \nCommercial Banks usually utilize it as a method to support the decision-making about collection actions.")
  
  set.seed(123)
  centers <- stats::kmeans(final_db$score, centers = 4, iter.max = 500, nstart = 100)$centers
  centers <- sort(centers)
  set.seed(123)
  clusters <- kmeans(final_db$score, centers = centers, iter.max = 500, nstart = 100)
  final_db$cluster = as.factor(clusters$cluster)
  
  final_db$cluster = revalue(final_db$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  
  median_cluster_db_to_predict <- final_db %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::summarise(tb = median(score, na.rm = TRUE)) 
  
  p2 <- final_db %>% 
    ggplot(aes(score, color = factor(cluster), fill = factor(cluster))) + 
    geom_density(alpha = 0.3) + 
    geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[1]), linetype = "dashed", color = "brown2") +
    geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[2]), linetype = "dashed", color = "darkorange1") +
    geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[3]), linetype = "dashed", color = "yellow2") +
    geom_vline(aes(xintercept = median_cluster_db_to_predict$tb[4]), linetype = "dashed", color = "green4") +
    guides(fill = "legend", colour = "none") +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    theme(legend.position="bottom") +
    labs(x = "collection score", y = "density", fill = "cluster", color = "none", title = "Figure 2: Scorecard Distribution by four collection clusters for validation data [observation: 201807 - 201812 / target: 201901]",
         caption = paste0("Based on data from IGB: ", segment, " | ", "version: ", Sys.Date()))
  
  both_plot <- grid.arrange(p1, p2, ncol = 1)
  
  db_to_predict = final_db %>% select(c(cod_contrato, selected_var[!selected_var %in% "target"], cluster, score))
  db_to_predict$cluster = as.numeric(db_to_predict$cluster)
  geom.mean = db_to_predict %>% mutate_if(is.numeric, funs(`+1` = .+1))
  x = geom.mean %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(psych::geometric.mean) %>% select(ends_with("+1"))
  x = x %>% mutate_if(is.numeric, funs(`-1` = .-1)) %>% select(ends_with("-1")) %>% arrange(`score_+1_-1`)
  
  db_to_predict = final_db %>% select(c(cod_contrato, selected_var, cluster, score))
  db_to_predict$cluster = as.numeric(db_to_predict$cluster)
  x = db_to_predict %>% dplyr::select_if(is.numeric) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(median, na.rm = TRUE)

  x$cluster = as.factor(x$cluster)
  x$cluster = revalue(x$cluster, c("1"="very high risk", "2"="high risk", "3"="low risk", "4"="very low risk"))
  
  df2 <- melt(x,  id.vars = c("score","cluster"), variable.name = "variable")
  cluster_prob <- ggplot(df2, aes(`score`, value)) + geom_line() + 
    facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") + 
    geom_point(df2, mapping = aes(`score`, value, color=factor(cluster), size = 5)) +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    theme(legend.position="bottom") +
    labs(title=paste0("Collection model clusters | ", segment), x="collection score", y="geometric mean of selected variable", col = "cluster",
         caption = paste0("Based on data from IGB: ", segment, " | ", "daily result: ", "2019-02-28")) +
    guides(size = "none", colour = "legend")
  
  db_to_predict = db_to_predict %>% select_if(is.numeric )
  teste = db_to_predict %>% #select_if(is.numeric) %>% 
    tidyr::gather("id", "value", -c(score, cluster)) %>% 
    ggplot(., aes(score, value, colour = factor(cluster))) + 
    geom_boxplot() + facet_wrap(~id, scales = "free") +
    theme(legend.position="bottom") +
    scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
    labs(title=paste0("Collection model clusters | ", segment), x="collection score", y="geometric mean of selected variable", col = "cluster",
         caption = paste0("Based on data from IGB: ", segment, " | ", "daily result: ", "2019-02-28"))
  
  #####################################
  
   filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/",desired_model,"/Daily Predictions/clusters_distribution_daily_",desired_model,"_",segment,"_","2019-02-28",".tiff")
  tiff(filename, units="in", width=12, height=8, res=500)
  print(cluster_prob)
  dev.off()
  
  
  conf_db = as.data.frame(as.matrix(conf.matrix, what = "overall")); conf_db = rownames_to_column(conf_db, "statistic")
  conf_db2 = as.data.frame(as.matrix(conf.matrix, what = "classes")); conf_db2 = rownames_to_column(conf_db2, "statistic")
  conf = bind_rows(conf_db, conf_db2); colnames(conf) <- c("statistic", "value"); conf$value = round(conf$value, 4)
  perf_stats1 = bind_rows(conf, df); perf_stats1 = perf_stats1 %>% mutate(stat_date = Sys.Date(), segment = segment)
  perf_stats2 = as.data.frame(conf.matrix$table); perf_stats2 = perf_stats2 %>% dplyr::rename("prediction" = "Prediction",
                                                                                              "reference" = "Reference",
                                                                                              "freq" = "Freq"); perf_stats2 = perf_stats2 %>% mutate(stat_date = Sys.Date(), segment = segment)
  
  #creating final database (new contracts)
  db_to_predict = final_db %>% select(cod_contrato, qtd_dias_em_atraso, prob_bad, prob_good, score, cluster) %>% mutate(stat_model = paste0(segment),
                                                                                                                        stat_model_update = Sys.Date())
  writing_valid(db_to_predict, segment, perf_stats1, perf_stats2, both_plot)
}

###

writing_valid <- function(db_to_predict, segment, perf_stats1, perf_stats2, both_plot){
  
  require(data.table)
  require(stringr)
  
  # Automatically creating files for desired outputs
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/",segment))
  save(db_to_predict, file = paste0("probs_by_contract_valid_", segment,"_", Sys.Date(),".RData"))
  fwrite(db_to_predict, file = paste0("lasso_model_predictions_valid_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats1, file = paste0("performance_stats_1_valid_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  fwrite(perf_stats2, file = paste0("performance_stats_2_valid_", segment,"_", Sys.Date(),".csv"), sep = ";", dec = ",")
  # plotting both plots together
  ggsave(paste0("score_dist_desired_model_valid", "_", segment,"_", Sys.Date(),".tiff"), both_plot, units="in", width=12, height=8, 
         path = paste0("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/",segment))
}

clusters = final_db %>% dplyr::group_by(cluster, target) %>% dplyr::summarise(n = n())
clusters = spread(clusters, target, n)
clusters = clusters %>% mutate(badrate = bad/(bad+good))

fwrite(clusters, file = "clusters.csv", sep = ";", dec = ",")
