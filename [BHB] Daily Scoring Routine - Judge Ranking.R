require(dplyr)
require(data.table)
# options(warn=-1) # turn off warnings

# defining global parameters
segment = c("CAR", "MOT")
desired_model = c("11_60","61_360")

# reading all functions for daily scoring
setwd("R:/Estatística/BHB/R Scripts")
if(!exists("foo", mode="function")) source("[BHB] Daily Scoring Functions.R", encoding = "UTF-8") 

# run if there were changes in historical payments database (addition of new base)
# setwd("R:/Estatística/BHB/R Scripts")
# if(!exists("foo", mode="function")) source("[BHB] Database Treatment - Historical Payments.R") 

# run if there are no changes in historical database (jan¹19 - dec'19)
setwd("R:/Estatística/BHB/Databases BHB/Historical Payments BHB")
load("delay_count_by_contr.RData")

# reading daily database
setwd("~/IGB/Daily IGB")
database = fread("igb_daily_01_03.txt", header = TRUE, 
                 dec = ",", check.names = TRUE, 
                 colClasses = c("Contrato" = "character",
                                "Cep" = "character",
                                "Cep l" = "character",
                                "CPF CNPJ" = "character"))

# calling formatting database function: standardize database and create ~50 new variables for modelling
formatting_database(database)

# calling target creation function: create target for database formatted: ~50% bads and ~50% goods
target_creation(bhb.final)

# calling desired models / databases / variables
  for(j in 1:length(segment)){
    
    setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model[i] ,"/Models"))
    load(paste0("lasso_model_61_360", "_", segment[j], "_FINAL.RData"))
    setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model[i] ,"/Databases"))
    load(paste0("bins_mod_61_360", "_", segment[j], ".RData"))
    setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model[i] ,"/Predictions"))
    load(paste0("selected_var_61_360", "_", segment[j],"_FINAL.RData"))
    
    paste0("Starting judge daily modelling process for 61_360 | ", segment[j], ".\n") %>% cat()
    
    modelling(mod_11_60, "61_360", segment[j])
    
    paste0("Judge daily modelling process for 61_360 | ", segment[j], " done.\n") %>% cat()
    "######################################################\n\n" %>% cat()
  }

# creating an unique database for all scored contracts
x = list()

for(i in 1:length(desired_model)){
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model[i], "/Daily Predictions"))
  files = list.files(pattern="score_by_contract.*csv")
  x[[i]] = bind_rows(lapply(files, fread, colClasses = c(cod_contrato = "character", stat_model_update = "as.Date"), dec = ",", header = TRUE))
}

all_contracts_scored = bind_rows(x) %>% arrange(desc(stat_model_update)) %>% group_by(cod_contrato) %>% top_n(n = 1, wt = stat_model_update)

# options(warn=0) #turn warnings back on