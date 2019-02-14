# creating an unique database for all scored contracts
x = list()

for(i in 1:length(desired_model)){
  setwd(paste0("D:/Users/sb044936/Desktop/Modelling databases R/", desired_model[i], "/Daily Predictions"))
  files = list.files(pattern="performance_stats_daily_1.*csv")
  x[[i]] = bind_rows(lapply(files, fread, dec = ",", header = TRUE, colClasses = c("stat_date" = "Date")))
}

all_contracts_scored = bind_rows(x)

x = all_contracts_scored %>% filter(statistic %in% c("Accuracy", "Sensitivity", "Specificity","AUC","Kappa", "KS")) %>%
                         ggplot(., aes(x = stat_date, y = value, group = statistic, colour = statistic, shape = statistic)) + 
                         geom_point() + geom_line(size = 1) + facet_wrap(.~model + segment) +
                         labs(title = "Collection models | daily performance", x="date", y="value", col = "statistic",
                         caption = "Based on data from IGB") +
                         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))

x

