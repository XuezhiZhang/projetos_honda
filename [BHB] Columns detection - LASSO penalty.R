perc_parc_renda
perc_pg_finan
perc_entrada_financ

# column numbers which are to be forced entering in the model
pf = which(names(mod_11_60) %in% c("perc_parc_renda", "perc_pg_finan", "perc_entrada_financ"))
penalty.factor=c(rep(1, ncol(mod_11_60)))

# change to 0 penalty factor of variables forced to enter the model
for(i in 1:length(pf)){
penalty.factor[pf] = 0}

