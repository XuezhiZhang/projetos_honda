# algorithm to create overdue days

# data último status (ANO & MÊS) + data assembléia base (DIA)
out.all = out.all %>% mutate(DTNOVA = paste0(substr(DTSTCONTRATO, 1, 7), substr(DTULASSPRAZO, 8, 10), collapse = "-"))

out.all$DTNOVA
