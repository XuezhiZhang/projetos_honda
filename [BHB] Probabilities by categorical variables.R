#BEST PLOT (BIN & MEAN OF DELINQUENCY INDEX)
 
probs_by_contract = probs_by_contract %>% filter(idade_cli <= 100) 

ggplot(probs_by_contract, aes(idade_cli, qtd_dias_em_atraso)) + stat_summary_2d(aes(z = bad_payer_prob), bins = 30, fun = mean) + 
  scale_fill_gradientn(limits = range(idade_bens$delinquency_index), colours = c("green", "red"), guide = guide_colourbar(title = "delinquency index")) +
  facet_wrap(segmento ~ .)

ggplot(probs_by_contract, aes(idade_cli, )) 
  
teste = probs_by_contract %>% select_if(is.character)
  teste = teste %>% mutate_if(is.character, factor) %>%
  select_if(~ nlevels(.) < 28)

  p <- ggplot(data = probs_by_contract, aes(x=reorder(nome_estado_cli, bad_payer_prob, FUN = median), y=bad_payer_prob)) + 
  geom_boxplot() + labs(title = "[11-60 | 2W] Boxplot - bad payer probability by state",
                        x = "state", y = "bad payer probability")
  
  q <- ggplot(data = probs_by_contract, aes(x=reorder(estado_civil_cli, bad_payer_prob, FUN = median), y=bad_payer_prob)) + 
    geom_boxplot() + labs(title = "[11-60 | 2W] Boxplot - bad payer probability by civil status",
                          x = "civil status", y = "bad payer probability")
  
  l <- ggplot(data = probs_by_contract, aes(x=reorder(rating_risco_contabil, bad_payer_prob, FUN = median), y=bad_payer_prob)) + 
    geom_boxplot() + labs(title = "[11-60 | 2W] Boxplot - bad payer probability by accounting risk",
                          x = "civil status", y = "bad payer probability")
  m <- ggplot(data = probs_by_contract, aes(x=reorder(profissao_cli, bad_payer_prob, FUN = median), y=bad_payer_prob)) + 
    geom_boxplot() + labs(title = "[11-60 | 2W] Boxplot - bad payer probability by occupation",
                          x = "occupation", y = "bad payer probability") + coord_flip() + theme(axis.text.y = element_text(size=5))
  
  ggplot(data=t, aes_string(x = "w", y = i)) + geom_line() 
p 
q
l
m

inter <- paste0("interaction(", paste0('"', teste, '"', collapse = ", "), ")")

var_character = probs_by_contract %>% select_if(is.character)
var_character = var_character %>% mutate_if(is.character, factor) %>%
                select_if(~ nlevels(.) < 28)

nm <- names(var_character)
for(i in seq_along(nm)){
print(ggplot(probs_by_contract, aes_string(x=paste0("reorder(", nm[i],", bad_payer_prob, FUN = median)"), y="bad_payer_prob")) +
  geom_boxplot())}


  
  
  
  ggplot(top10, aes_string(x=colnames(top10)[num1],y=meanFeat, 
                           fill=colnames(top10)[num1])) +  geom_bar(stat="identity")
  
  ggplot(top10, aes_string(x=paste0("reorder(",colnames(top10)[num1]"),y=meanFeat,
                           fill=colnames(top10)[num1])) +  geom_bar(stat="identity")