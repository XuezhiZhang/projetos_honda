
require(tidyr)
db_to_predict$dec <- cut(db_to_predict$score, breaks = seq(-2000, 2000, by = 20))
#labels = 1:10, include.lowest = TRUE)

z = db_to_predict %>% group_by(dec, target) %>% summarise(n = n()) %>% ungroup() %>%
  spread(target, n, fill=0)

z$bad_prop = z$`1`/(z$`1`+z$`0`)

s = db_to_predict %>% group_by(cluster, target) %>% summarise(n = n()) %>% ungroup() %>%
  spread(target, n, fill=0)
s$bad_prop = s$`bad payer`/(s$`bad payer`+s$`good payer`)

#############################################
#plot division of clusters by bad probability
#############################################

teste = db_to_predict %>% group_by(score, target, cluster) %>% summarise(n = n()) %>% ungroup() %>%
  spread(target, n, fill=0)

teste$bad_prop = teste$`1`/(teste$`1`+teste$`0`)

teste %>% ggplot(aes(score, bad_prop, color = factor(cluster), fill = factor(cluster))) + geom_point() +
  scale_color_manual(values=c("brown2", "darkorange1", "yellow2", "green4")) +
  scale_fill_manual(values=c("brown2", "darkorange1", "yellow2", "green4"))
