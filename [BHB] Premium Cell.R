# active contracts already filtered;
# and qtd_parc_restantes > 0;
# and situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED")

premium_cell = bhb.final %>% filter(segmento == "CAR" | tipo_cc == "ALTA")
premium_cell = premium_cell %>% filter(qtd_dias_em_atraso <= 360)
premium_cell = premium_cell %>% filter(qtd_dias_em_atraso >= 11)

selection = premium_cell %>% select(cod_contrato, tipo_pessoa, idade_cli, vlr_renda_mensal_cli, 
                                    profissao_cli, vlr_total_financiado, vlr_total_bens, vlr_parcela, 
                                    modelo, estado_civil_cli, segmento)

selection = na.omit(selection)

x = dist(selection[,-1], method = "euclidean")
fit = hclust(x, method = "ward")

plot(fit)

groups <- cutree(fit, k = 2)

selection$cluster = as.factor(groups)

library(party)
library(randomForest)

tree <- ctree(cluster ~ vlr_renda_mensal_cli + idade_cli +
              vlr_total_financiado + vlr_total_bens + vlr_parcela,
              data=selection, controls = ctree_control(maxdepth = 4))
plot(tree)

selection = selection %>% mutate_if(is.character, as.factor)
selection$id

rf - randomForest(cluster ~ tipo_pessoa, idade_cli, vlr_renda_mensal_cli, 
                  profissao_cli, vlr_total_financiado, vlr_total_bens, vlr_parcela, 
                  modelo, estado_civil_cli, data = selection)

p = ggplot(selection, aes(cluster, vlr_renda_mensal_cli))
p + geom_boxplot()

selection$dec_renda = quantile(selection$vlr_renda_mensal_cli, probs = seq(0.1, 1, 0.1), na.rm = FALSE)

quantis = seq(0.1, 1, 0.1)
apply(selection[,2:11], 2, quantile, quantis, na.rm = TRUE)


selection = selection %>% mutate(decil_renda = ntile(vlr_renda_mensal_cli, n = 10),
                                 decil_idade = ntile(idade_cli, n = 10),
                                 decil_vlr_financ = ntile(vlr_total_financiado, n = 10),
                                 decil_vlr_bens = ntile(vlr_total_bens, n = 10),
                                 decil_vlr_parcela = ntile(vlr_parcela, n = 10))
  
p = ggplot(selection, aes(x = premium)) + 
  geom_boxplot()
p

x = selection %>% group_by(decil_renda, decil_idade, decil_vlr_financ, decil_vlr_bens, decil_vlr_parcela) %>%
  dplyr::summarise(n = n())

##########
#BEST WAY#
##########

selection.car = selection %>% filter(segmento == "CAR") %>% mutate(median_renda = median(vlr_renda_mensal_cli),
                                 median_vlr_financiado = median(vlr_total_financiado),
                                 median_vlr_bens = median(vlr_total_bens),
                                 median_vlr_parcela = median(vlr_parcela),
                                 median_idade = median(idade_cli),
                                 param_renda = ifelse(vlr_renda_mensal_cli >= median_renda, "> median", "< median"),
                                 param_vlr_financiado = ifelse(vlr_total_financiado >= median_vlr_financiado, "> median", "< median"),
                                 param_vlr_bens = ifelse(vlr_total_financiado >= median_vlr_bens, "> median", "< median"),
                                 param_vlr_parcela = ifelse(vlr_parcela >= median_vlr_parcela, "> median", "< median"),
                                 param_idade = ifelse(idade_cli >= median_idade, "> median", "< median"),
                                 param = paste0(param_renda, " | ", param_vlr_financiado, " | ", param_vlr_bens, " | ",
                                                param_vlr_parcela, " | ", param_idade))

require(stringr)
selection.car$count = str_count(selection.car$param, pattern = "> median")
selection.car$premium = ifelse(selection.car$count >= 3, "premium", "no premium")
table(selection.car$premium)
table(selection.car$premium)[1]/nrow(selection.car)

fwrite(teste, file = "cases.csv", sep = ";", dec = ",")
fwrite(prof, file = "profissoes.csv", sep = ";", dec = ",")

selection.car = selection.car %>% mutate(profissao_cli_flag = ifelse(profissao_cli %in% c("GERENTE","PROPRIETARIO DE MICROEMPRESA","MEDICO","PROPRIETARIO ESTAB.COMERCIAL","ADVOGADO","PSICOLOGO","CHEFE INTERMEDIARIO","OFICIAIS DA POLICIA CIVIL","MILITAR REFORMADO","DIRETOR DE ESTABELECIMENTO DE","ARQUITETO","PRESIDENTE,GOVERNADOR,PREFEITO",
                                                                                 "SUPERVISORES, CHEFES DE SECAO","SERVIDOR PUBLICO FEDERAL","SUPERVISOR/INSPETOR","ENGENHEIRO","DIRETOR DE EMPRESAS","MARINHEIRO E ASSEMELHADO","AGRONOMO","DELEGADO DE POLICIA","BIOLOGO E BIOMEDICO","SENADOR/DEPUTADO/ VEREADOR","MIN.TRIB.SUPERIOR/DESEMBARGADO",
                                                                                 "ODONTOLOGO","PROP.ESTAB.AGRICOLA/PECUARIA/F","BANCARIO E ECONOMIARIO","PROPRIETARIO ESTAB.DE PREST.SE","ASSESSORES, SUPERINTENDENTES","PILOTO DE AERONAVES","OFICIAIS FORCAS ARMADAS/FORCAS","ECONOMISTA","QUIMICO","EMPRESARIO E PRODUTOR DE ESPE","RELACOES PUBLICAS",
                                                                                 "JOALHEIROS E OURIVES","COMANDANTE DE EMBARCACOES"), "yes", "no"))


fwrite(selection.car, file = "selection_car.csv", sep = ";", dec = ",")
fwrite(selection.mot, file = "selection_mot.csv", sep = ";", dec = ",")

selection.mot = selection %>% filter(segmento == "MOT") %>% mutate(median_renda = median(vlr_renda_mensal_cli),
                                                                   median_vlr_financiado = median(vlr_total_financiado),
                                                                   median_vlr_bens = median(vlr_total_bens),
                                                                   median_vlr_parcela = median(vlr_parcela),
                                                                   median_idade = median(idade_cli),
                                                                   param_renda = ifelse(vlr_renda_mensal_cli >= median_renda, "> median", "< median"),
                                                                   param_vlr_financiado = ifelse(vlr_total_financiado >= median_vlr_financiado, "> median", "< median"),
                                                                   param_vlr_bens = ifelse(vlr_total_financiado >= median_vlr_bens, "> median", "< median"),
                                                                   param_vlr_parcela = ifelse(vlr_parcela >= median_vlr_parcela, "> median", "< median"),
                                                                   param_idade = ifelse(idade_cli >= median_idade, "> median", "< median"),
                                                                   param = paste0(param_renda, " | ", param_vlr_financiado, " | ", param_vlr_bens, " | ",
                                                                                  param_vlr_parcela, " | ", param_idade))

selection.mot$count = str_count(selection.mot$param, pattern = "> median")
selection.mot$premium = ifelse(selection.mot$count >= 3, "premium", "no premium")
table(selection.mot$premium)
table(selection.mot$premium)[1]/nrow(selection.mot)

selection = bind_rows(selection.car, selection.mot)

fwrite(selection, file = "selection.csv", sep = ";", dec = ",")