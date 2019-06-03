require(data.table)

# reading IGB
setwd("~/IGB/Daily IGB")
base_fecha = fread("igb_daily_26_04.txt", header = TRUE, 
                   dec = ",", check.names = TRUE, 
                   colClasses = c("Contrato" = "character",
                                  "Cep" = "character",
                                  "Cep l" = "character",
                                  "CPF CNPJ" = "character"), na.strings = "")

x = data.frame(names(fecha_mar))
x = head(fecha_mar)
fwrite(x, file = "bhb_test.csv", sep = ";", dec = ",")

setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/1-30/MOT")
# save(final_db, file = "final_db_MOT.RData")
load("final_db_MOT.RData")
final_db_mot = final_db

setwd("D:/Users/sb044936/Desktop/Modelling databases R/New models [march 2019]/1-30/CAR")
# save(final_db, file = "final_db_CAR.RData")
load("final_db_CAR.RData")
final_db_car = final_db

final_db = bind_rows(final_db_car, final_db_mot)
rm(final_db_car, final_db_mot)


formatting_database = function(database){
  
  require(data.table)
  require(plyr)
  require(dplyr)
  require(lubridate)
  require(ggplot2)
  require(psych)
  require(naniar)
  require(reshape2)
  
  # FORMATTING COLUMN NAMES TO A STANDARD WAY
  bhb.final = database %>%
    dplyr::rename("cod_contrato" = "Contrato",
                  "data_contrato" = "Data.Ctr",
                  "tipo_pessoa" = "T",
                  "cpf_cnpj" = "CPF.CNPJ",
                  "cod_hda" = "Cod.HDA",
                  "nome_hda" = "Nome.HDA",
                  "nome_mun_loja" = "Municipio.Loj",
                  "nome_est_loja" = "Estado.Loj",
                  "vlr_vrg_antecipado" = "VRG.Antecipado",
                  "vlr_vrg_diluido" = "VRG.Diluido",
                  "vlr_saldo_inicial" = "Saldo.Inicial",
                  "prazo_contrato" = "Prz",
                  "tabela_neg" = "Tab.Negocia",
                  "vlr_tx_venda" = "Tx.Venda",
                  "vlr_total_financiado" = "Vlr.Total.Financiado",
                  "vlr_total_bens" = "Vlr.Total.Bens",
                  "vlr_liberado" = "Valor.Liberado",
                  "vlr_a_vencer" = "Valor.AVC",
                  "vlr_vencido" = "Valor.VC",
                  "qtd_itens" = "Qtd",
                  "cod_inst_financ" = "Cod.",
                  "nome_marca" = "Marca",
                  "cod_marca" = "Cod.M",
                  "modelo" = "Modelo",
                  "ano_fabr" = "Fabr",
                  "ano_modelo" = "Mod",
                  "status_contrato" = "Sta",
                  "data_risco_contabil" = "Dt.Risco",
                  "rating_risco_contabil" = "Ni",
                  "vlr_seguri_casco" = "Vlr.Seguro.Casco",
                  "vlr_tac" = "Vlr.Tac",
                  "vlr_iof" = "Vlr.IOF",
                  "qtd_dias_em_atraso" = "Dias",
                  "segmento" = "Seg",
                  "data_ult_pgt" = "Dt.Ult.Pgt",
                  "vlr_ult_pgt" = "Vlr.Ult.Pgto",
                  "data_vencimento" = "Dt.Vencto",
                  "vlr_parcela" = "Vlr.Pcl",
                  "data_nascimento_cli" = "Dt.Nasc",
                  "situacao_produto" = "Z",
                  "vlr_renda_mensal_cli" = "Renda.Mensal",
                  "estado_civil_cli" = "Estado.Civil",
                  "genero_cli" = "S",
                  "profissao_cli" = "Profissao",
                  "data_baixa" = "Dt.Baixa",
                  "cod_banco" = "Cod.B",
                  "nome_banco" = "Banco",
                  "data_ult_alt" = "Dt.ult.alt",
                  "proposta" = "Propost",
                  "num_chassi" = "Chassi",
                  "qtd_parcelas_pagas" = "Pcl.p",
                  "cod_pessoa" = "Cod.Pes",
                  "score" = "Scor",                            
                  "situacao_contrato" = "Sit",
                  "cep_digito_cli" = "Com",
                  "cep_cli" = "Cep",
                  "perc_entrada/financ" = "Perc",
                  "cod_plano" = "Cod.Plano",
                  "vlr_subs_conc" = "Vlr.subs.conc",
                  "vlr_subs_marca" = "Vlr.subs.Marca",
                  "nome_municipio_cli" = "Munic.Cli",
                  "nome_estado_cli" = "Es",
                  "vlr_taxa_subs_conc" = "Tx.subs.conc",
                  "nome_cliente" = "Pessoa",
                  "vlr_desp_finan" = "Desp.finan",
                  "nome_regiao_cli" = "Região",
                  "vlr_tx_anual_ctr" = "Tx.anual.ctr",
                  "car" = "Car",
                  "cep_loja" = "Cep.l",
                  "loja" = "Loja",
                  "data_ult_vencimento" = "Dt.ult.ven",
                  "vlr_tx_subs_marc" = "Tx.subs.marc",
                  "vlr_tx_banco" = "Tx.Banco",
                  "vlr_taxa_cliente" = "Tx.Cliente",
                  "re" = "Re",
                  "vlr_comissao" = "Vlr.Comissao",
                  "cod_tabela" = "Cod.Tab",
                  "data_ini_seguro" = "Ini.Seguro",
                  "data_fim_seguro" = "Fim.Seguro",
                  "data_prim_vencimento" = "Dt.prim..v",
                  "nome_placa" = "Placa",
                  "nome_renavam" = "Renavam",
                  "analista_c" = "Analista.C",
                  "vlr_entrada" = "Valor.de.Entrada",
                  "status_contabil" = "St",
                  "for" = "FOR",
                  "vlr_seg_prot_finan" = "Vlr.Seg..Prot..Fin.",
                  "nome_seg_prot_finan" = "Seg..Prot..Fin.",
                  "vlr_seg_gara_estend" = "Vlr.Seg..Garan..Est.",
                  "nome_seg_gara_estend" = "Seg..Garan..Est.",
                  "nome_seg_casco" = "Seg..Casco",
                  "contrato_cedido" = "CONTRATO.CEDIDO",
                  "numero_contrato_cessao" = "NU.DO.CONTRATO.EM.CESSÃO",
                  "coobrigacao_sem_n" = "COOBRIGAÇÃO.S.N",
                  "qtd_parc_restantes" = "Nro",
                  "parcela_atual" = "Nro.1",
                  "qtd_parc_atrasadas" = "Qtd.p",
                  "qtd_parc_pagas" = "Qtd.p.1") %>% 
    select(-DDD, -DDD.1, -Qtd.1)
  
  # FILTERING NECESSARY CONTRACTS AND MANIPULATING DATE COLUMNS 
  bhb.final = bhb.final %>% subset(qtd_parc_restantes > 0)
  bhb.final = bhb.final %>% subset(status_contrato == "ATV")
  bhb.final = bhb.final %>% subset(situacao_contrato %in% c("CCA", "CCJ", "CEA", "CED"))
  
  bhb.final = bhb.final %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), funs(as.Date(as.character(.), format = "%d/%m/%Y")))
  bhb.final$situacao_produto = plyr::revalue(bhb.final$situacao_produto, c(S = "Bem novo", N = "Bem usado"))
  
  # STANDARDIZING DATABASE & CREATING NEW VARIABLES FOR MODELLING
  
  bhb.final$vlr_renda_mensal_cli = ifelse(bhb.final$vlr_renda_mensal_cli == 1, NA, 
                                          bhb.final$vlr_renda_mensal_cli) 
  
  bhb.final$`idade_cli` =  as.integer(time_length(difftime(as.Date("2019-04-26", format = "%Y-%m-%d"), bhb.final$data_nascimento), "years"))
  bhb.final$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date("2019-04-26", format = "%Y-%m-%d"), bhb.final$data_contrato), "years"))
  bhb.final$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date("2019-04-26", format = "%Y-%m-%d"), bhb.final$data_contrato), "months"))
  bhb.final$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date("2019-04-26", format = "%Y-%m-%d"), bhb.final$data_ult_pgt), "days"))
  
  bhb.final$`perc_parc_pagas` = bhb.final$qtd_parc_pagas / (bhb.final$qtd_parc_pagas + bhb.final$qtd_parc_restantes)
  bhb.final$`perc_vnc_finan` = bhb.final$vlr_vencido / bhb.final$vlr_total_financiado
  bhb.final$`perc_vnc_renda` = bhb.final$vlr_vencido / bhb.final$vlr_renda_mensal
  bhb.final$`perc_vnc_bens` = bhb.final$vlr_vencido / bhb.final$vlr_total_bens
  bhb.final$`perc_ult_pgt_parc` = bhb.final$vlr_ult_pgt / bhb.final$vlr_parcela
  bhb.final$`perc_a_vencer_finan` = bhb.final$vlr_a_vencer / bhb.final$vlr_total_financiado
  
  bhb.final$`perc_parc_renda` = bhb.final$vlr_parcela/bhb.final$vlr_renda_mensal_cli
  bhb.final$`perc_pg_finan` = (bhb.final$vlr_parcela*bhb.final$qtd_parc_pagas)/bhb.final$vlr_total_financiado
  
  names(bhb.final) <- gsub("/", "_", names(bhb.final), fixed = TRUE)
  names(bhb.final) <- gsub(".", "_", names(bhb.final), fixed = TRUE)
  
  bhb.final <<- bhb.final
  
}

formatting_database(base_fecha)
bhb.final = bhb.final %>% filter(qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360)
bhb.final$score = NULL
bhb.final = left_join(bhb.final, final_db, by = "cod_contrato")

dealers.vision = bhb.final %>% filter(qtd_dias_em_atraso > 0 & qtd_dias_em_atraso <= 20) %>%
  group_by(cod_hda, nome_hda, segmento, nome_est_loja) %>% 
  dplyr::summarise(median_atraso = median(qtd_dias_em_atraso, na.rm = TRUE),
                   median_balance = median(vlr_vencido, na.rm = TRUE),
                   median_score = median(score, na.rm = TRUE),
                   contracts = n()) %>%
                   mutate(cut_median_atraso = cut(median_atraso, breaks = c(seq(0, 400, by = 5)), include.lowest = TRUE, dig.lab = 6),
                          cut_median_balance = cut(median_balance, breaks = c(seq(0, 100000, by = 200)), include.lowest = TRUE, dig.lab = 6),
                          cut_median_score = cut(median_score, breaks = c(seq(0, 1000, by = 50)), include.lowest = TRUE, dig.lab = 6),
                          cut_contracts = cut(contracts, breaks = c(seq(0, 10000, by = 100)), include.lowest = TRUE, dig.lab = 6))

out = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(outstanding_hda = sum(qtd_itens, na.rm = TRUE))
del = bhb.final %>% dplyr::filter(qtd_parc_restantes > 0 & qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360 & situacao_contrato %in% c("CCA","CCJ","CEA","CED")) %>% dplyr::group_by(cod_hda, nome_hda, segmento) %>% dplyr::summarise(delinquents_hda = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del)
all = all %>% mutate_if(is.numeric, list(~replace(., is.na(.), 0)))

all = all %>% mutate(index_hda = delinquents_hda/outstanding_hda)

dealers.vision = left_join(dealers.vision, all)
dealers.vision = dealers.vision %>% filter(paste0(cod_hda, nome_hda) %in% c(paste0(missing.assessoria$cod_hda, missing.assessoria$nome_hda)))

dealers.vision = dealers.vision %>% mutate(cut_index = cut(index_hda, breaks = c(seq(0, 1, by = 0.05)), include.lowest = TRUE),
                                           id = rownames(dealers.vision))

set.seed(276)
assessoria.db.1 <- dealers.vision %>% group_by(cut_median_atraso, cut_median_balance, cut_median_score, cut_contracts, segmento) %>% sample_frac(0.5)
assessoria.db.1$assessoria <- "FLEX"
assessoria.db.2 <- dealers.vision %>% filter(!id %in% c(assessoria.db.1$id))
assessoria.db.2$assessoria <- "PASCHOALOTTO"
                                    
assessoria.db <- bind_rows(assessoria.db.1, assessoria.db.2)

summar.shuffle =  assessoria.db %>% group_by(assessoria, segmento) %>% dplyr::summarise(dealers = n_distinct(nome_hda),
                                                                       contracts = sum(contracts, na.rm  = TRUE),
                                                                       states = n_distinct(nome_est_loja)) %>%
                                                                       mutate(perc_dealers = dealers/n_distinct(assessoria.db$nome_hda),
                                                                              perc_contracts = contracts/sum(assessoria.db$contracts, na.rm = TRUE))

summar.index =  assessoria.db %>% group_by(assessoria, segmento) %>% dplyr::summarise(delinquents_assess = sum(delinquents_hda),
                                                                                        outstanding_assess = sum(outstanding_hda),
                                                                                        index = delinquents_assess/outstanding_assess)

summar = left_join(summar.shuffle, summar.index)
##############################
contracts <- bhb.final %>% filter(qtd_dias_em_atraso > 0 & qtd_dias_em_atraso <= 20) %>% 
                           mutate(cut_atraso = cut(qtd_dias_em_atraso, breaks = c(seq(0, 400, by = 2)), include.lowest = TRUE, dig.lab = 6),
                                  cut_balance = cut(vlr_vencido, breaks = c(seq(0,100000, by = 500)), include.lowest = TRUE, dig.lab = 6))
contracts <- left_join(contracts, assessoria.db)

plot.table.est = contracts %>% group_by(assessoria, nome_est_loja, segmento) %>% dplyr::summarise(n = n())
plot.table.days = contracts %>% group_by(assessoria, cut_atraso, segmento) %>% dplyr::summarise(n = n())
plot.table.balance = contracts %>% group_by(assessoria, cut_balance, segmento) %>% dplyr::summarise(n = n())
plot.table.cluster = contracts %>% group_by(assessoria, cluster, segmento) %>% dplyr::summarise(n = n())
plot.table.hda = contracts %>% group_by(assessoria, nome_hda, segmento) %>% dplyr::summarise(n = n())
plot.table.index = contracts %>% group_by(assessoria, cut_index, segmento) %>% dplyr::summarise(n = n())

# add balance (owed value), dealer (with and without), score, 

est.plot = ggplot(data=plot.table.est, aes(x=nome_est_loja, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(title = "Digital Rule - Collection agency dealers definition",
       subtitle =  "Accounts share by state",
       fill = "", x = "state", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

delay.plot = ggplot(data=plot.table.days, aes(x=cut_atraso, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts share by delay",
       fill = "", x = "days overdue", y = "contracts (%)") + 
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

balance.plot = ggplot(data=plot.table.balance, aes(x=cut_balance, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle = "Accounts share by balance",
       fill = "", x = "balance", y = "contracts (%)") +
  #caption = "Source: IGB | march, 2019 | active contracts [1 - 360]") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hda.plot = ggplot(data=plot.table.hda, aes(x=nome_hda, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts share by dealer",
       fill = "", x = "dealer", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

cluster.plot = ggplot(data=plot.table.cluster, aes(x=cluster, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts share by cluster",
       fill = "", x = "collection risk cluster", y = "contracts (%)") + 
  theme(legend.position="none") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

index.plot = ggplot(data=plot.table.index, aes(x=cut_index, y=n, fill=assessoria)) +
  geom_bar(stat="identity", position = "fill") +
  facet_wrap(~segmento) + coord_flip() +
  labs(subtitle =  "Accounts share by index",
       fill = "", x = "dealer delinquency index", y = "contracts (%)",
       caption = "source: IGB | march, 2019 | active contracts [1 - 20]") + 
  theme(legend.position="bottom") #+
#  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", ))

hlay <- rbind(c(1,2,2),
              c(3,4,5))

both_plot_actual <- grid.arrange(est.plot, delay.plot, balance.plot, cluster.plot, index.plot, layout_matrix = hlay)

setwd("D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)")
assessoria.db.filtered = assessoria.db %>% select()
fwrite(assessoria.db, file = "collection_agency_distribution.csv", sep = ";", dec = ",")
fwrite(summar, file = "summar_collection_agency_distribution.csv", sep = ";", dec = ",")
save(assessoria.db, file = "assessoria_database.RData")

ggsave(paste0("plots_digital_rule_1_20","_", Sys.Date(),".tiff"), both_plot_actual, units="in", width=12, height=8, 
       path = "D:/Users/sb044936/Desktop/Serasa imputation (SCORE NET USER)/")

miss = digital_rule_db %>% filter(is.na(assessoria))