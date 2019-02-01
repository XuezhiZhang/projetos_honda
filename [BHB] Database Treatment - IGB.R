require(data.table)
require(dplyr)
require(plyr)
require(lubridate)
require(ggplot2)
require(psych)
require(naniar)
require(reshape2)

#READING MONTHLY IGB FILE (Fechamento)
setwd("R:/Estatística/BHB/Databases BHB/Fechamento BHB")
setwd("D:/Users/sb044936/Documents/IGB/Backup")
load("bhb_fecha_oct'18.RData")

bhb.fecha.dez.18 <- fread("bhbigbfin_0929320000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                                                   "Cep" = "character",
                                                                                                                   "Cep l" = "character",
                                                                                                                   "CPF CNPJ" = "character"))

bhb.fecha.nov.18 <- fread("bhbigbfin_0829290000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                                                   "Cep" = "character",
                                                                                                                   "Cep l" = "character",
                                                                                                                   "CPF CNPJ" = "character"))

bhb.fecha.oct.18 <- fread("bhbigbfin_1006560000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                               "Cep" = "character",
                                                                                               "Cep l" = "character",
                                                                                               "CPF CNPJ" = "character"))

bhb.fecha.sep.18 <- fread("bhbigbfin_0747120000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                               "Cep" = "character",
                                                                                               "Cep l" = "character",
                                                                                               "CPF CNPJ" = "character"))

bhb.fecha.ago.18 <- fread("bhbigbfin_0747290000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                               "Cep" = "character",
                                                                                               "Cep l" = "character",
                                                                                               "CPF CNPJ" = "character"))
bhb.fecha.valid <- fread("bhbigbfin_1947220000.txt", header = TRUE, dec = ",", check.names = TRUE, colClasses = c("Contrato" = "character",
                                                                                                                      "Cep" = "character",
                                                                                                                      "Cep l" = "character",
                                                                                                                      "CPF CNPJ" = "character"))


bhb.fecha = bind_rows(bhb.fecha.nov.18, bhb.fecha.oct.18); rm(bhb.fecha.nov.18, bhb.fecha.oct.18)
bhb.fecha = bhb.fecha.dez.18
setwd("D:/Users/sb044936/Documents/IGB")
bhb.fecha <- list.files(pattern = "*.txt") %>%
             lapply(fread, stringsAsFactors=F,
             colClasses = c("Contrato" = "character",
                        "Cep" = "character",
                        "Cep l" = "character",
                        "CPF CNPJ" = "character"), dec = ",",
             header = TRUE, check.names = TRUE) %>%
             bind_rows %>% subset(Sta == "ATV")

save(bhb.fecha.dez.18, file = "bhb_fecha_dez_18.RData")
save(bhb.fecha.nov.18, file = "bhb_fecha_nov_18.RData")
save(bhb.fecha.oct.18, file = "bhb_fecha_oct_18.RData")
bhb.final = bind_rows(bhb.fecha.oct.18, bhb.fecha.nov.18); rm(bhb.fecha.oct.18, bhb.fecha.nov.18)
bhb.final = bind_rows(bhb.final, bhb.fecha.dez.18); rm(bhb.fecha.dez.18)
save(bhb.final, file = "bhb_final.RData")
load("bhb_final.RData")

load("bhb_fecha_dez_18.RData")
load("bhb_fecha_nov_18.RData")
load("bhb_fecha_oct_18.RData")

load("bhb_fecha.RData") #OCT, NOV, DEC

bhb.fecha.valid = valid %>%
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

bhb.fecha.valid = bhb.fecha.valid %>% subset(qtd_parc_restantes > 0)

setwd("~/IGB")
# save(bhb.fecha, file = "bhb_fecha_ago_oct_18.RData")
load("bhb_fecha_ago_oct_18.RData")

#turning all variable that starts with "data_" as a date format
bhb.fecha.valid = bhb.fecha.valid %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("data_")), funs(as.Date(as.character(.), format = "%d/%m/%Y")))

#JOINING DATABASES: PAYMENTS & FECHAMENTO

names(atrasos.count.by.ctr)[1] = "cod_contrato"
bhb.fecha.valid <- left_join(bhb.fecha.valid, atrasos.count.by.ctr); bhb.fecha.valid = bhb.fecha.valid %>% select(-MODAL.)

#STANDARDIZING DATABASE & CREATING NEW VARIABLES FOR MODELLING

bhb.final$vlr_renda_mensal_cli = ifelse(bhb.final$vlr_renda_mensal_cli == 1, NA, 
                                        bhb.final$vlr_renda_mensal_cli) 

bhb.fecha.valid$`idade_cli` =  as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), bhb.fecha.valid$data_nascimento), "years"))
bhb.fecha.valid$`tempo_contrato_anos` = as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), bhb.fecha.valid$data_contrato), "years"))
bhb.fecha.valid$`tempo_contrato_meses` = as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), bhb.fecha.valid$data_contrato), "months"))
bhb.fecha.valid$`tempo_desde_ult_pgt` = as.integer(time_length(difftime(as.Date(Sys.Date(), format = "%Y-%m-%d"), bhb.fecha.valid$data_ult_pgt), "days"))

bhb.fecha.valid$`perc_parc_pagas` = bhb.fecha.valid$qtd_parc_pagas / (bhb.fecha.valid$qtd_parc_pagas + bhb.fecha.valid$qtd_parc_restantes)
bhb.fecha.valid$`perc_vnc_finan` = bhb.fecha.valid$vlr_vencido / bhb.fecha.valid$vlr_total_financiado
bhb.fecha.valid$`perc_vnc_renda` = bhb.fecha.valid$vlr_vencido / bhb.fecha.valid$vlr_renda_mensal
bhb.fecha.valid$`perc_vnc_bens` = bhb.fecha.valid$vlr_vencido / bhb.fecha.valid$vlr_total_bens
bhb.fecha.valid$`perc_ult_pgt_parc` = bhb.fecha.valid$vlr_ult_pgt / bhb.fecha.valid$vlr_parcela
bhb.fecha.valid$`perc_a_vencer_finan` = bhb.fecha.valid$vlr_a_vencer / bhb.fecha.valid$vlr_total_financiado

bhb.final$`perc_pg_atr_1_10` = bhb.final$qtd_pg_atr_1_10_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_1_60` = bhb.final$qtd_pg_atr_1_60_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_11_60` = (bhb.final$qtd_pg_atr_11_30_em_1_ano + bhb.final$qtd_pg_atr_31_60_em_1_ano) / (bhb.final$qtd_pg_atr_em_1_ano)
bhb.final$`perc_pg_atr_61_360` = bhb.final$qtd_pg_atr_61_360_em_1_ano / bhb.final$qtd_pg_atr_em_1_ano
bhb.final$`perc_pg_atr_360_mais` = bhb.final$qtd_pg_atr_360_mais_em_1_ano/ bhb.final$qtd_pg_atr_em_1_ano

bhb.fecha.valid$`perc_parc_renda` = bhb.fecha.valid$vlr_parcela/bhb.fecha.valid$vlr_renda_mensal_cli
bhb.fecha.valid$`perc_pg_finan` = (bhb.fecha.valid$vlr_parcela*bhb.fecha.valid$qtd_parc_pagas)/bhb.fecha.valid$vlr_total_financiado

#AGGREGATING OTHER DATABASES
setwd("R:/Estatística/BHB/Databases BHB/Another fonts BHB")
ibge = fread("indicadores_pnad_uf.txt",  header = TRUE, dec = ",") %>% filter(pnad_versao == "abr-mai-jun")
ibge = ibge %>% dplyr::rename("nome_estado_cli" = "uf")

#adding to bhb.fecha ibge database
bhb.final = left_join(bhb.final, ibge, by = "nome_estado_cli")
rm(ibge); gc();gc()

#adding cc (cilindradas) database
cc = fread("de_para_cc.txt",  header = TRUE)
bhb.final = left_join(bhb.final, cc, by = "modelo")

#########################
#CLEANING CORE DATABASES#
#########################

rm(bhb.fecha, atrasos.count.by.ctr, cc); gc(); gc()

# SUBSTITUTE ALL NUMERIC MISSING DATA WITH 0
# bhb.fecha = bhb.fecha %>% 
#                    mutate_at(vars(starts_with("vlr_")), funs(replace(., is.na(.), 0)))
bhb.fecha.valid = bhb.fecha.valid %>% mutate_if(is.numeric, funs(replace(., is.infinite(.), NA))) %>%
                          mutate_if(is.numeric, funs(replace(., is.nan(.), NA)))

names(bhb.final) <- gsub("/", "_", names(bhb.final), fixed = TRUE)
names(bhb.final) <- gsub(".", "_", names(bhb.final), fixed = TRUE)

save(bhb.final, file = "bhb_final.RData") #OCT/NOV/DEC
load("bhb_final.RData")

view.brain = bhb.fecha %>% filter(qtd_pg_atr_em_1_ano > 0) %>% select(cod_contrato, qtd_pg_atr_1_10_em_1_ano, qtd_pg_atr_11_60_em_1_ano, qtd_pg_atr_61_360_em_1_ano, qtd_pg_atr_360_mais_em_1_ano, qtd_pg_atr_em_1_ano, perc_pg_atr_1_10, perc_pg_atr_11_60, perc_pg_atr_61_360, perc_pg_atr_360_mais)
fwrite(view.brain, file = "view_brain.csv")

deps <- tools::package_dependencies("dplyr")$dplyr
install.packages(deps)

x = data.frame(lapply(bhb.fecha.t, as.Date, format = "%d/%m/%Y"))

###############################################

save(bhb.fecha.valid, file = "bhb_fecha_valid.RData")
load("bhb_fecha_valid.RData")
