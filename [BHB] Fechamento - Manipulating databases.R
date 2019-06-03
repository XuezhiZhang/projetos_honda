require(dplyr)
require(data.table)
require(stringr)
require(tidyr)

"\nReading all .txt files at 'R:/BHB/Fechamento Gerencial/Fechamento - 2019/Fechamento Mensal/Teste COB Fechamentos'.\n" %>% cat()
setwd("R:/BHB/Fechamento Gerencial/Fechamento - 2019/Fechamento Mensal/Teste COB Fechamentos") #setting working directory

files = list.files(pattern="*txt")
l = sapply(files, fread, sep = "\t", dec = ",", colClasses = c("Contrato" = "character",
                                                               "CPF_CNPJ" = "character"), check.names = TRUE,  na.strings = "") #lendo todos arquivos e colocando eles em uma lista
# looping para adicionar coluna com a descrição da base (2W; 2W LOSS; 4W; 4W LOSS)

bhb.final = l %>% bind_rows() #juntando todas as bases em uma só (2W, 2WLOSS, 4W, 4WLOSS)
"Step 1 done: joining all databases > 2W, 2W LOSS, 4W, 4W LOSS." %>% cat()


bhb.final = bhb.final %>% dplyr::rename("cod_contrato" = "Contrato",
                                    "data_contrato" = "Data_Ctr",
                                    "tipo_pessoa" = "T",
                                    "cpf_cnpj" = "CPF_CNPJ",
                                    "cod_hda" = "Cod_HDA",
                                    "nome_hda" = "Nome_HDA",
                                    "nome_mun_loja" = "Municipio_Loj",
                                    "nome_est_loja" = "Estado_Loj",
                                    "prazo_contrato" = "Prz",
                                    "vlr_tx_venda" = "Tx_Venda",
                                    "vlr_total_financiado" = "Vlr_Total_Financiado",
                                    "vlr_total_bens" = "Vlr_Total_Bens",
                                    "vlr_liberado" = "Valor_Liberado",
                                    "vlr_a_vencer" = "Valor_AVC",
                                    "vlr_vencido" = "Valor_VC",
                                    "qtd_itens" = "Qtd",
                                    "nome_marca" = "Marca",
                                    "nome_modelo" = "Modelo",
                                    "ano_fabr" = "Fabr",
                                    "ano_modelo" = "Mod",
                                    "status_contrato" = "Sta",
                                    "qtd_dias_em_atraso" = "Dias",
                                    "segmento" = "Seg",
                                    "data_ult_pgt" = "Dt_Ult_Pgt",
                                    "vlr_ult_pgt" = "Vlr_Ult_Pgto",
                                    "data_vencimento" = "Dt_Vencto",
                                    "vlr_parcela" = "Vlr_Pcl",
                                    "situacao_produto" = "Z",
                                    "tabela" = "TAb_Negocia",
                                    "vlr_renda_mensal_cli" = "Renda_Mensal",
                                    "profissao_cli" = "Profissao",
                                    "cod_empresa" = "Cod",
                                    "num_chassi" = "Chassi",
                                    "qtd_parcelas_pagas" = "Pcl_p",
                                    "situacao_contrato" = "Sit",
                                    "faixa_atraso" = "FAIXA",
                                    "assessoria_cob" = "ASSESSORIA_COB",
                                    "assessoria" = "ASSESSORIA",
                                    "status_cob" = "STATUS",
                                    "ano_contrato" = "Ano_Data_Ctr",
                                    "mes_contrato" = "Mes_Data_Ctr",
                                    "agr_prazo" = "AGRUP_PRAZO",
                                    "agr_entrada" = "AGRUP_ENTRADA",
                                    "agr_retorno" = "AGRUP_RETORNO",
                                    "familia_bem" = "FAMILIA",
                                    "cc" = "CC",
                                    "estado_bem" = "CLASS_ESTADO_BEM",
                                    "motivo_inad" = "MOT_INADIMPLENCIA",
                                    "subs_marca" = "SUBS_MARCA",
                                    "vlr_subs_marca" = "Vlr_subs_Marca",
                                    "nome_regiao_cli" = "REGIAO",
                                    "qtd_parc_restantes" = "Qtd_Parc_restante",
                                    "parcela_atual" = "Parcela_Atual",
                                    "qtd_parc_atrasadas" = "Qtd_P_Atrasadas",
                                    "qtd_parc_pagas" = "Qtd_P_Pagas",
                                    "data_safra" = "Dt_Safra",
                                    "perc_entrada" = "Perc",
                                    "status_contabil" = "St")
"Step 2 done: standardizing column names.\n" %>% cat()

bhb.final = bhb.final %>% select(-c("V60", "V61", "V62")) #excluindo colunas vazias que vem de lixo do excel
bhb.final = bhb.final %>% distinct() #removendo contratos duplicados

"\nAre there any duplicated contract?" %>% cat()
print(table(duplicated(bhb.final$cod_contrato))) # verifica se há cod_contrato duplicado
"\nHow many empty cells are in database?" %>% cat()
print(table(is.na(bhb.final))) # verifica quantas células vazias existem

bhb.final = bhb.final %>% mutate(nome_modelo = str_trim(nome_modelo)) # removendo espaços desnecessários na coluna nome_modelo
bhb.final = bhb.final %>% mutate(nome_modelo = str_squish(nome_modelo)) # removendo espaços desnecessários na coluna nome_modelo

setwd("A:/")
agrup.carro = fread("Agrupamentos_corrigido_carro.txt", sep = "\t") %>% mutate(segmento = "CAR")
agrup.moto = fread("Agrupamentos_corrigido_moto.txt", sep = "\t") %>% mutate(segmento = "MOT")

agrup = bind_rows(agrup.carro, agrup.moto)

bhb.final = left_join(bhb.final, agrup, by = c("nome_modelo","segmento"), suffix = c("_before", "_after"))
bhb.final = bhb.final %>% mutate(familia_bem = ifelse(is.na(familia_bem_before), familia_bem_after, familia_bem_before))
bhb.final = bhb.final %>% mutate(cc = ifelse(is.na(cc_before), cc_after, cc_before)) %>% distinct()
"\nStep 3 done: joining 'familia bem' and 'capacity cylinder (cc)'.\n\n" %>% cat()

#################

"Checking for missing values.\n" %>% cat()
"\nAre there any missing 'tipo cc' in database?" %>% cat()
print(table(is.na(bhb.final$tipo_cc), bhb.final$segmento))
"\nAre there any missing 'familia bem' in database?" %>% cat()
print(table(is.na(bhb.final$familia_bem), bhb.final$segmento))
"\nAre there any missing 'capacity cylinder (cc)' in database?" %>% cat()
print(table(is.na(bhb.final$cc), bhb.final$segmento))

#####################
# collection agency #
#####################

setwd("R:/BHB/Fechamento Gerencial/Fechamento - 2019/Concessionarias")
agrup.assess = fread("relacao_abr.txt", sep = "\t")

bhb.final$assessoria = NULL

bhb.final = left_join(bhb.final, agrup.assess, by = c("cod_hda", "nome_hda", "nome_est_loja")) %>% distinct()
# problema estava no join da base: juntar por cod_hda, nome_hda e nome_est_loja.
"\nStep 4 done: joining 'assessorias' to database.\n\n" %>% cat()

"Are there any missing 'assessoria' in database?" %>% cat()
table(is.na(bhb.final$assessoria))

setwd("R:/BHB/Fechamento Gerencial/Fechamento - 2019/Fechamento Mensal/Bases Fechamento/Bases Teste")
fwrite(bhb.final, file = paste0("base_final_",Sys.Date(),".csv"), sep = ";", dec = ",")
"\nStep 5 done: Final fechamento database written and saved.\n" %>% cat()

out = bhb.final %>% filter(qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360) %>% group_by(segmento) %>% summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
del.30 = bhb.final %>% filter(qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360) %>% group_by(segmento) %>% summarise(delinquents.30 = sum(qtd_itens, na.rm = TRUE))
del.90 = bhb.final %>% filter(qtd_dias_em_atraso >= 91 & qtd_dias_em_atraso <= 360) %>% group_by(segmento) %>% summarise(delinquents.90 = sum(qtd_itens, na.rm = TRUE))

all = full_join(out, del.30)
all = left_join(all, del.90, by = "segmento") %>% mutate(index.30 = round((delinquents.30/outstanding)*100,2),
                                                         index.90 = round((delinquents.90/outstanding)*100,2))

fwrite(all, file = paste0("general_indexes_30_90_",Sys.Date(),".csv"), sep = ";", dec = ",")
"Step 6 done: calculation of general indexes for 30 and 90 days overdue." %>% cat()
print(all)

##########
# STATES #
##########

out.states = bhb.final %>% group_by(nome_est_loja, segmento) %>% filter(qtd_dias_em_atraso >= 0 & qtd_dias_em_atraso <= 360) %>% summarise(outstanding = sum(qtd_itens, na.rm = TRUE))
out.states = spread(out.states, segmento, outstanding); names(out.states) = c("nome_est_loja", "outstanding_car", "outstanding_mot")
del.30.states = bhb.final %>% group_by(nome_est_loja, segmento) %>% filter(qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360) %>% summarise(delinquents.30 = sum(qtd_itens, na.rm = TRUE))
del.30.states = spread(del.30.states, segmento, delinquents.30); names(del.30.states) = c("nome_est_loja", "delinquents_30_car", "delinquents_30_mot")
del.90.states = bhb.final %>% group_by(nome_est_loja, segmento) %>% filter(qtd_dias_em_atraso >= 91 & qtd_dias_em_atraso <= 360) %>% summarise(delinquents.90 = sum(qtd_itens, na.rm = TRUE))
del.90.states = spread(del.90.states, segmento, delinquents.90); names(del.90.states) = c("nome_est_loja", "delinquents_90_car", "delinquents_90_mot")

states = left_join(out.states, del.30.states) 
states = left_join(states, del.90.states) %>% mutate(index_30_car = round((delinquents_30_car/outstanding_car)*100,2),
                                                     index_30_mot = round((delinquents_30_mot/outstanding_mot)*100,2),
                                                     index_90_car = round((delinquents_90_car/outstanding_car)*100,2),
                                                     index_90_mot = round((delinquents_90_mot/outstanding_mot)*100,2))

fwrite(states, file = paste0("states_indexes_30_90_",Sys.Date(),".csv"), sep = ";", dec = ",")
"\nStep 7 done: calculation of states indexes for 30 and 90 days overdue." %>% cat()
"\nAll files may be acessed at 'R:/BHB/Fechamento Gerencial/Fechamento - 2019/Fechamento Mensal/Bases Fechamento/Bases Teste'
######################.\n\n" %>% cat()


faixa_6_30 = bhb.final %>% filter(qtd_dias_em_atraso >= 6 & qtd_dias_em_atraso <= 30) %>% group_by(segmento) %>% summarise(delinq = sum(qtd_itens, na.rm = TRUE),
                                                                                                                           faixa = "6 - 30")
faixa_31_120 = bhb.final %>% filter(qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 120) %>% group_by(segmento) %>% summarise(delinq = sum(qtd_itens, na.rm = TRUE),
                                                                                                                               faixa = "31 - 120")
faixa_121_360 = bhb.final %>% filter(qtd_dias_em_atraso >= 121 & qtd_dias_em_atraso <= 360) %>% group_by(segmento) %>% summarise(delinq = sum(qtd_itens, na.rm = TRUE),
                                                                                                                                 faixa = "121 - 360")

delinq = bind_rows(faixa_6_30, faixa_31_120)
delinq = bind_rows(delinq, faixa_121_360)
delinq = delinq %>% spread(segmento, value = delinq) %>% arrange(faixa)
