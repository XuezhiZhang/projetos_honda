require(data.table)
require(dplyr)  

setwd("~/IGB/Histórico Fechamento/2019/Abril")
load("2019_04.RData")

  files = list.files(pattern="*txt")
  l = lapply(files, fread, sep = "\t", dec = ",", colClasses = c("Contrato" = "character",
                                                                 "Dt_Safra" = "character",
                                                                 "CPF_CNPJ" = "character"), check.names = TRUE,  na.strings = "")

  # script to find where the error is, step by step, archive by archive 
  # (it will print where the loop stopped)
  
  # out.file = data.frame()
  # for(i in 1:length(files)){
  #   file <- fread(files[i], header=TRUE, sep = "\t", dec = ",", colClasses = c("Contrato" = "character",
  #                                                                              "Dt_Safra" = "character",
  #                                                                              "CPF_CNPJ" = "character"), check.names = TRUE,  na.strings = "")
  #   out.file <- bind_rows(out.file, file)
  #   print(paste(files[i], "read", " - archive ", i))
  # }
  # 
  # x = fread("2016-10-2W.txt", sep = "\t", dec = ",")
  
  # load("names_2019.RData")
  
  db.2019 = l %>% bind_rows()
  
  db.2019 = db.2019 %>% select(-c(V61, V62, V63, V64, Cod, Modelo2)) 
  db.2019 = db.2019 %>% dplyr::rename("cod_contrato" = "Contrato",
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
                                      #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                      "num_chassi" = "Chassi",
                                      "qtd_parcelas_pagas" = "Pcl_p",
                                      #"score" = "SCORE",                            
                                      "situacao_contrato" = "Sit",
                                      "faixa_atraso" = "FAIXA",
                                      "assessoria_cob" = "ASSESSORIA_COB",
                                      "assessoria" = "ASSESSORIA",
                                      #"assessoria_nova" = "NOVA.ASSESSORIA",
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
  
  setwd("~/IGB/Histórico Fechamento/2019")
  names.2019 = names(db.2019)
  save(db.2019, file = "2019.RData")
  save(names.2015, file = "names_2015.RData")
  x = data.frame(levels(as.factor(db.2015$data_safra)))
  fwrite(x, file = "levels_safra_2015.csv")

  ##############################
  
  db.2016 = l %>% bind_rows()
  
  db.2016 = db.2016 %>% select(-c(V60, V61, V62, V63, V64, V65, Contrat, Cod, dia.SALES, DIA.VENCIMENTO, DIA.SALES, DIA.VENDA, SPF, Score)) 
  db.2016 = db.2016 %>% dplyr::rename("cod_contrato" = "Contrato",
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
                                              #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                              "num_chassi" = "Chassi",
                                              "qtd_parcelas_pagas" = "Pcl_p",
                                              "score" = "SCORE",                            
                                              "situacao_contrato" = "Sit",
                                              "faixa_atraso" = "FAIXA",
                                              "assessoria_cob" = "ASSESSORIA_COB",
                                              "assessoria" = "ASSESSORIA",
                                              "assessoria_nova" = "ASSESSORIA.NOVA",
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
  
  setwd("~/IGB/Histórico Fechamento/2016")
  names.2016 = names(db.2016)
  save(db.2016, file = "2016.RData")
  save(names.2016, file = "names_2016.RData")
  fwrite(x, file = "levels_safra_2016.csv")

  #############################
  
  db.2017 = l %>% bind_rows()
  
  db.2017 = db.2017 %>% select(-c(V60, V61, V62, V63, V64, V65, V4, Contrat, Cod)) 
  db.2017 = db.2017 %>% select(-c(SPF, TAB, CARTEIRA.RISCO, agruip, Agrup, CLASSE.RENDA, Modelo.1)) 
  
  db.2017 = db.2017 %>% dplyr::rename("cod_contrato" = "Contrato",
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
                                      #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                      "num_chassi" = "Chassi",
                                      "qtd_parcelas_pagas" = "Pcl_p",
                                      "score" = "SCORE",                            
                                      "situacao_contrato" = "Sit",
                                      "faixa_atraso" = "FAIXA",
                                      "assessoria_cob" = "ASSESSORIA_COB",
                                      "assessoria" = "ASSESSORIA",
                                      # "assessoria_nova" = "ASSESSORIA.NOVA",
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
  
  names.2017 = names(db.2017)
  save(db.2017, file = "2017.RData")
  save(names.2017, file = "names_2017.RData")
  
  #####################
  
  db.2018 = l %>% bind_rows()
  
  x = names(db.2018)[60:78]
  db.2018 = db.2018 %>% select(-c(x)) 
  db.2018 = db.2018 %>% select(-SCORE) 
  
  db.2018 = db.2018 %>% dplyr::rename("cod_contrato" = "Contrato",
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
                                      #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                      "num_chassi" = "Chassi",
                                      "qtd_parcelas_pagas" = "Pcl_p",
                                      #"score" = "SCORE",                            
                                      "situacao_contrato" = "Sit",
                                      "faixa_atraso" = "FAIXA",
                                      "assessoria_cob" = "ASSESSORIA_COB",
                                      "assessoria" = "ASSESSORIA",
                                      # "assessoria_nova" = "ASSESSORIA.NOVA",
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
  
  names.2018 = names(db.2018)
  save(db.2018, file = "2018.RData")
  save(names.2018, file = "names_2018.RData")
  x = data.frame(levels(as.factor(db.2018$data_safra)))
  fwrite(x, file = "levels_safra_2018.csv")
  
  load()
  
  ############################
  
  db.2019 = l %>% bind_rows()
  
  db.2019 = db.2019 %>% select(-Contrat) 
  db.2019 = db.2019 %>% select(-V61, -V62) 
  db.2019 = db.2019 %>% select(-V60)
  
  db.2019 = db.2019 %>% dplyr::rename("cod_contrato" = "Contrato",
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
                                      #"agr_profissao_cli" = "AGRUP..PROFISSÃO",
                                      "num_chassi" = "Chassi",
                                      "qtd_parcelas_pagas" = "Pcl_p",
                                      #"score" = "SCORE",                            
                                      "situacao_contrato" = "Sit",
                                      "faixa_atraso" = "FAIXA",
                                      "assessoria_cob" = "ASSESSORIA_COB",
                                      "assessoria" = "ASSESSORIA",
                                      # "assessoria_nova" = "ASSESSORIA.NOVA",
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
  
  names.2019 = names(db.2019)
  save(db.2019, file = "2019.RData")
  save(names.2019, file = "names_2019.RData")
  x = data.frame(levels(as.factor(db.2019$data_safra)))
  fwrite(x, file = "levels_safra_2019.csv")
  