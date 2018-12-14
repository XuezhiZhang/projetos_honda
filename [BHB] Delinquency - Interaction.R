plot.function = function(df, x, y){

require(dplyr)
require(ggplot2)

x = sym(x)
y = sym(y)

outstanding = db %>% dplyr::filter(qtd_dias_em_atraso < 360) %>% dplyr::group_by(segmento, !! x, !! y) %>% dplyr::summarise(outstanding = dplyr::sum(qtd_itens))
delinquents = db%>% dplyr::filter(qtd_dias_em_atraso >= 31 & qtd_dias_em_atraso <= 360) %>% dplyr::group_by(segmento, !! x, !! y) %>% dplyr::summarise(delinquents = dplyr::sum(qtd_itens), deliq_contracts = n())

join = full_join(outstanding, delinquents)
join = join %>% mutate(delinquency_index = delinquents/outstanding)

join = join %>% replace(., is.na(.), 0)
# join = join %>% filter(idade_cli <= 100)
# # idade_bens = idade_bens %>% filter(vlr_total_bens <= 400000)

#############################################################
sp1 <- ggplot(join, aes(!!x, !!y))
# sp +  geom_point(color = "lightgray")

pl <- sp1 + stat_density_2d(aes(fill = join$delinquency_index), geom = "polygon") + 
      facet_wrap( ~ segmento, ncol=2) +
      scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20")) +
      labs(title = paste0(x, " vs. ", y), caption = paste0("Source: IGB - active contracts - november 2018"))

      filename=paste0("D:/Users/sb044936/Desktop/Modelling databases R/Interactions/",x,"_vs_",y,"_",Sys.Date(),".tiff")
      tiff(filename, units="in", width=12, height=8, res=500)
      print(pl)
      dev.off()
}

z = join %>% group_by(var1.bin = quantile(idade_cli, probs = seq(0, 1, 0.1), na.rm = TRUE),
                      var2.bin = quantile(vlr_renda_mensal_cli, probs = seq(0, 1, 0.1), na.rm = TRUE)) %>%
  summarise(delinquents = sum(delinquents, na.rm = TRUE),
            outstanding = sum(outstanding, na.rm = TRUE),
            delinquency_index = delinquents/outstanding) %>%
  ggplot(aes(var1.bin, var2.bin, fill=delinquency_index)) +
  geom_tile() + 
  theme_classic()

# sp2 <- ggplot(join, aes(!!x, delinquency_index))
# 
# sp2 + stat_density_2d(aes(fill = stat(level)), geom = "polygon") + 
#   facet_wrap( ~ segmento, ncol=2) +
#   scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))
#############################################################

# Plotting

}

for(i in 1:ncol(z)){
    plot.function(db, z[1,i], z[2,i])}

pl = ggplot(join, aes(idade_cli, vlr_renda_mensal_cli)) + stat_summary_2d(aes(z = delinquency_index), bins = 30, fun = mean) + 
  scale_fill_gradientn(limits = range(join$delinquency_index), colours = c("green", "red"), guide = guide_colourbar(title = "delinquency index")) +
  facet_wrap(segmento ~ .)
plot(pl)

options(warn=-1)
options(warn=0)

x <- "cod_contrato"

library(ggpubr)
# Grouped Scatter plot with marginal density plots
ggscatterhist(
  idade_bens, x = "idade_cli", y = "vlr_total_bens",
  color = "segmento", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list(fill = "segmento", color = "black", size = 0.2)
)

for (i in 1:ncol(z)) {
  print(z[,i])
}

join = filter(is.finite)

sp <- ggplot(join, aes(idade_cli, vlr_renda_mensal_cli))
# sp +  geom_point(color = "lightgray")

sp + stat_density_2d(aes(fill = stat(level)), geom = "polygon") + 
     facet_wrap( ~ segmento, ncol=2) +
     scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))


sp + stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE) + 
     facet_wrap( ~ segmento, ncol=2) +
     scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))

# Rectangular binning
ggplot(join, aes(idade_cli, vlr_renda_mensal_cli)) +
  geom_bin2d(bins = 30, color ="white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_minimal()

x$delinquency_index = round(x$delinquency_index,4)

b <- ggplot(x, aes(x = idade_cli, y = vlr_renda_mensal_cli))
b + geom_point(aes(color = ..level.., size = 3)) +
    scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
    
    
    mine.heatmap <- ggplot(data = idade_bens, mapping = aes(x = idade_cli,
                                                           y = vlr_total_bens,
                                                           fill = delinquecy_index)) + geom_tile()

 z <-   function(df, x, y){
 p <-   ggplot(x , aes(x = idade_cli, y = vlr_renda_mensal_cli)) +
      geom_raster(aes(fill = delinquency_index), interpolate=TRUE) +
      scale_fill_gradient2(low="green", mid="yellow", high="red", limits=range(join$delinquency_index))
 plot(p)}
    
    z(join, "idade_cli", "vlr_renda_mensal_cli")
    
    require(ggplot)]

require(hexbin)
    
    ggplot(idade_bens, aes(idade_cli, vlr_total_bens, z = delinquency_index)) +
      stat_summary_2d() +
      geom_point(shape = 1, col = 'white') +
      viridis::scale_fill_viridis()
    
samp <- dplyr::sample_n(idade_bens, 10)

randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

samp <- randomRows(idade_bens, 2000)



#BEST PLOT (BIN & MEAN OF DELINQUENCY INDEX)
p = ggplot(join, aes(idade_cli, vlr_renda_mensal_cli)) + 
           stat_summary_2d(aes(z = delinquency_index), bins = 40, fun = mean) + 
           scale_fill_gradientn(limits = range(join$delinquency_index), colours = c("green", "red"), guide = guide_colourbar(title = "delinquency index")) +
           facet_wrap(segmento ~ .)

p

library("ggExtra")
ggMarginal(c, type = "density")

d <- ggplot(idade_bens, aes(idade_cli, vlr_total_bens))
d + stat_summary_2d(aes(z = outstanding), bins = 30) + scale_fill_gradientn(limits = range(idade_bens$outstanding), colours = c("green", "red")) + facet_wrap(segmento ~ .)











db = bhb.final %>%
  select(-cpf_cnpj,-tabela_neg,-num_chassi,-cep_digito_cli,-cep_cli,-nome_cliente,-vlr_tx_anual_ctr,
         -cep_loja,-vlr_tx_banco,-vlr_taxa_cliente,-cod_tabela,-nome_placa,-analista_c,
         -data_contrato, -cod_hda, -vlr_vrg_antecipado, -vlr_vrg_diluido, -vlr_saldo_inicial,
         -vlr_liberado, -cod_inst_financ, -cod_marca, -data_risco_contabil, -vlr_seguri_casco,
         -vlr_tac, -data_ult_pgt, -data_vencimento, -data_nascimento_cli, -data_baixa, -cod_banco,
         -data_ult_alt, -proposta, -cod_plano, -vlr_subs_conc, -vlr_subs_marca, -vlr_taxa_subs_conc, -vlr_desp_finan,
         -car, -cod_pessoa, -data_ult_vencimento, -vlr_tx_subs_marc, -re, -data_ini_seguro, -data_fim_seguro,
         -data_prim_vencimento, -nome_renavam, -`for`, -contrato_cedido, -numero_contrato_cessao,
         -coobrigacao_sem_n, -qtd_pg_atr_em_1_ano, -valor_pg_atr_em_1_ano, -qtd_pg_atr_1_10_em_1_ano,  
         -vlr_pg_atr_1_10_em_1_ano, -qtd_pg_atr_11_30_em_1_ano, -vlr_pg_atr_11_30_em_1_ano, -qtd_pg_atr_1_30_em_1_ano,    
         -vlr_pg_atr_1_30_em_1_ano, -qtd_pg_atr_1_60_em_1_ano, -vlr_pg_atr_1_60_em_1_ano, -qtd_pg_atr_31_60_em_1_ano,   
         -vlr_pg_atr_31_60_em_1_ano, -qtd_pg_atr_61_360_em_1_ano, -vlr_pg_atr_61_360_em_1_ano, -qtd_pg_atr_360_mais_em_1_ano,
         -vlr_pg_atr_360_mais_em_1_ano, -qtd_pg_atr_11_60_em_1_ano, -vlr_pg_atr_11_60_em_1_ano, -qtd_pg_atr_1_360_em_1_ano, -vlr_pg_atr_1_360_em_1_ano) 


names_numeric = sort(names(select_if(db, is.numeric)))

names <- names(bhb.fecha.numeric)