# Convert R$ to US$; currency on 05/10/2014 the election day ###
# Data cotação utilizada: 06/10/2014
# Taxa:
# 1 REAL BRASIL/BRL (790) = 0,4149722 DOLAR DOS EUA/USD (220)
# 1 DOLAR DOS EUA/USD (220) = 2,4098 REAL BRASIL/BRL (790)

USS = 0.4149722

receitas %>%
  group_by(ano_eleicao, uf, desc_unid_elei, desc_cargo, num_cand, nome_cand) %>%
  summarize(BRreceitas = sum(receita, na.rm=TRUE), n = n() ) %>%
  ungroup() %>%
  group_by(ano_eleicao, uf, desc_unid_elei, desc_cargo) %>%
  mutate(ufreceitas = sum(BRreceitas, na.rm=TRUE))%>%
  ungroup() -> receita_agreg


# Currency deflator ####
receita_agreg$deflator = NA
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==1994,4.3079417,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==1996,3.0378662,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==1998,2.8210468,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2000,2.4875147,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2002,2.1151837,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2004,1.6987911,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2006,1.5729311,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2008,1.4005731,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2010,1.2809865,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2012,1.1307950,receita_agreg$deflator)
receita_agreg$deflator = ifelse(receita_agreg$ano_eleicao==2014,1,receita_agreg$deflator)
receita_agreg = mutate(receita_agreg, district_share = (BRreceitas/ufreceitas))
receita_agreg = mutate(receita_agreg, BRreceitas_deflat = (BRreceitas*deflator))
receita_agreg = mutate(receita_agreg, USreceitas = (BRreceitas_deflat*USS))

receita_agreg %>% group_by(ano_eleicao, desc_cargo) %>%
  summarize( mean(USreceitas) ) %>% View()

  