
candidatos$occupation = as.character(lapply(strsplit(as.character(candidatos$desc_ocup), split=" "), head, n=1))

occupation$aggreg  <- NA
# Professionals
occupation$aggreg = with(occupation, ifelse(grepl('TRABALHADOR', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('OPERADOR', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('PROFESSOR', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('INSTRUTOR', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('ADMINISTRADOR', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('MEDICO', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('TECNICO', occupation),"Professionals", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('PROTETICO', occupation),"Professionals", aggreg))



# Business
occupation$aggreg = with(occupation, ifelse(grepl('TECNICO', occupation),"Professionals", aggreg))


cand_all$occupation = with(cand_all, ifelse(grepl('Proprietário', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Proprietário', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Proprietário', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Pecuarista', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('EMPRESÁRIO', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Empresário', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Produtor Agropecuário', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Agropecuarista', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('FAZENDEIRO', desc_ocup),"Business", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Fazendeiro', desc_ocup),"Business", occupation))




# State officials
cand_all$occupation = with(cand_all, ifelse(grepl('FUNCIONARIO PUBLICO', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('FUNCIONARIO PÚBLICO', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Funcionário Público', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SERVIDOR PUBLICO', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SERVIDOR PÚBLICO', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Servidor Público', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('MAGISTRADO', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Magistrado', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Judiciário', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Membro do Ministério Público', desc_ocup),"State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SERVENTUARIO', desc_ocup), "State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Serventuário', desc_ocup), "State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PROCURADOR', desc_ocup), "State officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Procurador', desc_ocup), "State officials", occupation))


occupation$aggreg = with(occupation, ifelse(grepl('OCUPANTE DE CARGO EM COMISSAO', occupation),"State officials", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('SERVENTUARIO DE JUSTICA', occupation),"State officials", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('OCUPANTE DE CARGO EM COMISSAO', occupation),"State officials", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('OCUPANTE DE CARGO EM COMISSAO', occupation),"State officials", aggreg))
occupation$aggreg = with(occupation, ifelse(grepl('OCUPANTE DE CARGO EM COMISSAO', occupation),"State officials", aggreg))


# Elected officials
occupation$aggreg = with(occupation, ifelse(grepl('DEPUTADO', occupation),"Elected officials", aggreg))



cand_all$occupation = with(cand_all, ifelse(grepl('Presidente', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Governador', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Prefeito', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SENADOR', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Vereador', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('VEREADOR', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Membros do Poder Legislativo', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Vereador', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Deputados', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Senador', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PRESIDENTE', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PREFEITO', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('GOVERNADOR', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('DEPUTADO', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('DEPUTADOS', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PARLAMENTAR', desc_ocup),"Elected officials", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Parlamentar', desc_ocup),"Elected officials", occupation))


# Military
cand_all$occupation = with(cand_all, ifelse(grepl('Membro das Forças Armadas', desc_ocup),"Military", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Militar Em Geral', desc_ocup),"Military", occupation))


# Pensioners
cand_all$occupation = with(cand_all, ifelse(grepl('APOSENTADO', desc_ocup),"Military", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Aposentado', desc_ocup),"Military", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Pensionista', desc_ocup),"Military", occupation))


# Others
cand_all$occupation = with(cand_all, ifelse(grepl('Outros', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('OUTROS', desc_ocup),"Others", occupation))

cand_all$occupation = with(cand_all, ifelse(grepl('Missionário', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SACERDOTE', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Sacerdote', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Religioso', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Seita Religiosa', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Seitas Religiosas', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('SEITA RELIGIOSA', desc_ocup), "Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('BISPO', desc_ocup), "Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Bispo', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PASTOR', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Pastor', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('PASTORA', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Pastora', desc_ocup),"Others", occupation))

 # Students
cand_all$occupation = with(cand_all, ifelse(grepl('ESTUDANTE', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Estudante', desc_ocup),"Others", occupation))
cand_all$occupation = with(cand_all, ifelse(grepl('Bolsista', desc_ocup),"Others", occupation))


 # Fix the occupation column: