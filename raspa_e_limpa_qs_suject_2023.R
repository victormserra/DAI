################################################################################
# Explicação: Os rankings publicados pela QS estão em formato de tabela dinâmica,
# ou seja, são carregados na medida que o usuário vai interagindo com a tabela e solicitando a apresentação de mais dados
# Assim, os dados são trazidos de algum repositório e carregados na medida que o usuário demanda
# Essa característica faz com que não seja possível raspar os dados usando técnicas mais elementares. É preciso raspar do repositório
# No caso, os dados do ranking vêm de um JSON, sigla de "Java Script Objet Notation"
# Não só isso, a página em que está o JSON desses dados não é uma informação pública
# Então é preciso descobrir esse "link secreto"
# A forma mais direta é usando a função "Ferramentas de Desenvolvedor" do Google Chrome
################################################################################

# Verifica se o pacote jsonlite está instalado. Se não estiver, instala
if(!require('jsonlite')){install.packages('jsonlite')}
library(jsonlite)

#----------------------------Raspa os dados-------------------------------------
# qs_ah = QS Arts and Humanities
qs_ah = fromJSON('https://www.topuniversities.com/rankings/endpoint?nid=3846211&page=0&items_per_page=521&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=&order_by=')[[5]]

# qs_et = QS Engineering and Techonology
qs_et = fromJSON('https://www.topuniversities.com/rankings/endpoint?nid=3846212&page=0&items_per_page=533&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=&order_by=')[[5]]

# qs_ls = QS Life Sciences and Medicine
qs_ls = fromJSON('https://www.topuniversities.com/rankings/endpoint?nid=3846213&page=0&items_per_page=521&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=&order_by=')[[5]]

# qs_ns = QS Natural Sciences
qs_ns = fromJSON('https://www.topuniversities.com/rankings/endpoint?nid=3846219&page=0&items_per_page=520&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=&order_by=')[[5]]

# qs_ss = QS Social Sciences & Management
qs_ss = fromJSON('https://www.topuniversities.com/rankings/endpoint?nid=3846220&page=0&items_per_page=535&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=&order_by=')[[5]]

# Os dados importados da fonte não vêm bonitinhos como o site apresenta. 
# É preciso fazer manipulações e tratamentos pra chegar no produto final correto, o que fazemos abaixo

##################QS Arts and Humanitites#######################################
# Tratamento dos dados do QS Arts and Humanities

# Seleciona as informações relevantes pro nosso ranking
qs_ah = qs_ah %>%
  mutate(Ano = '2023') %>%
  select(Ano,
         Colocação = rank,
         Instituição = title,
         Pais = country,
         `Overall Score` = overall_score,
         scores = scores)

# Coleta os dados de H-Index, que estão armazenados em um dataframe dentro de uma lista
hindex = numeric(0)
for(i in 1:nrow(qs_ah)){
  hindex[i] = qs_ah[[6]][[i]]$score[4]
}

# Coleta os dados de Citations per Paper
citations_per_paper = numeric(0)
for(i in 1:nrow(qs_ah)){
  citations_per_paper[i] = qs_ah[[6]][[i]]$score[3]
}

# Coleta os dados de Academic Reputation
academic_reputation = numeric(0)
for(i in 1:nrow(qs_ah)){
  academic_reputation[i] = qs_ah[[6]][[i]]$score[1]
}

# Coleta os dados de Employer Reputation
employer_reputation = numeric(0)
for(i in 1:nrow(qs_ah)){
  employer_reputation[i] = qs_ah[[6]][[i]]$score[2]
}

# Coleta os dados de International Research Network
international_research_network = numeric(0)
for(i in 1:nrow(qs_ah)){
  international_research_network[i] = qs_ah[[6]][[i]]$score[5]
}

# Transforma os quesitos de avaliação em data frame
scores = data.frame(`H-Index` = hindex, 
                    `Citations per Paper` = citations_per_paper, 
                    `Academic Reputation` = academic_reputation, 
                    `Employer Reputation` = employer_reputation, 
                    `International Research Network` = international_research_network)

# Junta os quesitos de avaliação com o resto dos dados
qs_ah = cbind(qs_ah, scores)

# Dá o tratamento final, removendo a coluna que não é mais necessária e renomeando pra ficar direitinho
qs_ah = qs_ah %>%
  select(- scores) %>%
  rename(`H-Index` = H.Index,
         `Citations per Paper` = Citations.per.Paper,
          `Academic Reputation` = Academic.Reputation,
         `Employer Reputation` = Employer.Reputation,
         `International Research Network` = International.Research.Network)

# Transformando os dados numéricos pra formato numérico
qs_ah[ ,5:10] = sapply(qs_ah[ ,5:10], as.numeric)


##################QS Natural Sciences###########################################
# Tratamento dos dados do QS Natural Sciences

# Seleciona as informações relevantes pro nosso ranking
qs_ns = qs_ns %>%
  mutate(Ano = '2023') %>%
  select(Ano,
         Colocação = rank,
         Instituição = title,
         Pais = country,
         `Overall Score` = overall_score,
         scores = scores)

# Coleta os dados de H-Index, que estão armazenados em um dataframe dentro de uma lista
hindex = numeric(0)
for(i in 1:nrow(qs_ns)){
  hindex[i] = qs_ns[[6]][[i]]$score[4]
}

# Coleta os dados de Citations per Paper
citations_per_paper = numeric(0)
for(i in 1:nrow(qs_ns)){
  citations_per_paper[i] = qs_ns[[6]][[i]]$score[3]
}

# Coleta os dados de Academic Reputation
academic_reputation = numeric(0)
for(i in 1:nrow(qs_ns)){
  academic_reputation[i] = qs_ns[[6]][[i]]$score[1]
}

# Coleta os dados de Employer Reputation
employer_reputation = numeric(0)
for(i in 1:nrow(qs_ns)){
  employer_reputation[i] = qs_ns[[6]][[i]]$score[2]
}

# Coleta os dados de International Research Network
international_research_network = numeric(0)
for(i in 1:nrow(qs_ns)){
  international_research_network[i] = qs_ns[[6]][[i]]$score[5]
}

# Transforma os quesitos de avaliação em data frame
scores = data.frame(`H-Index` = hindex, 
                    `Citations per Paper` = citations_per_paper, 
                    `Academic Reputation` = academic_reputation, 
                    `Employer Reputation` = employer_reputation, 
                    `International Research Network` = international_research_network)

# Junta os quesitos de avaliação com o resto dos dados
qs_ns = cbind(qs_ns, scores)

# Dá o tratamento final, removendo a coluna que não é mais necessária e renomeando pra ficar direitinho
qs_ns = qs_ns %>%
  select(- scores) %>%
  rename(`H-Index` = H.Index,
         `Citations per Paper` = Citations.per.Paper,
         `Academic Reputation` = Academic.Reputation,
         `Employer Reputation` = Employer.Reputation,
         `International Research Network` = International.Research.Network)

# Transformando os dados numéricos pra formato numérico
qs_ns[ ,5:10] = sapply(qs_ns[ ,5:10], as.numeric)

##################QS Engineering & Technologies#################################
# Tratamento dos dados do QS Engineering & Technologies

# Seleciona as informações relevantes pro nosso ranking
qs_et = qs_et %>%
  mutate(Ano = '2023') %>%
  select(Ano,
         Colocação = rank,
         Instituição = title,
         Pais = country,
         `Overall Score` = overall_score,
         scores = scores)

# Coleta os dados de H-Index, que estão armazenados em um dataframe dentro de uma lista
hindex = numeric(0)
for(i in 1:nrow(qs_et)){
  hindex[i] = qs_et[[6]][[i]]$score[4]
}

# Coleta os dados de Citations per Paper
citations_per_paper = numeric(0)
for(i in 1:nrow(qs_et)){
  citations_per_paper[i] = qs_et[[6]][[i]]$score[3]
}

# Coleta os dados de Academic Reputation
academic_reputation = numeric(0)
for(i in 1:nrow(qs_et)){
  academic_reputation[i] = qs_et[[6]][[i]]$score[1]
}

# Coleta os dados de Employer Reputation
employer_reputation = numeric(0)
for(i in 1:nrow(qs_et)){
  employer_reputation[i] = qs_et[[6]][[i]]$score[2]
}

# Coleta os dados de International Research Network
international_research_network = numeric(0)
for(i in 1:nrow(qs_et)){
  international_research_network[i] = qs_et[[6]][[i]]$score[5]
}

# Transforma os quesitos de avaliação em data frame
scores = data.frame(`H-Index` = hindex, 
                    `Citations per Paper` = citations_per_paper, 
                    `Academic Reputation` = academic_reputation, 
                    `Employer Reputation` = employer_reputation, 
                    `International Research Network` = international_research_network)

# Junta os quesitos de avaliação com o resto dos dados
qs_et = cbind(qs_et, scores)

# Dá o tratamento final, removendo a coluna que não é mais necessária e renomeando pra ficar direitinho
qs_et = qs_et %>%
  select(- scores) %>%
  rename(`H-Index` = H.Index,
         `Citations per Paper` = Citations.per.Paper,
         `Academic Reputation` = Academic.Reputation,
         `Employer Reputation` = Employer.Reputation,
         `International Research Network` = International.Research.Network)

# Transformando os dados numéricos pra formato numérico
qs_et[ ,5:10] = sapply(qs_et[ ,5:10], as.numeric)


##################QS Life Sciences and Medicine#################################
# Tratamento dos dados do QS Life Sciences and Medicine

# Seleciona as informações relevantes pro nosso ranking
qs_ls = qs_ls %>%
  mutate(Ano = '2023') %>%
  select(Ano,
         Colocação = rank,
         Instituição = title,
         Pais = country,
         `Overall Score` = overall_score,
         scores = scores)

# Coleta os dados de H-Index, que estão armazenados em um dataframe dentro de uma lista
hindex = numeric(0)
for(i in 1:nrow(qs_ls)){
  hindex[i] = qs_ls[[6]][[i]]$score[4]
}

# Coleta os dados de Citations per Paper
citations_per_paper = numeric(0)
for(i in 1:nrow(qs_ls)){
  citations_per_paper[i] = qs_ls[[6]][[i]]$score[3]
}

# Coleta os dados de Academic Reputation
academic_reputation = numeric(0)
for(i in 1:nrow(qs_ls)){
  academic_reputation[i] = qs_ls[[6]][[i]]$score[1]
}

# Coleta os dados de Employer Reputation
employer_reputation = numeric(0)
for(i in 1:nrow(qs_ls)){
  employer_reputation[i] = qs_ls[[6]][[i]]$score[2]
}

# Coleta os dados de International Research Network
international_research_network = numeric(0)
for(i in 1:nrow(qs_ls)){
  international_research_network[i] = qs_ls[[6]][[i]]$score[5]
}

# Transforma os quesitos de avaliação em data frame
scores = data.frame(`H-Index` = hindex, 
                    `Citations per Paper` = citations_per_paper, 
                    `Academic Reputation` = academic_reputation, 
                    `Employer Reputation` = employer_reputation, 
                    `International Research Network` = international_research_network)

# Junta os quesitos de avaliação com o resto dos dados
qs_ls = cbind(qs_ls, scores)

# Dá o tratamento final, removendo a coluna que não é mais necessária e renomeando pra ficar direitinho
qs_ls = qs_ls %>%
  select(- scores) %>%
  rename(`H-Index` = H.Index,
         `Citations per Paper` = Citations.per.Paper,
         `Academic Reputation` = Academic.Reputation,
         `Employer Reputation` = Employer.Reputation,
         `International Research Network` = International.Research.Network)

# Transformando os dados numéricos pra formato numérico
qs_ls[ ,5:10] = sapply(qs_ls[ ,5:10], as.numeric)


##################QS Social Sciences & Management#################################
# Tratamento dos dados do QS Social Sciences & Management

# Seleciona as informações relevantes pro nosso ranking
qs_ss = qs_ss %>%
  mutate(Ano = '2023') %>%
  select(Ano,
         Colocação = rank,
         Instituição = title,
         Pais = country,
         `Overall Score` = overall_score,
         scores = scores)

# Coleta os dados de H-Index, que estão armazenados em um dataframe dentro de uma lista
hindex = numeric(0)
for(i in 1:nrow(qs_ss)){
  hindex[i] = qs_ss[[6]][[i]]$score[4]
}

# Coleta os dados de Citations per Paper
citations_per_paper = numeric(0)
for(i in 1:nrow(qs_ss)){
  citations_per_paper[i] = qs_ss[[6]][[i]]$score[3]
}

# Coleta os dados de Academic Reputation
academic_reputation = numeric(0)
for(i in 1:nrow(qs_ss)){
  academic_reputation[i] = qs_ss[[6]][[i]]$score[1]
}

# Coleta os dados de Employer Reputation
employer_reputation = numeric(0)
for(i in 1:nrow(qs_ss)){
  employer_reputation[i] = qs_ss[[6]][[i]]$score[2]
}

# Coleta os dados de International Research Network
international_research_network = numeric(0)
for(i in 1:nrow(qs_ss)){
  international_research_network[i] = qs_ss[[6]][[i]]$score[5]
}

# Transforma os quesitos de avaliação em data frame
scores = data.frame(`H-Index` = hindex, 
                    `Citations per Paper` = citations_per_paper, 
                    `Academic Reputation` = academic_reputation, 
                    `Employer Reputation` = employer_reputation, 
                    `International Research Network` = international_research_network)

# Junta os quesitos de avaliação com o resto dos dados
qs_ss = cbind(qs_ss, scores)

# Dá o tratamento final, removendo a coluna que não é mais necessária e renomeando pra ficar direitinho
qs_ss = qs_ss %>%
  select(- scores) %>%
  rename(`H-Index` = H.Index,
         `Citations per Paper` = Citations.per.Paper,
         `Academic Reputation` = Academic.Reputation,
         `Employer Reputation` = Employer.Reputation,
         `International Research Network` = International.Research.Network)

# Transformando os dados numéricos pra formato numérico
qs_ss[ ,5:10] = sapply(qs_ss[ ,5:10], as.numeric)


#################################################################################
# Exportando uma planilha com todos os rankings

writexl::write_xlsx(list(qs_ns, qs_ah, qs_et, qs_ls, qs_ss), 'E:/Trabalho/Ranking QS Subjects 2023.xlsx')
