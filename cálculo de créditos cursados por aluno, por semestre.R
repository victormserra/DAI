# Intenção do Script:
# Calcular a quantidade de créditos cursada por cada aluno, nos semestres entre 2017 e 2022
# Cada histórico vem somente com a matrícula do aluno, o que é um problema:
# Como os alunos mudam de matrícula se trocam de curso ou saem e retornam à UnB, o "group_by" precisa ser por nome ou cpf
# Daí a gente precisa pegar uma base de informações dos alunos, com nome, cpf e matrícula
# E então dar cruzar essa base com cada um dos históricos
# A partir desses históricos com nome, a gente calcula créditos cursados pra cada nome diferente, usando group_by(nome, cpf, matricula)
# Enfim, a gente dá um full_join em todas as tabelas de créditos, pra ter um histórico completo
# E o último passo é reagregar outros dados identificadores
# O que eu percebi: o cpf é sempre o mesmo exceto por alguns zeros no começo, a depender da base utilizada
# E a matrícula a ser utilizada deve ser sempre a mais recente, quando há mais de uma matrícula
# O resultado disso é que basta dar um group_by(nome, cpf, matricula), sem se importar com a matrícula
# o cpf no group_by() é preciso só pra separar pessoas com o mesmo nome
#-------------------------------------------------------------------------------
# pacotes e dados de identificação dos alunos
library(data.table)
library(dplyr)
setwd('C:/Users/04291229186/Desktop/Trabalho/Planilhas')

informacoes.alunos.siger = readxl::read_xlsx('Siger_Censo_Informacoes_Alunos.xlsx')
informacoes.alunos.siger = informacoes.alunos.siger %>%
  rename(matricula = Aluno, nome = Nome, cpf = CPF) %>%
  select(matricula, nome, cpf) %>%
  mutate(nome = stringr::str_to_upper(nome)) %>%
  unique()

informacoes.alunos.sigaa = readxl::read_xlsx('completo_sigaa_14032023.xlsx')
informacoes.alunos.sigaa = informacoes.alunos.sigaa %>%
  select(matricula, nome, cpf) %>%
  mutate(nome = stringr::str_to_upper(nome)) %>%
  unique()

informacoes.alunos = rbind(informacoes.alunos.sigaa, informacoes.alunos.siger) %>%
  mutate(nome = stringr::str_to_upper(nome)) %>%
  unique()

#-------------------------------------------------------------------------------
# Abaixo que começa de fato a geração das bases de dados
#-------------------------------------------------------------------------------
# 20171
hist.20171 = readxl::read_xlsx('Siger_Censo_Historicos_Escolares_Alunos_20171.xlsx') %>%
  rename(nome_disciplina = nome) 
  
hist.20171 = left_join(hist.20171, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
    mutate(nome = stringr::str_to_upper(nome))

creditos.20171 = hist.20171 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20171 = sum(creditos))
  
rm(hist.20171)
gc()

#-------------------------------------------------------------------------------
# 20172
hist.20172 = fread('Siger_Censo_Historicos_Escolares_Alunos_20172.csv') %>%
  rename(nome_disciplina = nome)

hist.20172$matricula = as.character(hist.20172$matricula)

hist.20172 = left_join(hist.20172, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf),
         matricula = as.character(matricula))

creditos.20172 = hist.20172 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20172 = sum(creditos))

rm(hist.20172)
gc()

#-------------------------------------------------------------------------------
# 20181
hist.20181 = fread('Siger_Censo_Historicos_Escolares_Alunos_20181.csv') %>%
  rename(nome_disciplina = nome)

hist.20181$matricula = as.character(hist.20181$matricula)

hist.20181 = left_join(hist.20181, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20181 = hist.20181 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20181 = sum(creditos))

rm(hist.20181)
gc()

#-------------------------------------------------------------------------------
# 20182
hist.20182 = fread('Siger_Censo_Historicos_Escolares_Alunos_20182.csv') %>%
  rename(nome_disciplina = nome)

hist.20182$matricula = as.character(hist.20182$matricula)

hist.20182 = left_join(hist.20182, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20182 = hist.20182 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20182 = sum(creditos))

rm(hist.20182)
gc()

#-------------------------------------------------------------------------------
# 20191
hist.20191 = fread('Siger_Censo_Historicos_Escolares_Alunos_20191.csv') %>%
  rename(nome_disciplina = nome)

hist.20191$matricula = as.character(hist.20191$matricula)

hist.20191 = left_join(hist.20191, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20191 = hist.20191 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20191 = sum(creditos))

rm(hist.20191)
gc()

#-------------------------------------------------------------------------------
# 20192
hist.20192 = fread('Siger_Censo_Historicos_Escolares_Alunos_20192.csv') %>%
  rename(nome_disciplina = nome)

hist.20192$matricula = as.character(hist.20192$matricula)

hist.20192 = left_join(hist.20192, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20192 = hist.20192 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20192 = sum(creditos))

rm(hist.20192)
gc()

#-------------------------------------------------------------------------------
# 20201
hist.20201 = fread('Siger_Censo_Historicos_Escolares_Alunos_20201.csv') %>%
  rename(nome_disciplina = nome)

hist.20201$matricula = as.character(hist.20201$matricula)

hist.20201 = left_join(hist.20201, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20201 = hist.20201 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20201 = sum(creditos))

rm(hist.20201)
gc()

#-------------------------------------------------------------------------------
# 20202
hist.20202 = fread('Siger_Censo_Historicos_Escolares_Alunos_20202.csv') %>%
  rename(nome_disciplina = nome)

hist.20202$matricula = as.character(hist.20202$matricula)

hist.20202 = left_join(hist.20202, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20202 = hist.20202 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20202 = sum(creditos))

rm(hist.20202)
gc()

#-------------------------------------------------------------------------------
# 20211
hist.20211 = fread('Siger_Censo_Historicos_Escolares_Alunos_20211.csv') %>%
  rename(nome_disciplina = nome)

hist.20211$matricula = as.character(hist.20211$matricula)

hist.20211 = left_join(hist.20211, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20211 = hist.20211 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20211 = sum(creditos))

rm(hist.20211)
gc()

#-------------------------------------------------------------------------------
# 20212
hist.20212 = fread('Siger_Censo_Historicos_Escolares_Alunos_20212.csv') %>%
  rename(nome_disciplina = nome)

hist.20212$matricula = as.character(hist.20212$matricula)

hist.20212 = left_join(hist.20212, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20212 = hist.20212 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20212 = sum(creditos))

rm(hist.20212)
gc()

#-------------------------------------------------------------------------------
# 20221
hist.20221 = fread('Siger_Censo_Historicos_Escolares_Alunos_20221.csv') %>%
  rename(nome_disciplina = nome)

hist.20221$matricula = as.character(hist.20221$matricula)

hist.20221 = left_join(hist.20221, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20221 = hist.20221 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20221 = sum(creditos))

rm(hist.20221)
gc()

#-------------------------------------------------------------------------------
# 20222
hist.20222 = data.table::fread('Siger_Censo_Historicos_Escolares_Alunos_20222.csv') %>%
  rename(nome_disciplina = nome)

hist.20222$matricula = as.character(hist.20222$matricula)

hist.20222 = left_join(hist.20222, informacoes.alunos) %>%
  select(nome, cpf, matricula:frequencia) %>%
  mutate(nome = stringr::str_to_upper(nome),
         cpf = as.character(cpf))

creditos.20222 = hist.20222 %>%
  mutate(creditos = as.numeric(creditos)) %>%
  filter(mencao == 'MM' | mencao == 'MS' | mencao == 'SS' | mencao == 'CC') %>%
  group_by(nome, cpf, matricula) %>%
  summarize(creditos_20222 = sum(creditos))

rm(hist.20222)
gc()

#-------------------------------------------------------------------------------
# Junção dos créditos por semestre pra formar as bases de interesse, esse é o produto final do script

creditos_2017_2022 = full_join(creditos.20171, creditos.20172) %>%
  full_join(creditos.20181) %>%
  full_join(creditos.20182) %>%
  full_join(creditos.20191) %>%
  full_join(creditos.20192) %>%
  full_join(creditos.20201) %>%
  full_join(creditos.20202) %>%
  full_join(creditos.20211) %>%
  full_join(creditos.20212) %>%
  full_join(creditos.20221) %>%
  full_join(creditos.20222)

creditos_2017_2022 = left_join(creditos_2017_2022, informacoes.alunos) %>%
  select(nome, cpf, matricula, creditos_20171:creditos_20222)

# a base 'agregado' junta os créditos por pessoa e cpf, desconsiderando as matrículas diferentes para as mesmas pessoas
creditos_2017_2022_agregado = creditos_2017_2022 %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  group_by(nome, cpf) %>%
  summarize(
    creditos_20171 = sum(creditos_20171),
    creditos_20172 = sum(creditos_20172),
    creditos_20181 = sum(creditos_20181),
    creditos_20182 = sum(creditos_20182),
    creditos_20191 = sum(creditos_20191),
    creditos_20192 = sum(creditos_20192),
    creditos_20201 = sum(creditos_20201),
    creditos_20202 = sum(creditos_20202),
    creditos_20211 = sum(creditos_20211),
    creditos_20212 = sum(creditos_20212),
    creditos_20221 = sum(creditos_20221),
    creditos_20222 = sum(creditos_20222)
  )

# tabelas pra ver os nomes que apresentam mais de uma matrícula na base
quant.nomes = as.data.frame(table(creditos_2017_2022$nome))
quant.nomes.mult = quant.nomes %>% filter(Freq > 1)

#-------------------------------------------------------------------------------
# criando as variáveis de créditos acumulados por ano, o guilherme vai querer

creditos_2017_2022 = creditos_2017_2022 %>%
  mutate(
    creditos_acumulado_2017 = sum(creditos_20171, creditos_20172),
    creditos_acumulado_2018 = sum(creditos_acumulado_2017, creditos_20181, creditos_20182),
    creditos_acumulado_2019 = sum(creditos_acumulado_2018, creditos_20191, creditos_20192),
    creditos_acumulado_2020 = sum(creditos_acumulado_2019, creditos_20201, creditos_20202),
    creditos_acumulado_2021 = sum(creditos_acumulado_2020, creditos_20211, creditos_20212),
    creditos_acumulado_2022 = sum(creditos_acumulado_2021, creditos_20221, creditos_20222)
  )

creditos_2017_2022_agregado = creditos_2017_2022_agregado %>%
  mutate(
    creditos_acumulado_2017 = sum(creditos_20171, creditos_20172),
    creditos_acumulado_2018 = sum(creditos_acumulado_2017, creditos_20181, creditos_20182),
    creditos_acumulado_2019 = sum(creditos_acumulado_2018, creditos_20191, creditos_20192),
    creditos_acumulado_2020 = sum(creditos_acumulado_2019, creditos_20201, creditos_20202),
    creditos_acumulado_2021 = sum(creditos_acumulado_2020, creditos_20211, creditos_20212),
    creditos_acumulado_2022 = sum(creditos_acumulado_2021, creditos_20221, creditos_20222)
  )

#-------------------------------------------------------------------------------
# exportação

writexl::write_xlsx(creditos_2017_2022, 'créditos_2017_2022.xlsx')

writexl::write_xlsx(creditos_2017_2022_agregado, 'créditos_2017_2022_agregado.xlsx')
