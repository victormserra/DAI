# Pacotes
library(dplyr)
library(writexl)
library(stringr)
library(data.table)
#-------------------------------------------------------------------------------
# Dados
load('C:\\Users\\04291229186\\Desktop\\Trabalho\\Planilhas\\Completo02_01_2023.Rdata') # Dados SIGRA
graduação = readxl::read_xlsx('C:\\Users\\04291229186\\Desktop\\Trabalho\\Planilhas\\Completo_SIGAA_02012023.xlsx')
#-------------------------------------------------------------------------------
# graduação - alunos registrados presencial e EaD

graduação %>%
  filter(status_aluno == 'ATIVO' | status_aluno == 'ATIVO - FORMANDO',
         !(ano_ingresso == '2022' & periodo_ingresso == '2')) %>% # retira não registrados, retira os q ingressaram no 2º de 2022
  distinct(cpf, .keep_all = TRUE) %>% # retira dupla contagem de alunos
  group_by(modalidade) %>% # agrupa por modalidade: presencial e EaD
  summarize(n())

# obs: não dá pra separar por semestres 1º/22 e 2º/22 porque o 2º/22 não tá fechado

#-------------------------------------------------------------------------------
# graduação - quantidade de cursos ofertados presencial e EaD

graduação %>%
  select(nome_curso, modalidade) %>%
  group_by(modalidade) %>%
  distinct() %>%
  summarize(n())

#-------------------------------------------------------------------------------
# graduação - quantidade de diplomados em 2022

graduação %>%
  filter(ano_referencia_movimentacao == '2022',
         tipo_movimentacao_aluno == 'CONCLUÍDO') %>%
  summarize(n())

# não dá pra informar porque os dados não foram atualizados no sistema, não consta nenhuma movimentação em 2022

#-------------------------------------------------------------------------------
#----------------------------SIGRA----------------------------------------------
#-------------------------------------------------------------------------------

mestrado = Completo %>% filter(Nivel == 'Mestrado')
doutorado = Completo %>% filter(Nivel == 'Doutorado')
residencia = Completo %>% filter(Nivel == 'Residencia')

#-------------------------------------------------------------------------------
# alunos registrados
Completo %>%
  group_by(Nivel) %>%
  filter(`Forma Saida` == 'Está Cursando') %>%
  summarize(n())

# quantidade de cursos
df=Completo %>%
  select(Nivel, `Nome Opcao`, `Forma Saida`) %>%
  group_by(Nivel) %>%
  filter(`Forma Saida` == 'Está Cursando') %>%
  distinct(`Nome Opcao`)

%>%
  summarize(n())
  
# diplomados em 2022
Completo %>%
  select(Nivel, `Ano Sa`)
  group_by(Nivel) %>%
  filter(`Ano Saida` == '2022')

%>%
  summarize(n())
  
residencia2 = residencia %>%
  filter(`Forma Saida` == 'Formatura com Especializacao')




# retirar os pré-cadastrados
# retirar os não cadastrados
# retirar cancelados antes de 2021
# retirar formados antes de 2021
# fazer distinct por cpf

# OBS: O total de matriculados na graduação, calculado aqui, não bate com o total registrado no sistema
# Então tem algum problema nos esquemas

graduação1 = graduação %>%
 filter(status_aluno == 'ATIVO' | status_aluno == 'ATIVO - FORMANDO') %>%
 distinct(cpf, .keep_all = TRUE) %>%
 dplyr::summarize(Nível = 'Graduação', `Quantidade de Alunos` = n(), `Data da Consulta` = '16/09/2022')

graduação2 = graduação %>%
  filter(status_aluno == 'ATIVO' | status_aluno == 'ATIVO - FORMANDO',
         status_aluno != 'PRÉ-CADASTRADO',
         status_aluno != 'NÃO CADASTRADO',
         !(status_aluno == 'CANCELADO' & ano_referencia_movimentacao == '2019'),
         !(status_aluno == 'FORMADO' & ano_referencia_movimentacao == '2019'),
         !(status_aluno == 'CANCELADO' & ano_referencia_movimentacao == '2020'),
         !(status_aluno == 'FORMADO' & ano_referencia_movimentacao == '2020')) %>%
  distinct(cpf, .keep_all = TRUE) %>%
  summarize(Nível = 'Graduação', `Quantidade de Alunos` = n(), `Data da Consulta` = '09/01/2023')

# Total do Mestrado, Doutorado, Residência
pós = Completo %>%
  distinct(CPF, .keep_all = TRUE) %>%
  filter(`Forma Saida` == 'Está Cursando' | 
         `Forma Saida` == 'Vestibular p/mesma Habilitacao' |
           `Forma Saida` == 'Mudanca de Turno' |
           `Forma Saida` == 'Mudanca de Habilitacao' |
           `Forma Saida` == 'Vestibular p/outra Habilitacao') %>%
  group_by(Nivel) %>%
  summarize(`Quantidade de Alunos` = n(),
                   `Data da Consulta` = '09/01/2023') %>%
  rename(Nível = Nivel)

residencia = Completo %>%
  distinct(CPF, .keep_all = TRUE) %>%
  filter(Nivel == 'Residencia', 
         `Ano Saida` == '2022', 
         `Forma Saida` == 'Formatura com Especializacao') %>%
  group_by(`Semestre Saida`) %>%
  tally()

# Combina as tabelas
graduação_e_pós = rbind(graduação, pós)

# Exportação
write_xlsx(graduação_e_pós, 
           'C:\\Users\\04291229186\\Desktop\\Trabalho\\Pedidos Externos\\UnB - Graduação e Pós-Graduação Ativos.xlsx')
