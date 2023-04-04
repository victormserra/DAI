library(dplyr)
library(writexl)
load('SIGRA/Completo16_09_2022.Rdata')

formas.saída = unique(Completo$`Forma Saida`)

df = distinct(Completo) %>%
  filter(Nivel == 'Mestrado' | Nivel == 'Doutorado',
         `Forma Saida` == 'Está Cursando' | 
         `Forma Saida` == 'Formatura Pos-Graduacao' |
         `Forma Saida` == 'Formatura' |
           `Forma Saida` == 'Formatura Anterior a 1/88' ) %>%
  group_by(Pais) %>%
  summarize(`Quantidade de Alunos` = n()) %>%
  rename(País = Pais) %>%
  arrange(desc(`Quantidade de Alunos`))

write_xlsx(df, 'C:\\Users\\04291229186\\Desktop\\Trabalho\\SIGRA\\Quantidade de Mestrado ou Dr por País.xlsx')
