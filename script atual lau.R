# Mudar isso aqui pela pasta certa no SEU computador
setwd("~/Insper/Insper Data/Dados/Dados atuais")

# Importando a base geral - você precisa ter ela na mesma pasta do wd que colocou aqui em cima
# e precisa escrever o nome do arquivo exatamente como ele é
df <- readxl::read_xlsx('spcultural2 - com analise desc.xlsx', sheet = 1)

# Pacotes obrigatórios para começar

library(ggplot2)
library(tidyverse)
library(dplyr)

#__________________________________________________________________________________________
# Ajeitando os dados 
df <- df %>% 
  mutate(religiao = ifelse(religiao == "1", "Evangélica",
                    ifelse(religiao == "2", "Protestante",
                    ifelse(religiao == "3", "Ev. Pentecostal",
                    ifelse(religiao == "4", "Ev. Não Pentecostal",
                    ifelse(religiao == "5", "Outras Evangélicas",
                    ifelse(religiao == "6", "Umbanda / Candomblé",
                    ifelse(religiao == "7", "Espírita",
                    ifelse(religiao == "8", "Católica",
                    ifelse(religiao == "9", "Judaica",
                    ifelse(religiao == "10", "Outra",
                    ifelse(religiao == "11", "Agnóstico",
                    ifelse(religiao == "12", "Ateu", religiao)))))))))))),
         estcivil = ifelse(estcivil == "1", "Solteiro(a)",
                    ifelse(estcivil == "2", "Casado(a)",
                    ifelse(estcivil == "3", "Viúvo(a)",
                    ifelse(estcivil == "4", "Divorciado(a)", estcivil))))) %>% 
  view()


base_junta <- df %>% 
  mutate(ID = row_number()) %>% 
  select(ID, sexo, idade, cor, rendaf, escola, religiao, estcivil, i_biblio:f_games) 

 
int1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, religiao, estcivil, i_biblio:i_games) %>% 
  pivot_longer(-c(ID:estcivil), names_to="atividade", values_to="nivel_interesse") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


freq1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, religiao, estcivil, f_biblio:f_games) %>% 
  pivot_longer(-c(ID:estcivil), names_to="atividade", values_to="frequencia") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


intfreq <- int1 %>% 
  left_join(freq1, by = c('ID', 'atividade', 'rendaf', 'sexo', 'cor', 'escola', 'idade', 'religiao', 'estcivil'))


#__________________________________________________________________________________________

########## GRÁFICOS ######################################

# Nuvem de palavras - atividades mais frequentadas - geral
library(wordcloud2)

(words <- intfreq %>% 
    mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia),
           atividade = ifelse(atividade == "biblio", "bibliotecas",
                       ifelse(atividade == "livros", "livros",
                       ifelse(atividade == "carnaval", "blocos de carnaval",
                       ifelse(atividade == "musicais", "saraus", 
                       ifelse(atividade == "circo", "circos",
                       ifelse(atividade == "concertos", "concertos de música",
                       ifelse(atividade == "museu", "museus e exposições",
                       ifelse(atividade == "danca", "apresentações de dança",
                       ifelse(atividade == "games", "jogos virtuais",
                       ifelse(atividade == "festas", "festas populares",
                       ifelse(atividade == "shows", "shows", 
                       ifelse(atividade == "teatro", "teatro ou musicais", atividade))))))))))))) %>%
    filter(frequencia == "Último Ano") %>% 
    group_by(atividade) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq)))

wordcloud2(words, minRotation = 0, maxRotation = 0, fontFamily = "Segoe UI",
           backgroundColor = "white", color = "random-dark")

#__________________________________________________________________________________________
##### Piramide de genero - interesses
intfreq %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(x = atividade, fill = sexo, y = ifelse(test = sexo == "masculino", yes = -mediaint, no = mediaint))) + 
  geom_bar(stat = "identity") + scale_y_continuous(labels = abs, limits = max(int1$nivel_interesse) * c(-1,1)) + 
  coord_flip() + labs(title="Interesse pelas Atividades Culturais", subtitle = "De cada grupo de gênero", fill = "Sexo", y="Médio do Nível de Interesse", x = "Atividade Cultural") +
  theme_light()

##### Lollipop genero - interesses
intfreq %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(mediaint, atividade, color = sexo, label = mediaint)) + 
  geom_segment(aes(x = 3, y = atividade, xend = 9, yend = atividade), color = "grey87") +
  geom_point(aes(color=sexo, size=mediaint)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Média do Nível de Interesse", y = "Atividade Cultural",
       title = "Interesse pelas Atividades Culturais", col = "Sexo") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.minor.y = element_line(colour = "grey87"),
        panel.grid.major = element_line(colour = "grey87"),
        plot.caption=element_text(size = 8))


#__________________________________________________________________________________________
################# FREQUÊNCIA DE CONSUMO - sem ponderação

### % de pessoas que consumiram há menos de 1 ano

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))), 
         frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'Último Ano',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
    geom_col(aes(x = fr, y = atividade, fill = educ)) +
    geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
    labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
         title = "% de entrevistados que consumiram no último ano",
         subtitle = "Recorte por Faixas de Escolaridade", 
         fill = "Nível educacional") + 
    theme_classic()

# Recorte por Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras', "outra", cor), 
         frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
  group_by(atividade, frequencia, cor) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'Último Ano',
         cor != 'outra', cor != 'mestiço') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>%
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que consumiram no último ano",
       subtitle = "Recorte por Autodeclaração de Cor", 
       fill = "Cor") + 
  theme_classic()

# Recorte por classe economica
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe")))),
         frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
  group_by(atividade, frequencia, faixasrenda) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'Último Ano') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que consumiram no último ano",
       subtitle = "Recorte por Classe Econômica", 
       fill = "Classe Econômica") + 
  theme_classic()


### EXCLUSÃO SOCIAL -  % de pessoas que nunca foram

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = educ)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Faixas de Escolaridade", 
       fill = "Nível educacional") + 
  theme_classic()

# Recorte por Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras', 
                       "outra", cor)) %>% 
  group_by(atividade, frequencia, cor) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi',
         cor != 'outra', cor != 'mestiço') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>%
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Autodeclaração de Cor", 
       fill = "Cor") + 
  theme_classic()

#Recorte por Sexo
intfreq %>% 
  group_by(atividade, frequencia, sexo) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = sexo)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Sexo", 
       fill = "Sexo") + 
  theme_classic()

#Recorte por Religião
intfreq %>% 
  group_by(atividade, frequencia, religiao) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = religiao)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Religião", 
       fill = "Religião") + 
  theme_classic()

#Recorte por Renda
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                ifelse(rendaf=="R$9.371 a R$18.740", "B",
                ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe"))))) %>% 
  group_by(atividade, frequencia, faixasrenda) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo só 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posição do texto no gráfico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=3) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Classe Econômica", 
       fill = "Classe Econômica") + 
  theme_classic()



#__________________________________________________________________________________________
###### RELAÇÃO ALTO INTERESSE X FREQUÊNCIA - Geral (sem recortes)

  # Criando tabela frequência relativa do consumo no último ano de cada atividade
  tabelafreq <- intfreq %>% 
  mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
  group_by(atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'Último Ano') %>% 
  mutate(frf = fa/3004 * 100) %>% 
  view()
  
  # Criando tabela frequência relativa do alto interesse por cada atividades 
  tabelaint <- intfreq %>% 
  mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", "Alto Interesse", "Médio/Baixo interesse")) %>%
  group_by(atividade, nivel_interesse) %>% 
  summarise(fai = n()) %>% 
  filter(nivel_interesse == 'Alto Interesse') %>% 
  mutate(fri = fai/3004 * 100) %>% 
  view()
  
  # Juntando as duas tabelas e criando a Relação Frequência X Interesse
  
  tabelaintfreq <- tabelaint %>% 
    left_join(tabelafreq, by = c('atividade')) 
  
  tabelaintfreq["FrequenciaxInteresse"] <-((tabelaintfreq$frf/tabelaintfreq$fri) * 100) 

  write.csv(tabelaintfreq, file = "intXfreq geral.csv") # exportando para csv
  
#__________________________________________________________________________________________
###### RELAÇÃO ALTO INTERESSE X FREQUÊNCIA - Com recortes
  
#### Recorte de Renda
  
  tabelaintfreq3 <- tabelaint %>% 
    left_join(tabelafreq, by = c('atividade', 'faixasrenda')) 
  
  tabelaintfreq3["FrequenciaxInteresse"] <-((tabelaintfreq3$frf/tabelaintfreq3$fri) * 100) 
  view(tabelaintfreq3)
  
  write.csv(tabelaintfreq3, file = "InteressexFrequência Renda.csv")

#### Recorte de Escolaridade
  tabelafreq2 <- intfreq %>% 
    mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia),
           educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "1 - Fundamental Incompleto",
                  ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "2 - Fundamental Completo",
                  ifelse(escola=="ensino medio comp"|escola=="superior inc", "3 - Médio Completo",
                  ifelse(escola=="superior comp"|escola=="pos grad", "4 - Superior Completo", "NULL"))))) %>%
    group_by(atividade, frequencia, educ) %>% 
    summarise(fa = n()) %>%
    filter(frequencia == 'Último Ano') %>% 
    mutate(frf = fa/3004 * 100) %>%
    
    tabelaint2 <- intfreq %>% 
    mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", "Alto Interesse", "Médio/Baixo interesse"),
           educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "1 - Fundamental Incompleto",
                         ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "2 - Fundamental Completo",
                                ifelse(escola=="ensino medio comp"|escola=="superior inc", "3 - Médio Completo",
                                       ifelse(escola=="superior comp"|escola=="pos grad", "4 - Superior Completo", "NULL"))))) %>%
    group_by(atividade, nivel_interesse, educ) %>% 
    summarise(fai2 = n()) %>% 
    filter(nivel_interesse == 'Alto Interesse') %>% 
    mutate(fri2 = fai2/3004 * 100) %>% 
    view()
  
  # Juntando as duas tabelas e criando a Relação Frequência X Interesse - escolaridade
  
  tabelaintfreq4 <- tabelaint2 %>% 
    left_join(tabelafreq2, by = c('atividade', 'educ')) 
  
  tabelaintfreq4["FrequenciaxInteresse"] <-((tabelaintfreq4$frf/tabelaintfreq4$fri2) * 100) 
  view(tabelaintfreq4)
  write.csv(tabelaintfreq4, file = "FrequenciaxInteresse Escolaridade.csv")
  
##### Recorte de Idade
  tabelafreq3 <- intfreq %>% 
    mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
    
    group_by(atividade, frequencia, idade) %>% 
    summarise(fa = n()) %>%
    filter(frequencia == 'Último Ano') %>% 
    mutate(frf = fa/3004 * 100) %>% 
    
    tabelaint3 <- intfreq %>% 
    mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", "Alto Interesse", "Médio/Baixo interesse")) %>%
    
    group_by(atividade, nivel_interesse, idade) %>% 
    summarise(fai = n()) %>% 
    filter(nivel_interesse == 'Alto Interesse') %>% 
    mutate(fri3 = fai/3004 * 100) %>%
    view()
  
  # Juntando as duas tabelas e criando a Relação Frequência X Interesse - idade
  
  tabelaintfreq5 <- tabelaint3 %>% 
    left_join(tabelafreq3, by = c('atividade', 'idade')) 
  
  tabelaintfreq5["FrequenciaxInteresse"] <-((tabelaintfreq5$frf/tabelaintfreq5$fri3) * 100) 
  
  
  write.csv(tabelaintfreq5, file = "InteressexFrequência Idade.csv")
  
#####Recorte por estado civil
  tabelafreq6 <- intfreq %>% 
    mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
            
    group_by(atividade, frequencia, estcivil) %>% 
    summarise(fa = n()) %>%
    filter(frequencia == 'Último Ano') %>% 
    mutate(frf = fa/3004 * 100) %>% 
    
    tabelaint6 <- intfreq %>% 
    mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", "Alto Interesse", "Médio/Baixo interesse")) %>%
    
    group_by(atividade, nivel_interesse, estcivil) %>% 
    summarise(fai = n()) %>% 
    filter(nivel_interesse == 'Alto Interesse') %>% 
    mutate(fri6 = fai/3004 * 100) %>%
    view()
  
  #Juntando as duas tabelas e criando a Relação Interesse x Frequência - Estado Civil
  tabelaintfreq7 <- tabelaint6 %>% 
    left_join(tabelafreq6, by = c('atividade', 'estcivil')) 
  
  tabelaintfreq7["FrequenciaxInteresse"] <-((tabelaintfreq7$frf/tabelaintfreq7$fri6) * 100) 
  
  
  write.csv(tabelaintfreq6, file = "InteressexFrequência Estcivil.csv")

  ggplot(tabelaintfreq7, aes(atividade, FrequenciaxInteresse, estcivil)) + 
    geom_hline(yintercept=100) +
    geom_point(aes(fill=estcivil, col=estcivil, size=1)) + 
    labs(title="Distribuição da Frequência x Interesse pelas Atividade Cultural", subtitle="Recorte por estado civil", x="Atividade", y="Frequência / Interesse") + 
    theme_classic() 

# _______________________________________________________________________________________
#### Gráficos de Barra 
## frequência de consumo PONDERADA pelo total de cada faixa de recorte
    
#### Primeiramente: Criando bases para todos os recortes: 
  freqeduc <- intfreq %>% 
    mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                  ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                  ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                  ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
    group_by(educ, atividade) %>% 
    summarise(fae = n()) %>% 
    filter(educ != "NULL") %>%
    group_by(educ) %>% 
    view()
  
  freqclasse <- intfreq %>% 
    mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                         ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                         ifelse(rendaf=="R$9.371 a R$18.740", "B",
                         ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe"))))) %>% 
    group_by(faixasrenda, atividade) %>% 
    summarise(fac = n()) %>% 
    filter(faixasrenda != "Não sabe") %>%
    group_by(faixasrenda) %>% 
    view()          
           
  freqcor <- intfreq %>% 
    mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mestiço', "outra",
                 ifelse(cor == 'parda' | cor == 'preta', "negra", cor))) %>% 
    group_by(cor, atividade) %>% 
    summarise(facor = n()) %>% 
    filter(cor != "outra") %>% 
    group_by(cor) %>% 
    view()
  
  freqsexo <- intfreq %>% 
    group_by(sexo, atividade) %>% 
    summarise(fasex = n()) %>% 
    group_by(sexo) %>% 
    view()
  
  freqidade <- intfreq %>% 
    group_by(idade, atividade) %>% 
    summarise(faid = n()) %>% 
    group_by(idade) %>% 
    view()
  
  freqreligiao <- intfreq %>% 
    group_by(religiao, atividade) %>% 
    summarise(farelig = n()) %>% 
    group_by(religiao) %>% 
    view()
  
  freqestcivil <-intfreq %>% 
    group_by(estcivil, atividade) %>% 
    summarise(faestcivil = n()) %>% 
    group_by(estcivil) %>% 
    view()
   
##### Códigos dos gráficos:
## Primeiro código: "nunca foi" / Segundo código: "Foi há menos de 1 ano"
  
  # Recortes por Escolaridade
    intfreq %>% 
      mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                    ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                    ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                    ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
      left_join(freqeduc, by = c("atividade", "educ")) %>% 
      group_by(atividade, educ, frequencia, fae) %>% 
      summarise(fa = n()) %>%
      filter(educ != "NULL", frequencia == "nunca foi") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fae *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = ifelse(fr > 5, -(cumsum(fr) - 0.5*fr) + sum(fr), -(cumsum(fr) - 0.5*fr) + sum(fr) + 7)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = educ)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada Faixa de Escolaridade", 
           fill = "Nível educacional") + theme_classic() 
    
    intfreq %>% 
      mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                    ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                    ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                    ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))),
             frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
      left_join(freqeduc, by = c("atividade", "educ")) %>% 
      group_by(atividade, educ, frequencia, fae) %>% 
      summarise(fa = n()) %>%
      filter(educ != "NULL", frequencia == "Último Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fae *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = ifelse(fr > 5, -(cumsum(fr) - 0.5*fr) + sum(fr), -(cumsum(fr) - 0.5*fr) + sum(fr) + 7)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = educ)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada Faixa de Escolaridade", 
           fill = "Nível educacional") + theme_classic()   
    
  # Recortes por renda
    intfreq %>% 
      mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                           ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                           ifelse(rendaf=="R$9.371 a R$18.740", "B",
                           ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe"))))) %>% 
      left_join(freqclasse, by = c("atividade", "faixasrenda")) %>% 
      group_by(atividade, faixasrenda, frequencia, fac) %>% 
      summarise(fa = n()) %>%
      filter(faixasrenda != "Não sabe", frequencia == "nunca foi") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fac *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada Classe Social", 
           fill = "Classe Social") + theme_classic()
    
    
    intfreq %>% 
      mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                           ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                           ifelse(rendaf=="R$9.371 a R$18.740", "B",
                           ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe")))),
             frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
      left_join(freqclasse, by = c("atividade", "faixasrenda")) %>% 
      group_by(atividade, faixasrenda, frequencia, fac) %>% 
      summarise(fa = n()) %>%
      filter(faixasrenda != "Não sabe", frequencia == "Último Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fac *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada Classe Social", 
           fill = "Classe Social") + theme_classic()
    
  # Recortes por cor
    intfreq %>% 
      mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mestiço', "outra",
                          ifelse(cor == 'parda' | cor == 'preta', "negra", cor))) %>% 
      left_join(freqcor, by = c("atividade", "cor")) %>% 
      group_by(atividade, cor, frequencia, facor) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "nunca foi", cor != "outra",) %>%
      group_by(atividade) %>% 
      mutate(fr = fa/facor *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = cor)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada grupo de cor", 
           fill = "Autodeclaração de Cor") + theme_classic()
    
    intfreq %>% 
      mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mestiço', "outra",
                   ifelse(cor == 'parda' | cor == 'preta', "negra", cor)),
             frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
      left_join(freqcor, by = c("atividade", "cor")) %>% 
      group_by(atividade, cor, frequencia, facor) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "Último Ano", cor != "outra",) %>%
      group_by(atividade) %>% 
      mutate(fr = fa/facor *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = cor)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada grupo de cor", 
           fill = "Autodeclaração de Cor") + theme_classic()
    
# Recortes por sexo
    intfreq %>% 
      left_join(freqsexo, by = c("atividade", "sexo")) %>% 
      group_by(atividade, sexo, frequencia, fasex) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "nunca foi") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fasex *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = sexo)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada grupo de gênero", 
           fill = "Sexo") + theme_classic()
    
    intfreq %>% 
      mutate (frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
      left_join(freqsexo, by = c("atividade", "sexo")) %>% 
      group_by(atividade, sexo, frequencia, fasex) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "Último Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fasex *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = sexo)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada grupo de gênero", 
           fill = "Sexo") + theme_classic()

# Recortes por religião
    intfreq %>%
      left_join(freqreligiao, by = c("atividade", "religiao")) %>% 
      group_by(atividade, religiao, frequencia, farelig) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "nunca foi") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/farelig *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = religiao)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada grupo Religioso", 
           fill = "Religião") + theme_classic()
    
    intfreq %>%
      mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
      left_join(freqreligiao, by = c("atividade", "religiao")) %>% 
      group_by(atividade, religiao, frequencia, farelig) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "Último Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/farelig *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = religiao)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size = 4) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada grupo Religioso", 
           fill = "Religião") + theme_classic()
    

# Recortes por Estado Civil
    intfreq %>%
      left_join(freqestcivil, by = c("atividade", "estcivil")) %>% 
      group_by(atividade, estcivil, frequencia, faestcivil) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "nunca foi") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/faestcivil *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = estcivil)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que nunca consumiram as Atividades",
           subtitle = "Em cada grupo de Estado Civil", 
           fill = "Estado Civil") + theme_classic()
    
    intfreq %>%
      mutate(frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>% 
      left_join(freqestcivil, by = c("atividade", "estcivil")) %>% 
      group_by(atividade, estcivil, frequencia, faestcivil) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "Último Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/faestcivil *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = estcivil)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size = 4) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no último ano",
           subtitle = "Em cada grupo de Estado Civil", 
           fill = "Estado Civil") + theme_classic()
    
    
#________________________________________________________________________________________
######## % de quem tem ALTO interesse
### Para observar as mudanças de preferências conforme mudam as faixas de recorte
## GRÁFICO DE PONTOS (ponderados)
    
    intfreq %>% 
      mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", 
                                      "Alto Interesse", "Médio/Baixo interesse")) %>%
      left_join(freqidade, by = c("atividade", "idade")) %>% 
      group_by(atividade, nivel_interesse, idade, faid) %>% 
      summarise(fa = n()) %>%
      filter(nivel_interesse == 'Alto Interesse') %>% 
      group_by(atividade) %>% 
      mutate(fr = fa/faid * 100) %>% 
      mutate(fr = round(fr, digits = 1))  %>% 
      ggplot(aes(x = idade, y = atividade)) +
      geom_text(aes(label= fr), vjust=0, hjust=-1, size=3) +
      geom_point(aes(col=atividade, size= fr), show.legend = FALSE) +
      labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
           title = "Alto Interesse nas Atividades Culturais",
           subtitle = "Alto Interesse = 8,9,10 | Recorte por Faixas de Idade", 
           color = "Atividade") + 
      theme_light()
    
# _______________________________________________________________________________________
##### ACESSO À INTERNET  
## Gráfico de Barras 

# Escolaridade
    
df %>% 
mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
              ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
              ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
              ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))),
      p34 = ifelse(p34 == "1", "1 - Todos os Dias, e está sempre conectado",
            ifelse(p34 == "2", "2 - Todos os Dias, mas nem sempre está conectado",
            ifelse(p34 == "3", "3 - Acessa, mas não sempre",
            ifelse(p34 == "4", "4 - Acessa com pouca frequência",
            ifelse(p34 == "96", "5 - Nunca acessa", p34)))))) %>%
group_by(educ, p34) %>%
filter(educ != "NULL") %>% 
summarise(fa = n()) %>% 
mutate(total = sum(fa)) %>%
mutate(fr = fa/total * 100) %>%
mutate(fr=round(fr, digits=1)) %>% 
mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% 
ggplot() + geom_col(aes(x = educ, y = fr, fill = p34)) +
geom_text(aes(x = educ, y = pos, label = fr), size=4) + 
scale_fill_brewer(palette = "Dark2") +
labs(x = "Escolaridade", y = "Porcentagem de pessoas (%)",
    title = "Acesso à Internet", subtitle = "Recorte por Nível de Escolaridade", 
    fill = "Frequência de acesso:") + theme_light()


# Renda 

df %>% 
  mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe")))),
         p34 = ifelse(p34 == "1", "1 - Todos os Dias, e está sempre conectado",
               ifelse(p34 == "2", "2 - Todos os Dias, mas nem sempre está conectado",
               ifelse(p34 == "3", "3 - Acessa, mas não sempre",
               ifelse(p34 == "4", "4 - Acessa com pouca frequência",
               ifelse(p34 == "96", "5- Nunca acessa", p34)))))) %>% 
  group_by(faixasrenda, p34) %>%
  summarise(fa = n()) %>%
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>%
  mutate(fr=round(fr,digits = 1)) %>% 
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>%
  ggplot()+
  geom_col(aes(x= faixasrenda, y= fr, fill= p34))+
  geom_text(aes(x= faixasrenda, y = pos, label = fr), size=4) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Faixa de Renda", y="Porcentagem de pessoas (%)",
       title= "Acesso à internet",
       subtitle = "Recorte por Renda",
       fill="Frequência de acesso")+
  theme_light()

# _______________________________________________________________________________________
# BOXPLOTS
# interesses pelas atividades

# Sexo
ggplot(intfreq, aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=sexo)) + 
  scale_y_continuous(limits=c(0,10)) + 
  labs(title="Distribuição do Interesse pelas Atividade Cultural", subtitle="Recorte por sexo", x="Atividade Cultural", y="Nível de Interesse") + 
  theme_classic() 

# Classes
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe"))))) %>% 
  filter(faixasrenda != "Não sabe") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=faixasrenda)) + 
  scale_y_continuous(limits=c(0,10)) + 
  scale_fill_brewer(palette = "Set1") +
  labs(title="Distribuição do Interesse pelas Atividade Cultural", subtitle="Recorte por Classe Social", x="Atividade Cultural", y="Nível de Interesse") + 
  theme_classic() 

# Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'não sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mestiço', "outra",
               ifelse(cor == 'parda' | cor == 'preta', "negra", cor))) %>% 
  filter(cor != "outra") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=cor)) + 
  scale_y_continuous(limits=c(0,10)) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title="Distribuição do Interesse pelas Atividade Cultural", subtitle="Recorte por Raça", x="Atividade Cultural", y="Nível de Interesse") + 
  theme_classic() 

# Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - Médio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
  filter(educ != "NULL") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=educ)) + 
  scale_y_continuous(limits=c(0,10)) + 
  labs(title="Distribuição do Interesse pelas Atividade Cultural", subtitle="Recorte por Escolaridade", x="Atividade Cultural", y="Nível de Interesse") + 
  theme_classic() 

 #Estado civil
ggplot(intfreq, aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=estcivil)) + 
  scale_y_continuous(limits=c(0,10)) + 
  labs(title="Distribuição do Interesse pelas Atividade Cultural", subtitle="Recorte por estado civil", x="Atividade Cultural", y="Nível de Interesse") + 
  theme_classic() 


# _______________________________________________________________________________________
#ATIVIDADES PAGAS X GRATUITAS

df %>% 
  mutate(p9=ifelse(p9 == "1", "Somente gratuitas",
                   ifelse(p9 == "2", "Mais gratuitas do que pagas",
                          ifelse(p9 == "3", "Mais pagas do que gratuitas", "Somente pagas")))) %>% 
  group_by(p9) %>% 
  summarise(fa= n()) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr = round(fr, digits = 2)) %>% 
  ggplot() +
  geom_col(aes(x=p9, y=fr, fill=p9)) +
  geom_text(aes(x=p9, y=fr,label= fr), vjust=-0.8, hjust=0.7, size=4) +
  ylim(c(0,50)) +
  labs(title = "Frequência de consumo de atividades pagas vs gratuitas",
       x = "",y= "Frequência de consumo (%)", fill="Legenda")+
  theme_light()

# _______________________________________________________________________________________

