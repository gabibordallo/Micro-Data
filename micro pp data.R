setwd("~/Insper Data/Micro - PP")

# Pacotes obrigatórios para começar

library(ggplot2)
library(tidyverse)
library(dplyr)


#### BASES 
# spculturalgeral - base grande e original, sem mudanças
# spcultural - base filtrada com características de recorte + freq + interesse
# interesses - só as caracteristicas socio + interesse
# frequencia - só as caracteristicas socio + frequencia
# intfreq - interesse e frequencia juntos e ajeitadas pelo pivot longer


df <- readxl::read_xlsx('spcultural2 - com analise desc.xlsx', sheet = 1)


#### NUVEM DE PALAVRAS ####
library(wordcloud2)
library(tm)

# Atividades mais faladas - pergunta xp1 
# O que mais costuma fazer no tempo livre, quando não está trabalhando ou estudando?"

(words <- spculturalgeral %>% 
    filter(!is.na(xp1)) %>% 
    select(xp1) %>% 
    group_by(xp1) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq)))

wordcloud2(words, size = 1.0, shape="circle", ellipticity = 2.5, color='random-dark', shuffle = TRUE, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

# Ajeitando os dados 

base_junta <- df %>% 
  mutate(ID = row_number()) %>% 
  select(ID, sexo, idade, cor, rendaf, escola, i_biblio:f_games) 

 
  pivot_longer(c(i_biblio:f_games),  names_to="atividade", values_to="nivel_interesse")


int1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, i_biblio:i_games) %>% 
  pivot_longer(-c(ID:escola), names_to="atividade", values_to="nivel_interesse") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


freq1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, f_biblio:f_games) %>% 
  pivot_longer(-c(ID:escola), names_to="atividade", values_to="frequencia") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


intfreq <- int1 %>% 
  left_join(freq1, by = c('ID', 'atividade', 'rendaf', 'sexo', 'cor', 'escola', 'idade'))

freq <- base_junta %>% 
  pivot_longer(-c(sexo:escola), names_to="atividade", values_to="freq_consumo")



########## Gráficos


##### Gráfico de pontos mudando de acordo com o nível de interesse médio
# Renda
intfreq %>%
  filter (rendaf != "não sabe", rendaf != "recusa") %>% 
  group_by (atividade, rendaf) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 

  ggplot() + geom_point(aes(y=atividade, x=rendaf, col=rendaf, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por cada grupo de renda)", x="Faixa de Renda Familiar", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte", legend="Faixa de Renda Familiar") + theme_light ()

# Escolaridade
intfreq %>%
  group_by (atividade, escola) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  
  ggplot() + geom_point(aes(y=atividade, x=escola, col=escola, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por nível de escolaridade)", x="Nível de Escolaridade", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte") + theme_light ()

# Cor
intfreq %>%
  filter (cor != "não sabe", cor != "7", cor != "nenhuma", cor != "outras", cor != "mestiço") %>% 
  group_by (atividade, cor) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  
  ggplot() + geom_point(aes(y=atividade, x=cor, col=cor, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por Autodeclaração de Cor)", x="Cor", y="Atividade Cultural", show.legend = FALSE, fill = "Cor", caption="Fonte: JLeiva Cultura & Esporte") + theme_light ()


##### Piramide de genero - interesses
intfreq %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(x = atividade, fill = sexo, y = ifelse(test = sexo == "masculino", yes = -mediaint, no = mediaint))) + 
  geom_bar(stat = "identity") + scale_y_continuous(labels = abs, limits = max(int$nivel_interesse) * c(-1,1)) + 
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


#### FREQUÊNCIA DE CONSUMO
### % de pessoas que consumiram há menos de 1 ano

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "1 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "2 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "3 - Médio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "4 - Superior Completo", "NULL")))), 
         frequencia = ifelse(frequencia == "último mês" | frequencia == "último ano", "Último Ano", frequencia)) %>%
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'Último Ano',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  ggplot() +
    geom_col(aes(x = fr, y = atividade, fill = educ)) +
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
         cor != 'outra') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
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
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que consumiram no último ano",
       subtitle = "Recorte por Classe Econômica", 
       fill = "Classe Econômica") + 
  theme_classic()



### EXCLUSÃO SOCIAL -  % de pessoas que nunca foram

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "1 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "2 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "3 - Médio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "4 - Superior Completo", "NULL"))))) %>% 
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = educ)) +
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
         cor != 'outra') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
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
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = sexo)) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Sexo", 
       fill = "Sexo") + 
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
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Classe Econômica", 
       fill = "Classe Econômica") + 
  theme_classic()




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
    
  
  
### Mudanças de preferências (% de quem tem alto interesse) 
# Recorte por idade 
  intfreq %>% 
    mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", 
                                    "Alto Interesse", "Médio/Baixo interesse")) %>% 
    group_by(atividade, nivel_interesse, idade) %>% 
    summarise(fa = n()) %>%
    filter(nivel_interesse == 'Alto Interesse') %>% 
    group_by(atividade) %>% 
    mutate(total = sum(fa)) %>%
    mutate(fr = fa/total * 100) %>%
    ggplot() +
    geom_point(aes(x = idade, y = atividade, color = idade, size = fr), show.legend = FALSE) +
    labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
         title = "% de entrevistados com Alto Interesse",
         subtitle = "Alto Interesse = 8,9,10 | Recorte por Faixas de Idade", 
         fill = "Idade") + 
    theme_light()
  
# Recorte por Classe economica 
  intfreq %>%
    mutate(faixasrenda = ifelse(rendaf=="até R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                         ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                         ifelse(rendaf=="R$9.371 a R$18.740", "B",
                         ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "Não sabe")))),
           nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", 
                                    "Alto Interesse", "Médio/Baixo interesse")) %>% 
    group_by(atividade, nivel_interesse, faixasrenda) %>% 
    summarise(fa = n()) %>%
    filter(nivel_interesse == 'Alto Interesse') %>% 
    group_by(atividade) %>% 
    mutate(total = sum(fa)) %>%
    mutate(fr = fa/total * 100) %>%
    ggplot() +
    geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
    labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
         title = "% de entrevistados com Alto Interesse",
         subtitle = "Alto Interesse = 8,9,10 | Recorte por Classe Econômica", 
         fill = "Classe Econômica") + 
    theme_classic()
  





