setwd("~/Insper Data/Micro - PP")

# Pacotes obrigat�rios para come�ar

library(ggplot2)
library(tidyverse)
library(dplyr)


#### BASES 
# spculturalgeral - base grande e original, sem mudan�as
# spcultural - base filtrada com caracter�sticas de recorte + freq + interesse
# interesses - s� as caracteristicas socio + interesse
# frequencia - s� as caracteristicas socio + frequencia
# intfreq - interesse e frequencia juntos e ajeitadas pelo pivot longer


df <- readxl::read_xlsx('spcultural2 - com analise desc.xlsx', sheet = 1)


# Ajeitando os dados 
df <- df %>% 
  mutate(religiao = ifelse(religiao == "1", "Evang�lica",
                    ifelse(religiao == "2", "Protestante",
                    ifelse(religiao == "3", "Ev. Pentecostal",
                    ifelse(religiao == "4", "Ev. N�o Pentecostal",
                    ifelse(religiao == "5", "Outras Evang�licas",
                    ifelse(religiao == "6", "Umbanda / Candombl�",
                    ifelse(religiao == "7", "Esp�rita",
                    ifelse(religiao == "8", "Cat�lica",
                    ifelse(religiao == "9", "Judaica",
                    ifelse(religiao == "10", "Outra",
                    ifelse(religiao == "11", "Agn�stico",
                    ifelse(religiao == "12", "Ateu", religiao))))))))))))) %>% 
  view()


base_junta <- df %>% 
  mutate(ID = row_number()) %>% 
  select(ID, sexo, idade, cor, rendaf, escola, religiao, i_biblio:f_games) 

 
  pivot_longer(c(i_biblio:f_games),  names_to="atividade", values_to="nivel_interesse")


int1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, religiao, i_biblio:i_games) %>% 
  pivot_longer(-c(ID:religiao), names_to="atividade", values_to="nivel_interesse") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


freq1 <- base_junta %>%
  select(ID, sexo, idade, cor, rendaf, escola, religiao, f_biblio:f_games) %>% 
  pivot_longer(-c(ID:religiao), names_to="atividade", values_to="frequencia") %>% 
  separate(atividade, into = c('i','atividade'), sep = '_') %>% 
  select(-i)


intfreq <- int1 %>% 
  left_join(freq1, by = c('ID', 'atividade', 'rendaf', 'sexo', 'cor', 'escola', 'idade', 'religiao'))



########## GR�FICOS ######################################

# Nuvem de palavras - atividades mais frequentadas - geral
library(wordcloud2)

(words <- intfreq %>% 
    mutate(frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia),
           atividade = ifelse(atividade == "biblio", "bibliotecas",
                       ifelse(atividade == "livros", "livros",
                       ifelse(atividade == "carnaval", "blocos de carnaval",
                       ifelse(atividade == "musicais", "saraus", 
                       ifelse(atividade == "circo", "circos",
                       ifelse(atividade == "concertos", "concertos de m�sica",
                       ifelse(atividade == "museu", "museus e exposi��es",
                       ifelse(atividade == "danca", "apresenta��es de dan�a",
                       ifelse(atividade == "games", "jogos virtuais",
                       ifelse(atividade == "festas", "festas populares",
                       ifelse(atividade == "shows", "shows", 
                       ifelse(atividade == "teatro", "teatro ou musicais", atividade))))))))))))) %>%
    filter(frequencia == "�ltimo Ano") %>% 
    group_by(atividade) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq)))

wordcloud2(words, minRotation = 0, maxRotation = 0, fontFamily = "Segoe UI",
           backgroundColor = "white", color = "random-dark")


##### Piramide de genero - interesses
intfreq %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(x = atividade, fill = sexo, y = ifelse(test = sexo == "masculino", yes = -mediaint, no = mediaint))) + 
  geom_bar(stat = "identity") + scale_y_continuous(labels = abs, limits = max(int$nivel_interesse) * c(-1,1)) + 
  coord_flip() + labs(title="Interesse pelas Atividades Culturais", subtitle = "De cada grupo de g�nero", fill = "Sexo", y="M�dio do N�vel de Interesse", x = "Atividade Cultural") +
  theme_light()

##### Lollipop genero - interesses
intfreq %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(mediaint, atividade, color = sexo, label = mediaint)) + 
  geom_segment(aes(x = 3, y = atividade, xend = 9, yend = atividade), color = "grey87") +
  geom_point(aes(color=sexo, size=mediaint)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "M�dia do N�vel de Interesse", y = "Atividade Cultural",
       title = "Interesse pelas Atividades Culturais", col = "Sexo") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.minor.y = element_line(colour = "grey87"),
        panel.grid.major = element_line(colour = "grey87"),
        plot.caption=element_text(size = 8))


################# FREQU�NCIA DE CONSUMO - sem pondera��o

### % de pessoas que consumiram h� menos de 1 ano

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))), 
         frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>%
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == '�ltimo Ano',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
    geom_col(aes(x = fr, y = atividade, fill = educ)) +
    geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
    labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
         title = "% de entrevistados que consumiram no �ltimo ano",
         subtitle = "Recorte por Faixas de Escolaridade", 
         fill = "N�vel educacional") + 
    theme_classic()

# Recorte por Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras', "outra", cor), 
         frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>% 
  group_by(atividade, frequencia, cor) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == '�ltimo Ano',
         cor != 'outra', cor != 'mesti�o') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>%
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que consumiram no �ltimo ano",
       subtitle = "Recorte por Autodeclara��o de Cor", 
       fill = "Cor") + 
  theme_classic()

# Recorte por classe economica
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe")))),
         frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>% 
  group_by(atividade, frequencia, faixasrenda) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == '�ltimo Ano') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que consumiram no �ltimo ano",
       subtitle = "Recorte por Classe Econ�mica", 
       fill = "Classe Econ�mica") + 
  theme_classic()


### EXCLUS�O SOCIAL -  % de pessoas que nunca foram

# Recorte por Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
  
  group_by(educ, atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi',
         educ != "NULL") %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = educ)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Faixas de Escolaridade", 
       fill = "N�vel educacional") + 
  theme_classic()

# Recorte por Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras', 
                       "outra", cor)) %>% 
  group_by(atividade, frequencia, cor) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi',
         cor != 'outra', cor != 'mesti�o') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>%
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = cor)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Autodeclara��o de Cor", 
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
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = sexo)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Sexo", 
       fill = "Sexo") + 
  theme_classic()

#Recorte por Religi�o
intfreq %>% 
  group_by(atividade, frequencia, religiao) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = religiao)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Religi�o", 
       fill = "Religi�o") + 
  theme_classic()

#Recorte por Renda
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                ifelse(rendaf=="R$9.371 a R$18.740", "B",
                ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe"))))) %>% 
  group_by(atividade, frequencia, faixasrenda) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == 'nunca foi') %>% 
  group_by(atividade) %>% 
  mutate(total = sum(fa)) %>%
  mutate(fr = fa/total * 100) %>% 
  mutate(fr=round(fr, digits=1)) %>% #definindo s� 1 casa decimal pro fr
  mutate(pos = -(cumsum(fr) - 0.5*fr)+100) %>% # posi��o do texto no gr�fico
  ggplot() +
  geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
  geom_text(aes(y = atividade, x = pos, label = fr), size=3) +
  labs(x = "Porcentagem (%) de Entrevistados", y = "Atividade Cultural",
       title = "% de entrevistados que nunca consumiram",
       subtitle = "Recorte por Classe Econ�mica", 
       fill = "Classe Econ�mica") + 
  theme_classic()



#__________________________________________________________________________________________
###### RELA��O ALTO INTERESSE X FREQU�NCIA - Geral (sem recortes)

  # Criando tabela frequ�ncia relativa do consumo no �ltimo ano de cada atividade
  tabelafreq <- intfreq %>% 
  mutate(frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>%
  group_by(atividade, frequencia) %>% 
  summarise(fa = n()) %>%
  filter(frequencia == '�ltimo Ano') %>% 
  mutate(frf = fa/3004 * 100) %>% 
  view()
  
  # Criando tabela frequ�ncia relativa do alto interesse por cada atividades 
  tabelaint <- intfreq %>% 
  mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", "Alto Interesse", "M�dio/Baixo interesse")) %>%
  group_by(atividade, nivel_interesse) %>% 
  summarise(fai = n()) %>% 
  filter(nivel_interesse == 'Alto Interesse') %>% 
  mutate(fri = fai/3004 * 100) %>% 
  view()
  
  # Juntando as duas tabelas e criando a Rela��o Frequ�ncia X Interesse
  
  tabelaintfreq <- tabelaint %>% 
    left_join(tabelafreq, by = c('atividade')) 
  
  tabelaintfreq["FrequenciaxInteresse"] <-((tabelaintfreq$frf/tabelaintfreq$fri) * 100) 

  write.csv(tabelaintfreq, file = "intXfreq geral.csv") # exportando para csv
  
  
# _______________________________________________________________________________________
#### Gr�ficos de Barra 
## frequ�ncia de consumo PONDERADA pelo total de cada faixa de recorte
    
#### Primeiramente: Criando bases para todos os recortes: 
  freqeduc <- intfreq %>% 
    mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                  ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                  ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
                  ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
    group_by(educ, atividade) %>% 
    summarise(fae = n()) %>% 
    filter(educ != "NULL") %>%
    group_by(educ) %>% 
    view()
  
  freqclasse <- intfreq %>% 
    mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                         ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                         ifelse(rendaf=="R$9.371 a R$18.740", "B",
                         ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe"))))) %>% 
    group_by(faixasrenda, atividade) %>% 
    summarise(fac = n()) %>% 
    filter(faixasrenda != "N�o sabe") %>%
    group_by(faixasrenda) %>% 
    view()          
           
  freqcor <- intfreq %>% 
    mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mesti�o', "outra",
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
   
##### C�digos dos gr�ficos:
## Primeiro c�digo: "nunca foi" / Segundo c�digo: "Foi h� menos de 1 ano"
  
  # Recortes por Escolaridade
    intfreq %>% 
      mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                    ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                    ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
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
           fill = "N�vel educacional") + theme_classic() 
    
    intfreq %>% 
      mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                    ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                    ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
                    ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))),
             frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>% 
      left_join(freqeduc, by = c("atividade", "educ")) %>% 
      group_by(atividade, educ, frequencia, fae) %>% 
      summarise(fa = n()) %>%
      filter(educ != "NULL", frequencia == "�ltimo Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fae *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = ifelse(fr > 5, -(cumsum(fr) - 0.5*fr) + sum(fr), -(cumsum(fr) - 0.5*fr) + sum(fr) + 7)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = educ)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no �ltimo ano",
           subtitle = "Em cada Faixa de Escolaridade", 
           fill = "N�vel educacional") + theme_classic()   
    
  # Recortes por renda
    intfreq %>% 
      mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                           ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                           ifelse(rendaf=="R$9.371 a R$18.740", "B",
                           ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe"))))) %>% 
      left_join(freqclasse, by = c("atividade", "faixasrenda")) %>% 
      group_by(atividade, faixasrenda, frequencia, fac) %>% 
      summarise(fa = n()) %>%
      filter(faixasrenda != "N�o sabe", frequencia == "nunca foi") %>%
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
      mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                           ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                           ifelse(rendaf=="R$9.371 a R$18.740", "B",
                           ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe")))),
             frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>% 
      left_join(freqclasse, by = c("atividade", "faixasrenda")) %>% 
      group_by(atividade, faixasrenda, frequencia, fac) %>% 
      summarise(fa = n()) %>%
      filter(faixasrenda != "N�o sabe", frequencia == "�ltimo Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fac *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = faixasrenda)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no �ltimo ano",
           subtitle = "Em cada Classe Social", 
           fill = "Classe Social") + theme_classic()
    
  # Recortes por cor
    intfreq %>% 
      mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mesti�o', "outra",
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
           fill = "Autodeclara��o de Cor") + theme_classic()
    
    intfreq %>% 
      mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mesti�o', "outra",
                   ifelse(cor == 'parda' | cor == 'preta', "negra", cor)),
             frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>%
      left_join(freqcor, by = c("atividade", "cor")) %>% 
      group_by(atividade, cor, frequencia, facor) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "�ltimo Ano", cor != "outra",) %>%
      group_by(atividade) %>% 
      mutate(fr = fa/facor *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = cor)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no �ltimo ano",
           subtitle = "Em cada grupo de cor", 
           fill = "Autodeclara��o de Cor") + theme_classic()
    
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
           subtitle = "Em cada grupo de g�nero", 
           fill = "Sexo") + theme_classic()
    
    intfreq %>% 
      mutate (frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>%
      left_join(freqsexo, by = c("atividade", "sexo")) %>% 
      group_by(atividade, sexo, frequencia, fasex) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "�ltimo Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/fasex *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = sexo)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size=4) +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no �ltimo ano",
           subtitle = "Em cada grupo de g�nero", 
           fill = "Sexo") + theme_classic()

# Recortes por religi�o
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
           fill = "Religi�o") + theme_classic()
    
    intfreq %>%
      mutate(frequencia = ifelse(frequencia == "�ltimo m�s" | frequencia == "�ltimo ano", "�ltimo Ano", frequencia)) %>% 
      left_join(freqreligiao, by = c("atividade", "religiao")) %>% 
      group_by(atividade, religiao, frequencia, farelig) %>% 
      summarise(fa = n()) %>%
      filter(frequencia == "�ltimo Ano") %>%
      group_by(atividade) %>% 
      mutate(fr = fa/farelig *100) %>% 
      mutate(fr=round(fr, digits=1)) %>% 
      mutate(pos = -(cumsum(fr) - 0.5*fr) + sum(fr)) %>% 
      ggplot() + geom_col(aes(x = fr, y = atividade, fill = religiao)) +
      geom_text(aes(y = atividade, x = pos, label = fr), size = 4) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Contagem (%)", y = "Atividade Cultural",
           title = "Porcentagem (%) de entrevistados que consumiram as Atividades no �ltimo ano",
           subtitle = "Em cada grupo Religioso", 
           fill = "Religi�o") + theme_classic()
    
    
#________________________________________________________________________________________
######## % de quem tem ALTO interesse
### Para observar as mudan�as de prefer�ncias conforme mudam as faixas de recorte
## GR�FICO DE PONTOS (ponderados)
    
    intfreq %>% 
      mutate(nivel_interesse = ifelse(nivel_interesse == "8" | nivel_interesse == "9" | nivel_interesse == "10", 
                                      "Alto Interesse", "M�dio/Baixo interesse")) %>%
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
##### ACESSO � INTERNET  
## Gr�fico de Barras 

# Escolaridade
    
df %>% 
mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
              ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
              ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
              ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL")))),
      p34 = ifelse(p34 == "1", "1 - Todos os Dias, e est� sempre conectado",
            ifelse(p34 == "2", "2 - Todos os Dias, mas nem sempre est� conectado",
            ifelse(p34 == "3", "3 - Acessa, mas n�o sempre",
            ifelse(p34 == "4", "4 - Acessa com pouca frequ�ncia",
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
    title = "Acesso � Internet", subtitle = "Recorte por N�vel de Escolaridade", 
    fill = "Frequ�ncia de acesso:") + theme_light()


# Renda 

df %>% 
  mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe")))),
         p34 = ifelse(p34 == "1", "1 - Todos os Dias, e est� sempre conectado",
               ifelse(p34 == "2", "2 - Todos os Dias, mas nem sempre est� conectado",
               ifelse(p34 == "3", "3 - Acessa, mas n�o sempre",
               ifelse(p34 == "4", "4 - Acessa com pouca frequ�ncia",
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
       title= "Acesso � internet",
       subtitle = "Recorte por Renda",
       fill="Frequ�ncia de acesso")+
  theme_light()

# _______________________________________________________________________________________
# BOXPLOTS
# interesses pelas atividades

# Sexo
ggplot(intfreq, aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=sexo)) + 
  scale_y_continuous(limits=c(0,10)) + 
  labs(title="Distribui��o do Interesse pelas Atividade Cultural", subtitle="Recorte por sexo", x="Atividade Cultural", y="N�vel de Interesse") + 
  theme_classic() 

# Classes
intfreq %>% 
  mutate(faixasrenda = ifelse(rendaf=="at� R$1.874"|rendaf=="R$1.875 a R$2.811", "D-E",
                       ifelse(rendaf=="R$2.812 a R$4.685"|rendaf=="R$4.686 a R$9.370", "C",
                       ifelse(rendaf=="R$9.371 a R$18.740", "B",
                       ifelse(rendaf=="R$18.741 a R$46.850"|rendaf=="R$46.851 ou mais", "A", "N�o sabe"))))) %>% 
  filter(faixasrenda != "N�o sabe") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=faixasrenda)) + 
  scale_y_continuous(limits=c(0,10)) + 
  scale_fill_brewer(palette = "Set1") +
  labs(title="Distribui��o do Interesse pelas Atividade Cultural", subtitle="Recorte por Classe Social", x="Atividade Cultural", y="N�vel de Interesse") + 
  theme_classic() 

# Cor
intfreq %>% 
  mutate(cor = ifelse(cor == '7'| cor == 'n�o sabe' | cor == 'nenhuma' | cor == 'outras' | cor =='mesti�o', "outra",
               ifelse(cor == 'parda' | cor == 'preta', "negra", cor))) %>% 
  filter(cor != "outra") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=cor)) + 
  scale_y_continuous(limits=c(0,10)) + 
  scale_fill_brewer(palette = "Spectral") +
  labs(title="Distribui��o do Interesse pelas Atividade Cultural", subtitle="Recorte por Ra�a", x="Atividade Cultural", y="N�vel de Interesse") + 
  theme_classic() 

# Escolaridade
intfreq %>% 
  mutate(educ = ifelse(escola=="fundamental 1 inc"|escola=="fundamental 2 inc", "4 - Fundamental Incompleto",
                ifelse(escola=="fundamental 2 comp"|escola=="ensino medio inc", "3 - Fundamental Completo",
                ifelse(escola=="ensino medio comp"|escola=="superior inc", "2 - M�dio Completo",
                ifelse(escola=="superior comp"|escola=="pos grad", "1 - Superior Completo", "NULL"))))) %>% 
  filter(educ != "NULL") %>% 
  ggplot(aes(atividade, nivel_interesse)) + 
  geom_boxplot(aes(fill=educ)) + 
  scale_y_continuous(limits=c(0,10)) + 
  labs(title="Distribui��o do Interesse pelas Atividade Cultural", subtitle="Recorte por Escolaridade", x="Atividade Cultural", y="N�vel de Interesse") + 
  theme_classic() 


# _______________________________________________________________________________________


