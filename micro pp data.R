
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

# Ajeitando os dados e juntando as bases de interesse e de frequencia de consumo
int <- interesses %>% 
  pivot_longer(-c(sexo:escola), names_to="atividade", values_to="nivel_interesse")

freq <- frequencia %>% 
  pivot_longer(-c(sexo:escola), names_to="atividade", values_to="freq_consumo")

intfreq <- left_join(int, freq, by=c("atividade", "sexo", "idade", "escola", "cor", "rendaf"))

########## Gráficos

# Dispersão - renda x nivel de interesse pelas atividades
int %>% 
  filter(nivel_interesse>7, rendaf) %>% 
  ggplot(aes(x = nivel_interesse,
             y = rendaf)) +
  geom_point(aes(color = atividade)) +
  facet_grid(~ atividade)

##### aquele dos pontos mudando de acordo com o interesse médio
# Renda
int %>%
  filter (rendaf != "não sabe", rendaf != "recusa") %>% 
  group_by (atividade, rendaf) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 

  ggplot() + geom_point(aes(y=atividade, x=rendaf, col=rendaf, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por cada grupo de renda)", x="Faixa de Renda Familiar", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte", legend="Faixa de Renda Familiar") + theme_light ()

# Escolaridade
int %>%
  group_by (atividade, escola) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  
  ggplot() + geom_point(aes(y=atividade, x=escola, col=escola, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por nível de escolaridade)", x="Nível de Escolaridade", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte") + theme_light ()

# Gênero
int %>%
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  
  ggplot() + geom_point(aes(y=atividade, x=sexo, col=sexo, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por Gênero)", x="Gênero", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte") + theme_light ()

# Cor
int %>%
  filter (cor != "não sabe", cor != "7", cor != "nenhuma", cor != "outras", cor != "mestiço") %>% 
  group_by (atividade, cor) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  
  ggplot() + geom_point(aes(y=atividade, x=cor, col=cor, size=mediaint)) + 
  labs (title="Interesse pelas Atividades Culturais", subtitle="(Por Autodeclaração de Cor)", x="Cor", y="Atividade Cultural", caption="Fonte: JLeiva Cultura & Esporte") + theme_light ()


##### Piramide de genero - interesses
int %>% 
  group_by (atividade, sexo) %>%
  summarise(mediaint = mean(nivel_interesse)) %>% 
  ggplot(aes(x = atividade, fill = sexo, y = ifelse(test = sexo == "masculino", yes = -mediaint, no = mediaint))) + 
  geom_bar(stat = "identity") + scale_y_continuous(labels = abs, limits = max(int$nivel_interesse) * c(-1,1)) + 
  coord_flip() + labs(title="Interesse por Atividades Culturais", x="Nível de Interesse", y = "Atividade Cultural") +
  theme_classic()

#ta muito lindo
#oi
#lalalalalala
