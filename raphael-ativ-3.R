---
  title: "Atvidade3"
output: html_document
date: "2022-11-06"
---
  
  
library(tidyr)
library(imputeTS)
library(httr)
library(jsonlite)
library(tidyverse)  # manipulacao de dados
library(cluster)    # Algoritmos de clusterizacao
library(factoextra) # Algoritmos de clusterizacao e gráficos
library(gridExtra)  # Mostrar vários gráficos
library(ggplot2)    # Biblioteca de gráficos
knitr::opts_chunk$set(echo = TRUE)


### Realizando a preparação da base que será utilizada, neste caso, todos os valores de HP dos 151 pokémons da primeira geração, lembrando que não haverá nenhum valor faltando na base, por isso tratamentos como na.omit() não serão necessários
pokemon_hp <- list()


## Gerando um dataset via api do status hp
## dos pokemons da primeira geração
for (i in 1:151) {
  my_url <- paste0("https://pokeapi.co/api/v2/pokemon/", i)
  teste <- httr::GET(my_url)
  teste_char <- base::rawToChar(teste$content)
  teste_JSON <- jsonlite::fromJSON(teste_char, flatten = TRUE)
  pokemon_hp <- append(pokemon_hp,teste_JSON$stats$base_stat[1])
}

pokemon_hp <- unlist(pokemon_hp)


### Realizando a normalização da base e calculando distância euclidiana
### Como podemos ver no gráfico gerado, o resultado não fica muito claro, já que os dados que estamos utilizando são apenas números sem classificação, diferente do modelo de crimes utilizado na aula, de qualquer forma vamos em frente

pokemon_hp_norm <- scale(pokemon_hp)
dist_eucli <- get_dist(pokemon_hp_norm)

fviz_dist(dist_eucli, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


### Como muito valor não foi gerado anteriormente, vamos partir para criação de clusters kmeans

k2 <- kmeans(pokemon_hp_norm, centers = 2, nstart = 25)
str(k2)
k2


## Agora que já fizemos nosso cluster inicial, vamos inserir mais um para podermos ter um maior número de hipóteses

k3 <- kmeans(pokemon_hp_norm, centers = 3, nstart = 25)


###Gerando um gráfico para cada cluster definido, nesse caso ocorrerá um erro, já que nossa base de dados possui apenas uma única coluna, então vamos adicionar mais uma a ela com os attacks de cada pokémon

p1 <- fviz_cluster(k2, geom = "point", data = pokemon_hp_norm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = pokemon_hp_norm) + ggtitle("k = 3")
grid.arrange(p1, p2, nrow = 2)


pokemon_attack <- list()


## Gerando um dataset via api do status hp
## dos pokemons da primeira geração
for (i in 1:151) {
  my_url <- paste0("https://pokeapi.co/api/v2/pokemon/", i)
  teste <- httr::GET(my_url)
  teste_char <- base::rawToChar(teste$content)
  teste_JSON <- jsonlite::fromJSON(teste_char, flatten = TRUE)
  pokemon_attack <- append(pokemon_attack,teste_JSON$stats$base_stat[2])
}

pokemon_attack <- unlist(pokemon_attack)

pokemon_attack_norm <- scale(pokemon_attack)
colnames(pokemon_hp_norm)[1] = "HP"
colnames(pokemon_attack_norm)[1] = "ATTACK"
pokemon_stats_norm <- data.frame(pokemon_hp_norm, pokemon_attack_norm)


### Agora sim vamos tentar de novo

k2 <- kmeans(pokemon_stats_norm, centers = 2, nstart = 25)
k3 <- kmeans(pokemon_stats_norm, centers = 3, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = pokemon_stats_norm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = pokemon_stats_norm) + ggtitle("k = 3")
grid.arrange(p1, p2, nrow = 2)

### Como podemos ver acima o gráfico com duas colunas se tornou bem mais satisfatório e visível, nos dando noção que existem pokémon com muito hp mais quase nenhum attack
### Para finalizar vamos observar o gap estatístico obtido para ter certeza do número otimizado de clusters, neste caso por sore, acabamos escolhendo justamente 2 clusters, o número ótimo

gap_stat <- clusGap(pokemon_stats_norm,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

fviz_gap_stat(gap_stat)