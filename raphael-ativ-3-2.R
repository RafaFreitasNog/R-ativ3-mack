---
  title: "Atividade_Continuada_3_2"
output: html_document
date: "2022-11-06"
---
  

knitr::opts_chunk$set(echo = TRUE)


### Bibliotecas a serem carregadas

install.packages(c("arules", "arulesViz", "RColorBrewer"))

library(arules)
library(arulesViz)
library(RColorBrewer)


### Criando base de dados para utilizar no metódo a priori

transacoes <- list(
  c('beer', 'wine', 'cheese', NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL),
  c('eggs', 'flower', 'butter', 'cheese', NULL),
  c('eggs', 'flower', 'butter', 'beer', 'potato chips'),
  c('wine', 'cheese', NULL, NULL, NULL),
  c('potato chips', NULL, NULL, NULL, NULL),
  c('eggs', 'flower', 'butter', 'wine', 'cheese'),
  c('eggs', 'flower', 'butter', 'beer', 'potato chips'),
  c('wine', 'beer', NULL, NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL),
  c('butter', 'eggs', NULL, NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL),
  c('flower', 'eggs', NULL, NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL),
  c('eggs', 'flower', 'butter', 'wine', 'cheese'),
  c('beer', 'wine', 'potato chips', 'cheese', NULL),
  c('wine', 'cheese', NULL, NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL),
  c('wine', 'cheese', NULL, NULL, NULL),
  c('beer', 'potato chips', NULL, NULL, NULL)
)

df <- as.data.frame(do.call(rbind, transacoes))


### Criando a base para o metódo apriori e suas variações de regras

rules <- apriori(transacoes,parameter=list(support=0.001,confidence=0.6,target="rules"))
rules2 <- apriori(transacoes,parameter=list(support=0.021,confidence=0.4,target="rules"))
rules3 <- apriori(transacoes,parameter=list(support=0.100,confidence=0.2,target="rules"))

inspect(rules[1:10])
inspect(rules2[1:10])
inspect(rules3[1:10])

plot(rules)
plot(rules2)
plot(rules3)

### Metódo apriori com as primeiras regras

highLiftRules<-head(sort(rules,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))

### Metódo apriori com as outras variações

highLiftRules<-head(sort(rules2,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))


highLiftRules<-head(sort(rules3,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))