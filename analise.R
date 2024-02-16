library(tidyverse)
library(rvest)
library(data.table)

populacao.mundial.web <- read_html("https://en.wikipedia.org/wiki/World_population")
lpop <- populacao.mundial.web %>% rvest::html_nodes("table") %>% html_table()
pop.mundial <- lpop[[5]]
pop.mundial <- pop.mundial[-1,]
#Criando função auxiliar para limpar os nomes das variáveis, 
#tirando os espacos e substituindo por "_".

#Gabarito Aula 3
limpeza.nomes <- function(nomes)
{
  nomes.limpos <- gsub("__", "_", gsub("\\s","_",gsub("/", "", nomes)))
  return(nomes.limpos)
}


#Implementacao da Aula 4 - Substituindo o gsub pela funcao do stringr
limpeza.nomes <- function(nomes)
{
  nomes.limpos <- stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(nomes, "/", ""), "\\s", "_"), "__", "_")
  return(nomes.limpos)  
}

names(pop.mundial) <- limpeza.nomes(names(pop.mundial))

#Implementação utilizando pipe

limpeza.nomes.pipe <- function(nomes)
{
  nomes.limpos <- nomes %>% stringr::str_replace_all("/", "") %>% stringr::str_replace_all("\\s", "_") %>% stringr::str_replace_all("__", "_")
  return(nomes.limpos)  
}

names(pop.mundial) <- limpeza.nomes.pipe(names(pop.mundial))
names(pop.mundial)[3] <- "Percentage_of_the_world"

#Selecionando as colunas úteis para analise

#pop.mundial <- pop.mundial %>% dplyr::select(Rank:Date)
#pop.mundial <- pop.mundial %>% dplyr::select(!c("Source_(official_or_from_the_United_Nations)")

#Limpando a variável Population para se tornar uma variável numérica
pop.mundial <- pop.mundial %>% dplyr::mutate(Population = as.numeric(stringr::str_replace_all(Population, "\\,", "")))

#Limpando a variável Percentage_of_the_world and converting to proportions in classe numeric
pop.mundial <- pop.mundial %>% dplyr::mutate(Percentage_of_the_world = as.numeric(stringr::str_replace_all(Percentage_of_the_world, "%", ""))/100)

#Limpando a variável Date e convertendo de character para classe Date 

pop.mundial <- pop.mundial %>% dplyr::mutate(Date = as.IDate(gsub(" ", "-", Date), format = "%d-%b-%Y"))

#Vamos criar uma variável continente ?

#Importando uma base de dados que tenha a informacao de paises por continente
continentes <- readr::read_csv("dados_auxiliares/continents-according-to-our-world-in-data.csv")

#Vinculando esta base à nossa base limpa com os dados de populacao por pais no top-10

pop.mundial <- pop.mundial %>% dplyr::left_join(continentes, by = c("Country_Dependency" = "Entity"))

#Vamos agrupar os dados de populacao por continente e criar uma nova variavel ?

pop.mundial <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::mutate(Population_Continent = sum(Population))
pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::summarise(Population_Continent = sum(Population))