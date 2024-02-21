Analise Populacional
================
2024-02-19

# Importando bibliotecas

Importe a biblioteca rvest para acessar dados web do wikipedia e
processar esses dados.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(knitr)
```

# Importando os dados a partir do site da wikipedia

# Criando função auxiliar para limpar os nomes das variáveis,

- tirando os espacos e;
- Substituindo por “\_“.

# Selecionando as colunas úteis para analise e limpei as variáveis

- Limpando a variável Population para se tornar uma variável numérica
- Limpando a variável Percentage_of_the_world and convertendo para
  proporções na classe numeric
- Limpando a variável Date e convertendo de character para classe Date

# Vamos criar uma variável continente ?

- Importando uma base de dados que tenha a informacao de paises por
  continente
- Vinculando esta base à nossa base limpa com os dados de populacao por
  pais no top-10

# Vamos agrupar os dados de populacao por continente e criar uma nova variavel ?

``` r
pop.mundial <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::mutate(Population_Continent = sum(Population))
pop.mundial.agg <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::summarise(Population_Continent = sum(Population))
kable(pop.mundial.agg)
```

| Continent     | Population_Continent |
|:--------------|---------------------:|
| Africa        |            216746934 |
| Asia          |           3514781044 |
| Europe        |            147190000 |
| North America |            464377230 |
| South America |            217307635 |
