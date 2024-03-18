Análise descritiva de uma base de dados
================
Otto Tavares
15 March, 2024

## Introdução

Na Aula 8, temos o objetivo de abrir uma base de dados e dar os
primeiros passos em análise estatística dessa base.

Como sempre, o primeiro passo é importar as bibliotecas que serão
utilizadas para análise, como tydiverse, summarytools e dlookr.

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
library(dlookr)
```

    ## Registered S3 methods overwritten by 'dlookr':
    ##   method          from  
    ##   plot.transform  scales
    ##   print.transform scales
    ## 
    ## Attaching package: 'dlookr'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     transform

``` r
library(summarytools)
```

    ## 
    ## Attaching package: 'summarytools'
    ## 
    ## The following object is masked from 'package:tibble':
    ## 
    ##     view

``` r
library(readxl)
library(knitr)
```

A base trabalhada nesta aula, será a base de dados hipotética
disponbilizada no livro texto dos autores Bussab e Moretim. Vamos
importá-la e imprimir as primeiras observações para conhecimento das
variáveis.

``` r
kable(head(salarios))
```

|   n | estado_civil | Grau_de_instrucao  | n_filhos | salario | idade_anos | idade_meses | regiao   |
|----:|:-------------|:-------------------|---------:|--------:|-----------:|------------:|:---------|
|   1 | solteiro     | ensino fundamental |       NA |    4.00 |         26 |           3 | interior |
|   2 | casado       | ensino fundamental |        1 |    4.56 |         32 |          10 | capital  |
|   3 | casado       | ensino fundamental |        2 |    5.25 |         36 |           5 | capital  |
|   4 | solteiro     | ensino médio       |       NA |    5.73 |         20 |          10 | outra    |
|   5 | solteiro     | ensino fundamental |       NA |    6.26 |         40 |           7 | outra    |
|   6 | casado       | ensino fundamental |        0 |    6.66 |         28 |           0 | interior |

\###Identificando os tipos de cada variável na base

Para identificar os tipos de cada variável na base, vamos utilizar a
função diagnose do pacote dlookr e reportar o tipo de cada um para
melhor trabalharmos os dados.

``` r
salarios %>% dlookr::diagnose()
```

    ## # A tibble: 8 × 6
    ##   variables         types missing_count missing_percent unique_count unique_rate
    ##   <chr>             <chr>         <int>           <dbl>        <int>       <dbl>
    ## 1 n                 nume…             0             0             36      1     
    ## 2 estado_civil      char…             0             0              2      0.0556
    ## 3 Grau_de_instrucao char…             0             0              3      0.0833
    ## 4 n_filhos          nume…            16            44.4            6      0.167 
    ## 5 salario           nume…             0             0             36      1     
    ## 6 idade_anos        nume…             0             0             24      0.667 
    ## 7 idade_meses       nume…             0             0             12      0.333 
    ## 8 regiao            char…             0             0              3      0.0833

É fácil ver que na base há três variáveis qualitativas, sendo as
variáveis Estado Civil e região nominais, enquanto a variável Grau de
Instrução é ordinal.

Sobre as variáveis quantitativas, temos número de filhos e idade com
variáveis discretas, equanto a variável salário é contínua.

\##Análise de frequências de variáveis qualitativas

A variável região é uma das variáveis qualitativas nominais da base,
sendo uma variável interessante para extraírmos as frequências. Para
esse caso, vamos utilizar a função freq() do pacote summarytools

``` r
salarios %>% dplyr::select(regiao) %>% summarytools::freq()
```

    ## Frequencies  
    ## salarios$regiao  
    ## Type: Character  
    ## 
    ##                  Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## -------------- ------ --------- -------------- --------- --------------
    ##        capital     11     30.56          30.56     30.56          30.56
    ##       interior     12     33.33          63.89     33.33          63.89
    ##          outra     13     36.11         100.00     36.11         100.00
    ##           <NA>      0                               0.00         100.00
    ##          Total     36    100.00         100.00    100.00         100.00

Nas colunas Freq, temos a frequência absoluta, mostrando um grau de
bastante homogeneidade entre as classes. Padrão esse, que é confirmado
com a coluna Valid, que apresenta as frequências relativas de cada opção
de região.

Podemos fazer a mesma análise para os dados de estado civil, os quais
podemos estar interessados em buscar evidência se há mais funcionários
casados ou solteiros na empresa. A seguir, temos a tabela destas
proporções, onde é perceptível que há maior proporção de funcionários
casados.

``` r
salarios %>% dplyr::select(estado_civil) %>% summarytools::freq()
```

    ## Frequencies  
    ## salarios$estado_civil  
    ## Type: Character  
    ## 
    ##                  Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## -------------- ------ --------- -------------- --------- --------------
    ##         casado     20     55.56          55.56     55.56          55.56
    ##       solteiro     16     44.44         100.00     44.44         100.00
    ##           <NA>      0                               0.00         100.00
    ##          Total     36    100.00         100.00    100.00         100.00

É importante destacar, que lemos a coluna Valid sem nos preocupar nestes
casos, pois não há dados faltantes para nenhumas das duas variáveis.

Por fim, podemos criar tabelas de frequências para uma variável
quantitativa discreta, como é o caso do número de filhos dos
funcionários da empresa.

``` r
salarios %>% dplyr::select(n_filhos) %>% summarytools::freq()
```

    ## Frequencies  
    ## salarios$n_filhos  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0      4     20.00          20.00     11.11          11.11
    ##           1      5     25.00          45.00     13.89          25.00
    ##           2      7     35.00          80.00     19.44          44.44
    ##           3      3     15.00          95.00      8.33          52.78
    ##           5      1      5.00         100.00      2.78          55.56
    ##        <NA>     16                              44.44         100.00
    ##       Total     36    100.00         100.00    100.00         100.00

Como há dados faltantes para essa variável, é importante o analista
determinar qual o espaço amostral está interessado em focar sua análise.

A fim de ser comparável às análises pregressas, é importante que as
frequências absoluta e relativa do total de dados seja considerada, isto
é, leitura da coluna Total, a fim de manter o mesmo espaço amostral.

Caso, ele esteja interessado em analisar apenas os dados válidos, ele
pode redefinir o espaço amostral, ler apenas a coluna Valid, porém
recalculando as tabelas anteriores, considerando os indivíduos apenas
com dados preenchidos para a variável filhos.

``` r
summarytools::ctable(x = salarios$Grau_de_instrucao, 
       y = salarios$regiao, 
       prop = "t") 
```

    ## Cross-Tabulation, Total Proportions  
    ## Grau_de_instrucao * regiao  
    ## Data Frame: salarios  
    ## 
    ## -------------------- -------- ------------ ------------ ------------ -------------
    ##                        regiao      capital     interior        outra         Total
    ##    Grau_de_instrucao                                                              
    ##   ensino fundamental             4 (11.1%)    3 ( 8.3%)    5 (13.9%)   12 ( 33.3%)
    ##         ensino médio             5 (13.9%)    7 (19.4%)    6 (16.7%)   18 ( 50.0%)
    ##             superior             2 ( 5.6%)    2 ( 5.6%)    2 ( 5.6%)    6 ( 16.7%)
    ##                Total            11 (30.6%)   12 (33.3%)   13 (36.1%)   36 (100.0%)
    ## -------------------- -------- ------------ ------------ ------------ -------------

``` r
summarytools::ctable(x = factor(salarios$n_filhos), 
       y = salarios$estado_civil, 
       prop = "t") 
```

    ## Cross-Tabulation, Total Proportions  
    ## factor(salarios$n_filhos) * estado_civil  
    ## 
    ## --------------------------- -------------- ------------ ------------ -------------
    ##                               estado_civil       casado     solteiro         Total
    ##   factor(salarios$n_filhos)                                                       
    ##                           0                   4 (11.1%)    0 ( 0.0%)    4 ( 11.1%)
    ##                           1                   5 (13.9%)    0 ( 0.0%)    5 ( 13.9%)
    ##                           2                   7 (19.4%)    0 ( 0.0%)    7 ( 19.4%)
    ##                           3                   3 ( 8.3%)    0 ( 0.0%)    3 (  8.3%)
    ##                           5                   1 ( 2.8%)    0 ( 0.0%)    1 (  2.8%)
    ##                        <NA>                   0 ( 0.0%)   16 (44.4%)   16 ( 44.4%)
    ##                       Total                  20 (55.6%)   16 (44.4%)   36 (100.0%)
    ## --------------------------- -------------- ------------ ------------ -------------

# Calculando o teste de correlação ponto bisserial

Para realizar o cálculo dessa correlação, devemos converter a variável
categória que estava em texto para binária (0,1). Fizemos através do
pacote básico, transformando convertendo para binário através da função
factor e transformando em numérica, pois a função recebe as variáveis em
seu formato numérico.

``` r
#Convertendo solteiro para 0 e casado para 1 com variável binária e calculando o coeficiente de correlação ponto-bisserial
cor.test(x = salarios$salario, 
       y = as.numeric(as.character(factor(salarios$estado_civil, levels = c("solteiro","casado"), labels = c(0,1)))))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  salarios$salario and as.numeric(as.character(factor(salarios$estado_civil, levels = c("solteiro", "casado"), labels = c(0, 1))))
    ## t = 1.4894, df = 34, p-value = 0.1456
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08822049  0.53271009
    ## sample estimates:
    ##       cor 
    ## 0.2474883

Reparem que, embora tenha calculado uma relação entre as categórias de
estado civil e salário, apontando para uma fraca relação entre ser
casado e ganhar maiores salários, essa valor não é significativo ao
nível de 95% (papo para futuro, mas é possível checar pelo p-valor e
pelo intervalo de confiança).
