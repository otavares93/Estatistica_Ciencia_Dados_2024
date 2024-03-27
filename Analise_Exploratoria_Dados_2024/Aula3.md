Visualização da Distribuição de Dados
================
Otto Tavares
2023-02-13

## Introdução

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
library(purrr)
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
library(ggpubr)
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(rcompanion)
library(naniar)
library(mice)
```

    ## 
    ## Attaching package: 'mice'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 32245 Columns: 63
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr  (3): mes_ano, munic, Regiao
    ## dbl (60): CISP, mes, ano, AISP, RISP, mcirc, hom_doloso, lesao_corp_morte, l...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Limpando os dados e realizando diagnóstico de dados faltantes

1.  Via diagnose, que já conhecemos, ou miss_var_summary da biblioteca
    naniar. Descrição estatística da distribuição de missing calculando
    as frequências relativas por variável

2.  Via visualização de dados faltantes através de um gráfico de barras
    percentual, gerado pela biblioteca naniar e função vis_miss

3.  Via ordenamento de variáveis com maior frequência de missing

<!-- -->

    ## # A tibble: 63 × 6
    ##    variables  types     missing_count missing_percent unique_count unique_rate
    ##    <chr>      <chr>             <int>           <dbl>        <int>       <dbl>
    ##  1 CISP       numeric               0               0          138    0.00428 
    ##  2 mes        numeric               0               0           12    0.000372
    ##  3 ano        numeric               0               0           20    0.000620
    ##  4 mes_ano    character             0               0          240    0.00744 
    ##  5 AISP       numeric               0               0           41    0.00127 
    ##  6 RISP       numeric               0               0            7    0.000217
    ##  7 munic      character             0               0           90    0.00279 
    ##  8 mcirc      numeric               0               0           90    0.00279 
    ##  9 Regiao     character             0               0            4    0.000124
    ## 10 hom_doloso numeric               0               0           40    0.00124 
    ## # ℹ 53 more rows

    ## # A tibble: 63 × 3
    ##    variable                   n_miss pct_miss
    ##    <chr>                       <int>    <num>
    ##  1 roubo_bicicleta             17389  53.9   
    ##  2 furto_bicicleta             17389  53.9   
    ##  3 posse_drogas                 4646  14.4   
    ##  4 trafico_drogas               4646  14.4   
    ##  5 apreensao_drogas_sem_autor   4646  14.4   
    ##  6 apf                          4646  14.4   
    ##  7 aaapai                       4646  14.4   
    ##  8 cmp                          4646  14.4   
    ##  9 cmba                         4646  14.4   
    ## 10 roubo_cx_eletronico            19   0.0589
    ## # ℹ 53 more rows

![](Aula3_files/figure-gfm/filtrando%20os%20dados%20e%20descrevendo%20os%20dados%20faltantes-1.png)<!-- -->![](Aula3_files/figure-gfm/filtrando%20os%20dados%20e%20descrevendo%20os%20dados%20faltantes-2.png)<!-- -->

# Realizando o teste de Little para checar se os dados faltantes são completamente aleatórios

## Para os dados de furtos e roubos

    ## # A tibble: 1 × 4
    ##   statistic    df p.value missing.patterns
    ##       <dbl> <dbl>   <dbl>            <int>
    ## 1     8147.    39       0                3

## Para os dados de posse de drogas

    ## # A tibble: 1 × 4
    ##   statistic    df p.value missing.patterns
    ##       <dbl> <dbl>   <dbl>            <int>
    ## 1      256.     1       0                2

# Realizando análise da relação entre a distribuição de missing e das variáveis observadas

## Para roubo de bicicleta

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

| AISP | Regiao             |    n | tot.miss | tot.miss.regiao | freq.intra.regiao | freq.regiao |
|-----:|:-------------------|-----:|---------:|----------------:|------------------:|------------:|
|   10 | Interior           | 1257 |    17389 |            8477 |         0.1482836 |   0.4874921 |
|   11 | Interior           | 1047 |    17389 |            8477 |         0.1235107 |   0.4874921 |
|   25 | Interior           |  828 |    17389 |            8477 |         0.0976761 |   0.4874921 |
|   29 | Interior           |  701 |    17389 |            8477 |         0.0826943 |   0.4874921 |
|    8 | Interior           |  653 |    17389 |            8477 |         0.0770320 |   0.4874921 |
|   36 | Interior           |  645 |    17389 |            8477 |         0.0760882 |   0.4874921 |
|   32 | Interior           |  592 |    17389 |            8477 |         0.0698360 |   0.4874921 |
|   35 | Interior           |  561 |    17389 |            8477 |         0.0661791 |   0.4874921 |
|   33 | Interior           |  417 |    17389 |            8477 |         0.0491919 |   0.4874921 |
|   30 | Interior           |  408 |    17389 |            8477 |         0.0481302 |   0.4874921 |
|   28 | Interior           |  402 |    17389 |            8477 |         0.0474224 |   0.4874921 |
|   38 | Interior           |  387 |    17389 |            8477 |         0.0456529 |   0.4874921 |
|   37 | Interior           |  303 |    17389 |            8477 |         0.0357438 |   0.4874921 |
|   26 | Interior           |  264 |    17389 |            8477 |         0.0311431 |   0.4874921 |
|   34 | Interior           |   12 |    17389 |            8477 |         0.0014156 |   0.4874921 |
|    9 | Capital            |  714 |    17389 |            5192 |         0.1375193 |   0.2985796 |
|    3 | Capital            |  660 |    17389 |            5192 |         0.1271186 |   0.2985796 |
|    6 | Capital            |  366 |    17389 |            5192 |         0.0704931 |   0.2985796 |
|   14 | Capital            |  366 |    17389 |            5192 |         0.0704931 |   0.2985796 |
|    5 | Capital            |  327 |    17389 |            5192 |         0.0629815 |   0.2985796 |
|   16 | Capital            |  265 |    17389 |            5192 |         0.0510401 |   0.2985796 |
|   23 | Capital            |  265 |    17389 |            5192 |         0.0510401 |   0.2985796 |
|    2 | Capital            |  264 |    17389 |            5192 |         0.0508475 |   0.2985796 |
|   18 | Capital            |  264 |    17389 |            5192 |         0.0508475 |   0.2985796 |
|   19 | Capital            |  264 |    17389 |            5192 |         0.0508475 |   0.2985796 |
|    1 | Capital            |  204 |    17389 |            5192 |         0.0392912 |   0.2985796 |
|   39 | Capital            |  198 |    17389 |            5192 |         0.0381356 |   0.2985796 |
|    4 | Capital            |  192 |    17389 |            5192 |         0.0369800 |   0.2985796 |
|   31 | Capital            |  177 |    17389 |            5192 |         0.0340909 |   0.2985796 |
|   27 | Capital            |  162 |    17389 |            5192 |         0.0312018 |   0.2985796 |
|   17 | Capital            |  132 |    17389 |            5192 |         0.0254237 |   0.2985796 |
|   22 | Capital            |  132 |    17389 |            5192 |         0.0254237 |   0.2985796 |
|   41 | Capital            |  108 |    17389 |            5192 |         0.0208012 |   0.2985796 |
|   13 | Capital            |   99 |    17389 |            5192 |         0.0190678 |   0.2985796 |
|   40 | Capital            |   30 |    17389 |            5192 |         0.0057781 |   0.2985796 |
|   36 | Capital            |    3 |    17389 |            5192 |         0.0005778 |   0.2985796 |
|   20 | Baixada Fluminense |  695 |    17389 |            2400 |         0.2895833 |   0.1380183 |
|   24 | Baixada Fluminense |  648 |    17389 |            2400 |         0.2700000 |   0.1380183 |
|   15 | Baixada Fluminense |  528 |    17389 |            2400 |         0.2200000 |   0.1380183 |
|   34 | Baixada Fluminense |  300 |    17389 |            2400 |         0.1250000 |   0.1380183 |
|   21 | Baixada Fluminense |  129 |    17389 |            2400 |         0.0537500 |   0.1380183 |
|   40 | Baixada Fluminense |   70 |    17389 |            2400 |         0.0291667 |   0.1380183 |
|   39 | Baixada Fluminense |   30 |    17389 |            2400 |         0.0125000 |   0.1380183 |
|   12 | Grande Niterói     |  792 |    17389 |            1320 |         0.6000000 |   0.0759101 |
|    7 | Grande Niterói     |  528 |    17389 |            1320 |         0.4000000 |   0.0759101 |

## Para posse de drogas

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

| AISP | Regiao             |   n | tot.miss | tot.miss.regiao | freq.intra.regiao | freq.regiao |
|-----:|:-------------------|----:|---------:|----------------:|------------------:|------------:|
|   10 | Interior           | 357 |     4646 |            2249 |         0.1587372 |   0.4840723 |
|   11 | Interior           | 327 |     4646 |            2249 |         0.1453980 |   0.4840723 |
|   25 | Interior           | 216 |     4646 |            2249 |         0.0960427 |   0.4840723 |
|   29 | Interior           | 180 |     4646 |            2249 |         0.0800356 |   0.4840723 |
|    8 | Interior           | 173 |     4646 |            2249 |         0.0769231 |   0.4840723 |
|   36 | Interior           | 165 |     4646 |            2249 |         0.0733659 |   0.4840723 |
|   32 | Interior           | 141 |     4646 |            2249 |         0.0626945 |   0.4840723 |
|   35 | Interior           | 141 |     4646 |            2249 |         0.0626945 |   0.4840723 |
|   28 | Interior           | 114 |     4646 |            2249 |         0.0506892 |   0.4840723 |
|   33 | Interior           |  99 |     4646 |            2249 |         0.0440196 |   0.4840723 |
|   38 | Interior           |  99 |     4646 |            2249 |         0.0440196 |   0.4840723 |
|   37 | Interior           |  81 |     4646 |            2249 |         0.0360160 |   0.4840723 |
|   26 | Interior           |  72 |     4646 |            2249 |         0.0320142 |   0.4840723 |
|   30 | Interior           |  72 |     4646 |            2249 |         0.0320142 |   0.4840723 |
|   34 | Interior           |  12 |     4646 |            2249 |         0.0053357 |   0.4840723 |
|    9 | Capital            | 216 |     4646 |            1401 |         0.1541756 |   0.3015497 |
|    3 | Capital            | 180 |     4646 |            1401 |         0.1284797 |   0.3015497 |
|    6 | Capital            | 108 |     4646 |            1401 |         0.0770878 |   0.3015497 |
|   14 | Capital            | 108 |     4646 |            1401 |         0.0770878 |   0.3015497 |
|    5 | Capital            |  75 |     4646 |            1401 |         0.0535332 |   0.3015497 |
|    1 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|    2 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|   16 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|   18 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|   19 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|   23 | Capital            |  72 |     4646 |            1401 |         0.0513919 |   0.3015497 |
|   39 | Capital            |  66 |     4646 |            1401 |         0.0471092 |   0.3015497 |
|    4 | Capital            |  36 |     4646 |            1401 |         0.0256959 |   0.3015497 |
|   17 | Capital            |  36 |     4646 |            1401 |         0.0256959 |   0.3015497 |
|   22 | Capital            |  36 |     4646 |            1401 |         0.0256959 |   0.3015497 |
|   27 | Capital            |  36 |     4646 |            1401 |         0.0256959 |   0.3015497 |
|   31 | Capital            |  36 |     4646 |            1401 |         0.0256959 |   0.3015497 |
|   13 | Capital            |  33 |     4646 |            1401 |         0.0235546 |   0.3015497 |
|   36 | Capital            |   3 |     4646 |            1401 |         0.0021413 |   0.3015497 |
|   20 | Baixada Fluminense | 215 |     4646 |             636 |         0.3380503 |   0.1368920 |
|   24 | Baixada Fluminense | 168 |     4646 |             636 |         0.2641509 |   0.1368920 |
|   15 | Baixada Fluminense | 144 |     4646 |             636 |         0.2264151 |   0.1368920 |
|   34 | Baixada Fluminense |  72 |     4646 |             636 |         0.1132075 |   0.1368920 |
|   21 | Baixada Fluminense |  33 |     4646 |             636 |         0.0518868 |   0.1368920 |
|   40 | Baixada Fluminense |   4 |     4646 |             636 |         0.0062893 |   0.1368920 |
|   12 | Grande Niterói     | 216 |     4646 |             360 |         0.6000000 |   0.0774860 |
|    7 | Grande Niterói     | 144 |     4646 |             360 |         0.4000000 |   0.0774860 |

    ## Adding missing grouping variables: `AISP`

# Comparando a imputação dos dados com o realizado para posse de drogas para o método knn

    ## Adding missing grouping variables: `AISP`
    ## Adding missing grouping variables: `AISP`

    ## Warning: Removed 36 rows containing missing values (`geom_line()`).

![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20knn%20com%20o%20original-1.png)<!-- -->![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20knn%20com%20o%20original-2.png)<!-- -->

# Comparando a imputação dos dados com o realizado para posse de drogas para o método de imputação multipla

![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20pmm%20com%20o%20original-1.png)<!-- -->

# Comparando a imputação dos dados com o realizado para os dados de salário para o método de imputação multipla

## Com o método pmm

    ## Warning: Number of logged events: 25

    ## Warning: Removed 16 rows containing non-finite values (`stat_density()`).

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-1.png)<!-- -->![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-2.png)<!-- -->![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-3.png)<!-- -->

## Com o método midastouch (pmm ponderado)

    ## Warning: Number of logged events: 25

    ## Warning: Removed 16 rows containing non-finite values (`stat_density()`).

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-1.png)<!-- -->![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-2.png)<!-- -->![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-3.png)<!-- -->
