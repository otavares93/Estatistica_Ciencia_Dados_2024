Análise descritiva de uma base de dados
================
Otto Tavares
2023-02-13

## Introdução

Na Aula 7, temos o objetivo de abrir uma base de dados e dar os
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

#crimes.furtos %>% dplyr::filter(mes_ano == "2022m12") %>% diagnose()

#crimes.furtos %>% dplyr::filter(mes_ano == "2022m12") %>% dfSummary() %>% view()
```

A base trabalhada nesta aula, será a base de dados hipotética
disponbilizada no livro texto dos autores Bussab e Moretim. Vamos
importá-la e imprimir as primeiras observações para conhecimento das
variáveis.

``` r
kable(salarios)
```

|   n | estado_civil | Grau_de_instrucao  | n_filhos | salario | idade_anos | idade_meses | regiao   |
|----:|:-------------|:-------------------|---------:|--------:|-----------:|------------:|:---------|
|   1 | solteiro     | ensino fundamental |       NA |    4.00 |         26 |           3 | interior |
|   2 | casado       | ensino fundamental |        1 |    4.56 |         32 |          10 | capital  |
|   3 | casado       | ensino fundamental |        2 |    5.25 |         36 |           5 | capital  |
|   4 | solteiro     | ensino médio       |       NA |    5.73 |         20 |          10 | outra    |
|   5 | solteiro     | ensino fundamental |       NA |    6.26 |         40 |           7 | outra    |
|   6 | casado       | ensino fundamental |        0 |    6.66 |         28 |           0 | interior |
|   7 | solteiro     | ensino fundamental |       NA |    6.86 |         41 |           0 | interior |
|   8 | solteiro     | ensino fundamental |       NA |    7.39 |         43 |           4 | capital  |
|   9 | casado       | ensino médio       |        1 |    7.59 |         34 |          10 | capital  |
|  10 | solteiro     | ensino médio       |       NA |    7.44 |         23 |           6 | outra    |
|  11 | casado       | ensino médio       |        2 |    8.12 |         33 |           6 | interior |
|  12 | solteiro     | ensino fundamental |       NA |    8.46 |         27 |          11 | capital  |
|  13 | solteiro     | ensino médio       |       NA |    8.74 |         37 |           5 | outra    |
|  14 | casado       | ensino fundamental |        3 |    8.95 |         44 |           2 | outra    |
|  15 | casado       | ensino médio       |        0 |    9.13 |         30 |           5 | interior |
|  16 | solteiro     | ensino médio       |       NA |    9.35 |         38 |           8 | outra    |
|  17 | casado       | ensino médio       |        1 |    9.77 |         31 |           7 | capital  |
|  18 | casado       | ensino fundamental |        2 |    9.80 |         39 |           7 | outra    |
|  19 | solteiro     | superior           |       NA |   10.53 |         25 |           8 | interior |
|  20 | solteiro     | ensino médio       |       NA |   10.76 |         37 |           4 | interior |
|  21 | casado       | ensino médio       |        1 |   11.06 |         30 |           9 | outra    |
|  22 | solteiro     | ensino médio       |       NA |   11.59 |         34 |           2 | capital  |
|  23 | solteiro     | ensino fundamental |       NA |   12.00 |         41 |           0 | outra    |
|  24 | casado       | superior           |        0 |   12.79 |         26 |           1 | outra    |
|  25 | casado       | ensino médio       |        2 |   13.23 |         32 |           5 | interior |
|  26 | casado       | ensino médio       |        2 |   13.60 |         35 |           0 | outra    |
|  27 | solteiro     | ensino fundamental |       NA |   13.85 |         46 |           7 | outra    |
|  28 | casado       | ensino médio       |        0 |   14.69 |         29 |           8 | interior |
|  29 | casado       | ensino médio       |        5 |   14.71 |         40 |           6 | interior |
|  30 | casado       | ensino médio       |        2 |   15.99 |         35 |          10 | capital  |
|  31 | solteiro     | superior           |       NA |   16.22 |         31 |           5 | outra    |
|  32 | casado       | ensino médio       |        1 |   16.61 |         36 |           4 | interior |
|  33 | casado       | superior           |        3 |   17.26 |         43 |           7 | capital  |
|  34 | solteiro     | superior           |       NA |   18.75 |         33 |           7 | capital  |
|  35 | casado       | ensino médio       |        2 |   19.40 |         48 |          11 | capital  |
|  36 | casado       | superior           |        3 |   23.30 |         42 |           2 | interior |

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
salarios %>% dplyr::select(regiao) %>% summarytools::freq(., style = 'rmarkdown') %>% kable()
```

    ## setting plain.ascii to FALSE

|          | Freq |   % Valid | % Valid Cum. |   % Total | % Total Cum. |
|:---------|-----:|----------:|-------------:|----------:|-------------:|
| capital  |   11 |  30.55556 |     30.55556 |  30.55556 |     30.55556 |
| interior |   12 |  33.33333 |     63.88889 |  33.33333 |     63.88889 |
| outra    |   13 |  36.11111 |    100.00000 |  36.11111 |    100.00000 |
| <NA>     |    0 |        NA |           NA |   0.00000 |    100.00000 |
| Total    |   36 | 100.00000 |    100.00000 | 100.00000 |    100.00000 |

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
salarios %>% dplyr::select(estado_civil) %>% summarytools::freq(., style = 'rmarkdown') %>% kable()
```

    ## setting plain.ascii to FALSE

|          | Freq |   % Valid | % Valid Cum. |   % Total | % Total Cum. |
|:---------|-----:|----------:|-------------:|----------:|-------------:|
| casado   |   20 |  55.55556 |     55.55556 |  55.55556 |     55.55556 |
| solteiro |   16 |  44.44444 |    100.00000 |  44.44444 |    100.00000 |
| <NA>     |    0 |        NA |           NA |   0.00000 |    100.00000 |
| Total    |   36 | 100.00000 |    100.00000 | 100.00000 |    100.00000 |

É importante destacar, que lemos a coluna Valid sem nos preocupar nestes
casos, pois não há dados faltantes para nenhumas das duas variáveis.

Por fim, podemos criar tabelas de frequências para uma variável
quantitativa discreta, como é o caso do número de filhos dos
funcionários da empresa.

``` r
salarios %>% dplyr::select(n_filhos) %>% summarytools::freq(., style = 'rmarkdown') %>% kable()
```

    ## setting plain.ascii to FALSE

|       | Freq | % Valid | % Valid Cum. |    % Total | % Total Cum. |
|:------|-----:|--------:|-------------:|-----------:|-------------:|
| 0     |    4 |      20 |           20 |  11.111111 |     11.11111 |
| 1     |    5 |      25 |           45 |  13.888889 |     25.00000 |
| 2     |    7 |      35 |           80 |  19.444444 |     44.44444 |
| 3     |    3 |      15 |           95 |   8.333333 |     52.77778 |
| 5     |    1 |       5 |          100 |   2.777778 |     55.55556 |
| <NA>  |   16 |      NA |           NA |  44.444444 |    100.00000 |
| Total |   36 |     100 |          100 | 100.000000 |    100.00000 |

Como há dados faltantes para essa variável, é importante o analista
determinar qual o espaço amostral está interessado em focar sua análise.

A fim de ser comparável às análises pregressas, é importante que as
frequências absoluta e relativa do total de dados seja considerada, isto
é, leitura da coluna Total, a fim de manter o mesmo espaço amostral.

Caso, ele esteja interessado em analisar apenas os dados válidos, ele
pode redefinir o espaço amostral, ler apenas a coluna Valid, porém
recalculando as tabelas anteriores, considerando os indivíduos apenas
com dados preenchidos para a variável filhos.

\##Análise descritiva e de histogramas de uma variável contínua  

Já para a variável salários, podemos analisar a centralidade dos dados,
dipersão, assimetria, bem como suas estatísticas de ordem, a fim de
checar se há presença de outliers.

Para realizar essa análise, podemos utilizar a função descr do pacote
summarytools, e posteriormente realizar a leitura desses dados.

``` r
salarios %>% dplyr::select(salario) %>% summarytools::descr(., style = 'rmarkdown') %>% kable()
```

|             |     salario |
|:------------|------------:|
| Mean        |  11.1222222 |
| Std.Dev     |   4.5874575 |
| Min         |   4.0000000 |
| Q1          |   7.5150000 |
| Median      |  10.1650000 |
| Q3          |  14.2700000 |
| Max         |  23.3000000 |
| MAD         |   4.7220810 |
| IQR         |   6.5075000 |
| CV          |   0.4124587 |
| Skewness    |   0.5997938 |
| SE.Skewness |   0.3925439 |
| Kurtosis    |  -0.3291263 |
| N.Valid     |  36.0000000 |
| Pct.Valid   | 100.0000000 |

``` r
salarios %>% summarytools::dfSummary() %>% kable()
```

<table>
<colgroup>
<col style="width: 0%" />
<col style="width: 2%" />
<col style="width: 6%" />
<col style="width: 5%" />
<col style="width: 76%" />
<col style="width: 6%" />
<col style="width: 1%" />
<col style="width: 0%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">No</th>
<th style="text-align: left;">Variable</th>
<th style="text-align: left;">Stats / Values</th>
<th style="text-align: left;">Freqs (% of Valid)</th>
<th style="text-align: left;">Graph</th>
<th style="text-align: left;">text.graph</th>
<th style="text-align: left;">Valid</th>
<th style="text-align: left;">Missing</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: left;">n<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 18.5 (10.5)<br />
min &lt; med &lt; max:<br />
1 &lt; 18.5 &lt; 36<br />
IQR (CV) : 17.5 (0.6)</td>
<td style="text-align: left;">36 distinct values</td>
<td
style="text-align: left;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuCAQAAABxABKuAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAABQUlEQVR42u3dwQ2CMABAUTHs6A46j+7AlHqtXuBFozH8dyMUaH5SDr10uh8ijr+ewL8pGCoYmseL2w5/aJdpfcyYZX6+dVp9eFkd84kR3/rOsjqPVy1JVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwNL//iv923XLownBIwe6DbTkKYdSSRAVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUPTuKW9x0OFtzgPe/pTjUxLEhUMFQw9ADFtEnTa3WH9AAAAPXRFWHRpY2M6Y29weXJpZ2h0AENvcHlyaWdodCAyMDA3IEFwcGxlIEluYy4sIGFsbCByaWdodHMgcmVzZXJ2ZWQunmbcKQAAACN0RVh0aWNjOmRlc2NyaXB0aW9uAEdlbmVyaWMgUkdCIFByb2ZpbGUapziOAAAAAElFTkSuQmCC"></td>
<td style="text-align: left;">: : : : : : :<br />
: : : : : : :<br />
: : : : : : :<br />
: : : : : : :<br />
: : : : : : : :</td>
<td style="text-align: left;">36<br />
(100.0%)</td>
<td style="text-align: left;">0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td style="text-align: right;">2</td>
<td style="text-align: left;">estado_civil<br />
[character]</td>
<td style="text-align: left;">1. casado<br />
2. solteiro</td>
<td style="text-align: left;">\20 (55.6%)<br />
\16 (44.4%)</td>
<td
style="text-align: left;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGAAAAA3CAQAAABZGof8AAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAAAnklEQVRo3u3YTQqAIBQAYY3u2B3qPHUHT1lbC4J+oPHFfDs34sAzyLym2Dr6AAbQB3irrxdLkAsx5pOAlAb6bBeU3Sr8CBlAM4BmAM0AmgE0A2gG0AygGUA7/BOXZ7uAcv0QkR9v8636zOFHKHzA7g7MTTxsTbcmubmHrbufkfAjZADNAJoBNANoBtAMoBlAM4BmAO1fD1sRhR8hA2gbxtoLxysv0vsAAAA9dEVYdGljYzpjb3B5cmlnaHQAQ29weXJpZ2h0IDIwMDcgQXBwbGUgSW5jLiwgYWxsIHJpZ2h0cyByZXNlcnZlZC6eZtwpAAAAI3RFWHRpY2M6ZGVzY3JpcHRpb24AR2VuZXJpYyBSR0IgUHJvZmlsZRqnOI4AAAAASUVORK5CYII="></td>
<td style="text-align: left;">IIIIIIIIIII \</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: right;">IIIIIIII</td>
<td style="text-align: left;">36<br />
(100.0%)</td>
<td style="text-align: left;">0<br />
(0.0%)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: right;">3</td>
<td style="text-align: left;">Grau_de_instrucao<br />
[character]</td>
<td style="text-align: left;">1. ensino fundamental<br />
2. ensino médio<br />
3. superior</td>
<td style="text-align: left;">\12 (33.3%)<br />
\18 (50.0%)<br />
 6 (16.7%)</td>
<td
style="text-align: left;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFgAAABOCAQAAAAOExk5AAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAAAu0lEQVRo3u3awQkDIRAF0Bi2xxSRepIeUuXuVXMQBqNx4P3bsgjv8BFmsJy3XLn/GwC8W476472k0M8SPVGzjvbXYzr3M3g+XSWAgYGBgYGBtwowMDBwP18j0ugAMz+lHvDC0+Gi1MZ0lUgHbjr82nXRVnV1+V4invYiSFcJYGBgYGBg4K0CDAwM3I+9xIzYSwB3MrxIiT/X+CE4vkhZf6ukqwQwMDAwMDDwVgEGBgbuJ/ciJUPSVQJ4di4JOhI3Zx/6egAAAD10RVh0aWNjOmNvcHlyaWdodABDb3B5cmlnaHQgMjAwNyBBcHBsZSBJbmMuLCBhbGwgcmlnaHRzIHJlc2VydmVkLp5m3CkAAAAjdEVYdGljYzpkZXNjcmlwdGlvbgBHZW5lcmljIFJHQiBQcm9maWxlGqc4jgAAAABJRU5ErkJggg=="></td>
<td style="text-align: left;">IIIIII \</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

IIIIIIIIII   III \|36  
(100.0%) \|0  
(0.0%) \| \| 4\|n_filhos  
\[numeric\] \|Mean (sd) : 1.6 (1.3)  
min \< med \< max:  
0 \< 2 \< 5  
IQR (CV) : 1 (0.8) \|0 : 4 (20.0%)  
1 : 5 (25.0%)  
2 : 7 (35.0%)  
3 : 3 (15.0%)  
5 : 1 ( 5.0%)
\|<img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEEAAAB9CAQAAABXRsYYAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAAA9ElEQVRo3u3ayw3CMBCE4TFKjxRBPdADVcKByzqHxK/gsfTvDRHkT5qN5JFIH82e22wAhN9s8cOrazEeqfzZeNCWf3VvBrybf2kQBAQIECBAgAABwtnsbk3td5/2SfEWV3H565x4qkEQEKTdOj4HFszyVjGsR+RT82YZBAEBAgQIECBAgLAAgSojWQRhQLisR5xMyPyiHnE8+dIbBAEBAgQIECBAgLAAgR4hWQRhQOjoETX/Yyom1PSIcYtrEAQECBAgQIAAAcICBHqEZBGEAeGwR4xqChWEvEf8azUNgoAAAQIECBAgQFiA4NYj5oxBEBAk6Qv2jB3vOmOAlQAAAD10RVh0aWNjOmNvcHlyaWdodABDb3B5cmlnaHQgMjAwNyBBcHBsZSBJbmMuLCBhbGwgcmlnaHRzIHJlc2VydmVkLp5m3CkAAAAjdEVYdGljYzpkZXNjcmlwdGlvbgBHZW5lcmljIFJHQiBQcm9maWxlGqc4jgAAAABJRU5ErkJggg==">
\|IIII   IIIII   IIIIIII   III   I \|20  
(55.6%) \|16  
(44.4%) \| \| 5\|salario  
\[numeric\] \|Mean (sd) : 11.1 (4.6)  
min \< med \< max:  
4 \< 10.2 \< 23.3  
IQR (CV) : 6.5 (0.4) \|36 distinct values
\|<img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuCAQAAABxABKuAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAABg0lEQVR42u3cQUrDUABF0US6R/eg66l76Cp14iAVG3tDkzb1nJkIwV74wvMHx8+B4uXeP8DeCBYJFh2mX4wLH3Kc/UX4vvSxD2P68Q6Ln3Lm9eJ3Tvf+tDfmSEaCRYJFgkWCRYJFgkWCRYJFgkWCRYJFgkWCRYJFgkWCRYJFgkWCRYJFgkWCRVffSx69tTIMQ7rI/T+XtXMcyUiwSLBIsEiwSLBIsEiwSLBIsEiw6EYvBc+Zm+37e8N6g2DPNdodyUiwSLBIsEiwSLBIsEiwSLBIsOhsGm1/Wbu/nfljS269+/a3Mx3JSLBIsEiwSLBIsEiwSLBIsEiwSLBog3vJpZb9KWDtyf7AwS4N89NdJ7sjGQkWCRYJFgkWCRYJFgkWCRYJFgkWPfCWXGbtu/SnC7b2MHckI8EiwSLBIsEiwSLBIsEiwSLBoqebRsv8cWk82aCCfbt2gzqSkWCRYJFgkWCRYJFgkWCRYJFgkWDROF2dH/7f9K/eJuN71KhxJCPBIsGiL3elHrBbfQASAAAAPXRFWHRpY2M6Y29weXJpZ2h0AENvcHlyaWdodCAyMDA3IEFwcGxlIEluYy4sIGFsbCByaWdodHMgcmVzZXJ2ZWQunmbcKQAAACN0RVh0aWNjOmRlc2NyaXB0aW9uAEdlbmVyaWMgUkdCIFByb2ZpbGUapziOAAAAAElFTkSuQmCC">
\|  . :  
  : : .  
: : : : :   :  
: : : : : : : .  
: : : : : : : :   . \|36  
(100.0%) \|0  
(0.0%) \| \| 6\|idade_anos  
\[numeric\] \|Mean (sd) : 34.6 (6.7)  
min \< med \< max:  
20 \< 34.5 \< 48  
IQR (CV) : 10 (0.2) \|24 distinct values
\|<img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuCAQAAABxABKuAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAABaklEQVR42u3bwW3CMABA0aZiR3Yo87Q7MCW9uojLA8WkyX83JATWlxwltrPcPiI+3z2A/6ZgqGDoNH5YVvyjb75YXtYcDhmHfnr6V9iZvn2dNzDSlEQFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDE5eole0CzNoB2HAw2QOYtwPQlEQFQwVDBUMFQwVDBUMFQy/cuPp5nD146U5/m/fi62pKooKhgqGCoYKhgqGCoYKhDS9Rm1k7ALsJNuupoymJCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCob+nN455qsK5u640xFfVTBNSVQwVDBUMFQwVDBUMFQwtJtz+gafaYbXIA4a7PknmqYkKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGFrGxe2fTu889DWs6S81Mk1JVDBUMPQLa+gXrBjNldIAAAA9dEVYdGljYzpjb3B5cmlnaHQAQ29weXJpZ2h0IDIwMDcgQXBwbGUgSW5jLiwgYWxsIHJpZ2h0cyByZXNlcnZlZC6eZtwpAAAAI3RFWHRpY2M6ZGVzY3JpcHRpb24AR2VuZXJpYyBSR0IgUHJvZmlsZRqnOI4AAAAASUVORK5CYII=">
\|    :  
  . : :  
  : : : :  
. : : : :  
: : : : : : \|36  
(100.0%) \|0  
(0.0%) \| \| 7\|idade_meses  
\[numeric\] \|Mean (sd) : 5.6 (3.3)  
min \< med \< max:  
0 \< 6 \< 11  
IQR (CV) : 4.2 (0.6) \|12 distinct values
\|<img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuCAQAAABxABKuAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAABZklEQVR42u3cwW3CMABA0aZiR3aAedodOiW9Gk59QQ5p+O+GhCzrS0axHLPcPiI+Xz2B/6ZgqGDoNH5YXj2bwRf+uF4nTn6cymn1KNOd4bs/m82qJYkKhgqGCoYKhgqGCoYKhgqG7p70Z25HdOy9etgazdyOyNhbbnZMSxIVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDBUMFQwVDO34rpGxNxzXX+U6TLCtrnK1JFHBUMFQwVDBUMFQwVDBUMHQU0/6R7lwJZ4Kts9/lpirJYkKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGDvNCncEDwuF9xTcNtv6AsCWJCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoYKhgqGCoaW8fjk+w0vW/3FZTg1WmpkWpKoYKhg6Bd5TBeoUkPoZAAAAD10RVh0aWNjOmNvcHlyaWdodABDb3B5cmlnaHQgMjAwNyBBcHBsZSBJbmMuLCBhbGwgcmlnaHRzIHJlc2VydmVkLp5m3CkAAAAjdEVYdGljYzpkZXNjcmlwdGlvbgBHZW5lcmljIFJHQiBQcm9maWxlGqc4jgAAAABJRU5ErkJggg==">
\|.   . :  
:   : :  
:   : : :  
: : : : :  
: : : : : : \|36  
(100.0%) \|0  
(0.0%) \| \| 8\|regiao  
\[character\] \|1. capital  
2. interior  
3. outra \|\11 (30.6%)  
\12 (33.3%)  
\13 (36.1%)
\|<img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEMAAABOCAQAAADRXWMmAAAAAmJLR0QA/4ePzL8AAAAHdElNRQfoAhsRMxlG/7QGAAAAq0lEQVRo3u3XUQqAIBRE0Qz32CJcT+2hVdZX9TRBENQB7/yVUAeeguOuRSHraACMf7x9OBpulODSN/ZnPl7aGiHOwrrIUGDAgAEDBgwYMCZhJJfA0mWtVZy9mLrqz9TE/llkKCKMaG/sXQptyMy+U0/5kj8EIkOBAQMGDBgwYMCYhEFPoaeIMwbUpTdmK3avS0/iMykyFBgwYMCAAQMGjEkYinVpXESGAsPmBhyBEjeR48GVAAAAPXRFWHRpY2M6Y29weXJpZ2h0AENvcHlyaWdodCAyMDA3IEFwcGxlIEluYy4sIGFsbCByaWdodHMgcmVzZXJ2ZWQunmbcKQAAACN0RVh0aWNjOmRlc2NyaXB0aW9uAEdlbmVyaWMgUkdCIFByb2ZpbGUapziOAAAAAElFTkSuQmCC">
\|IIIIII   IIIIII   IIIIIII \|36  
(100.0%) \|0  
(0.0%) \| \##Análise visual da distribuição dos indivíduos por idade

### Com o boxplot

``` r
salarios %>% dplyr::select(Grau_de_instrucao, salario) %>% ggplot(aes(x=Grau_de_instrucao, y = salario)) + geom_boxplot() + xlab('Grau de instrução') + ylab('Salários') + theme_classic()
```

![](Aula7_files/figure-gfm/analisando%20salarios%20por%20grau%20instrucao%20com%20box%20-1.png)<!-- -->

### Com o violino

``` r
salarios %>% dplyr::select(Grau_de_instrucao, salario) %>% ggplot(aes(x=Grau_de_instrucao, y = salario)) + geom_violin() + xlab('Grau de instrução') + ylab('Salários') + theme_classic()
```

![](Aula7_files/figure-gfm/analisando%20salarios%20por%20grau%20instrucao%20com%20violin%20-1.png)<!-- -->

### Com o dotplot

``` r
salarios %>% dplyr::select(Grau_de_instrucao, salario) %>% ggplot(aes(x=Grau_de_instrucao, y = salario)) + geom_dotplot(binaxis = "y", stackdir = "center") + xlab('Grau de instrução') + ylab('Salários') + theme_classic()
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](Aula7_files/figure-gfm/analisando%20salarios%20por%20grau%20instrucao%20com%20dot%20-1.png)<!-- -->

### Unindo o dotplot com o box ou violin para melhor ilustrar a análise

``` r
salarios %>% dplyr::select(Grau_de_instrucao, salario) %>% ggplot(aes(x=Grau_de_instrucao, y = salario)) + geom_violin() + geom_dotplot(binaxis = "y", stackdir = "center") + xlab('Grau de instrução') + ylab('Salários') + theme_classic()
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](Aula7_files/figure-gfm/analisando%20salarios%20por%20grau%20instrucao%20com%20dot%20e%20violin%20-1.png)<!-- -->

\##Análise visual da variável salário

### Utilizando o número de bins indicado pelos autores do livro, bins igual a 5.

``` r
salarios %>% dplyr::select(salario) %>% ggplot(aes(x=salario))+geom_histogram(aes(y = after_stat(density)) ,bins = 5, fill = 'lightblue') + xlab('Salário') + ylab('Densidade de Frequência') + geom_vline(xintercept=c(median(salarios$salario), mean(salarios$salario))) + annotate("text", x=median(salarios$salario) + 0.3, y=0.05, label="Mediana", angle=90) + annotate("text", x=mean(salarios$salario) + 0.3, y=0.05, label="Média", angle=90) + theme_classic()
```

![](Aula7_files/figure-gfm/analisando%20salario%20visualmente%20-1.png)<!-- -->
\### Adicionando a densidade estimada via kernel à visualização

``` r
salarios %>% dplyr::select(salario) %>% ggplot(aes(x=salario))+geom_histogram(aes(y = after_stat(density)) ,bins = 5, fill = 'lightblue') + xlab('Salário') + ylab('Densidade de Frequência') + geom_vline(xintercept=c(median(salarios$salario), mean(salarios$salario))) + annotate("text", x=median(salarios$salario) + 0.3, y=0.05, label="Mediana", angle=90) + annotate("text", x=mean(salarios$salario) + 0.3, y=0.05, label="Média", angle=90) + geom_density(linetype = 2) + theme_classic()
```

![](Aula7_files/figure-gfm/analisando%20salario%20visualmente%20com%20kernel%20-1.png)<!-- -->

\##Análise visual da variável salário, utilizando a binarização a partir
de uma função customizada

### Definindo as funções gerais para criação de bins

``` r
#Freedman-Diaconis
fd_bins <- function(x)
{
  bins <- 2*IQR(x)/((length(x))^(1/3))
  return(bins)
}

#Sturge
s_bins <- function(x)
{
  bins <- 3.49*sd(x)/((length(x))^(1/3))
  return(bins)
}
```

### Cálculo do número de bins a partir da função de Freedman-Diaconis

``` r
salarios %>% dplyr::select(salario) %>% ggplot(aes(x=salario))+geom_histogram(aes(y = after_stat(density)) ,binwidth = fd_bins, fill = 'lightblue') + xlab('Salário') + ylab('Densidade de Frequência') + geom_vline(xintercept=c(median(salarios$salario), mean(salarios$salario))) + annotate("text", x=median(salarios$salario) + 0.3, y=0.05, label="Mediana", angle=90) + annotate("text", x=mean(salarios$salario) + 0.3, y=0.05, label="Média", angle=90) + geom_density(linetype = 2) + theme_classic()
```

![](Aula7_files/figure-gfm/analisando%20salario%20visualmente%20com%20fd-1.png)<!-- -->
