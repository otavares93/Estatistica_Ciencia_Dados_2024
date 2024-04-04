library(tidyverse)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)

shinyUI(
  fluidPage(
    navbarPage("Aula 7 - Dashboardo Inicial",
               
               ######################################
               ## Primeira Aba (Gráfico de Barras) ##
               ######################################
               
               tabPanel("Gráfico de Barras", 
                        #Introduzindo a primeira página com sugestões de leitura 
                        mainPanel(plotOutput("grafico_barras"), height = "2000px", width = "1000px")
               ),
               
               ######################################
               ##         Segunda Aba (Mapa)       ##
               ######################################
               
               tabPanel("Mapa com as informações de População", 
                        #Introduzindo a primeira página com sugestões de leitura 
                        mainPanel(plotOutput("grafico_mapa"), height = "2000px", width = "1000px")
               )
            )
          )
        )
