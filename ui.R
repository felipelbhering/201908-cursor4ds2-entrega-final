library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
#library(DT)

data_vehicules <-
  readr::read_rds("data/2020_09_fipe_top_veiculos.rds")


format_real <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".") %>%
    stringr::str_trim() %>%
    stringr::str_c("R$ ", .)
}

variacao_ano <- function(df,df_completo , n_lag =1){
  df %>% 
    semi_join(df_completo , by="model") %>% 
    mutate(year =case_when( 
      year=="0 km" ~ "N_0",
      year=="2019" ~ "N_1",
      year=="2018" ~ "N_2",
      year=="2017" ~ "N_3",
      year=="2016" ~ "N_4",
      year=="2015" ~ "N_5",
      year=="2014" ~ "N_6",
      TRUE ~NA_character_)) %>% 
    arrange(model,year) %>% 
    #tidyr::pivot_wider(names_from = year, values_from=price ) %>%
    #data.frame() %>% 
    #filter(!is.na(N_6), !is.na(N_0)) %>% 
    group_by(model) %>% 
    mutate(pct_change = (lead(price,n_lag)/price - 1) * 100,
           custo_depreciacao = price-lead(price,n_lag)) %>% 
    mutate(year = 
             forcats::fct_recode(year,
                                 "Comprou Novo" = "N_0",
                                 "Comprou com 1 Ano Uso (2019)" = "N_1",
                                 "Comprou com 2 Anos Uso (2018)" = "N_2",
                                 "Comprou com 3 Anos Uso (2017)" = "N_3",
                                 "Comprou com 4 Anos Uso (2016)" = "N_4",
                                 "Comprou com 5 Anos Uso (2015)" = "N_5")) %>% 
    filter(! is.na(pct_change ))
}


modelos_abaixo_55 <- 
  data_vehicules %>% 
  filter(year=="0 km") %>% 
  filter(price<55000) %>% 
  pull(model)

data_vehicules <- data_vehicules %>% 
  filter((model %in% modelos_abaixo_55)) %>%
  distinct()

carros_completos <- data_vehicules %>%
  group_by(model) %>% 
  count(sort=TRUE) 

data_vehicules <- 
  data_vehicules %>%
  filter(model %in% (carros_completos %>% filter(n>1) %>% pull(model)))

carros_completos5 <-  data_vehicules %>%
  group_by(model) %>% 
  count(sort=TRUE) %>% 
  filter(n>=5)
##########################

#Ui

ui =  navbarPage("", #criando abas
      tabPanel("Geral",
      sidebarLayout(
  sidebarPanel(
    selectInput("taxa_selic1", 
                label = "Taxa Selic:",
                choices=seq(0.01,0.15,0.01))
  ),
  mainPanel(
    #textOutput('output$taxa_selic'),  #TableOutput('table')
    #dataTableOutput("table")
    h2("Top Carros mais baratos"),
    DT::dataTableOutput("table"),
    plotOutput("plot")
  )
)),
tabPanel("Filtros",
         sidebarLayout(
           sidebarPanel(
             selectInput("model", 
                         label = "Modelo Escolhido:",
                         choices= data_vehicules$model %>% unique()),
             
             selectInput("taxa_selic2", 
                         label = "Taxa Selic:",
                         choices=seq(0.01,0.15,0.01)),
             textInput("custos_seguro",
                       label = "Custos Seguro (R$) Ano ", value = "2500"),
             verbatimTextOutput("value")
           ),
           mainPanel(
             #textOutput('output$taxa_selic'),  #TableOutput('table')
             #dataTableOutput("table")

             h2("Carro filtrado"), 
             DT::dataTableOutput("table2")
           )
         )))

shinyApp(ui, server)

