Carro Comprar (Financiado x não)
Novo ou seminovo?
ou Alugar?
================
Felipe
2020/09/13

##### Utilizaremos os dados da tabela FIPE de veículos.

##### A Fundação Instituto de Pesquisas Econômicas - FIPE é um órgão de apoio institucional ao Departamento de Economia da Faculdade de Economia, Administração e Contabilidade (FEA) da Universidade de São Paulo (USP). Um dos índices mais conhecidos e usados no Brasil é a Tabela FIPE Veículos que é atualizada todos os meses com o valor médio de venda de veículos. \[Wikipedia\]

#### Objetivo: Entender o custo mensal de um automóvel (carros abaixo 55mil novos), o quão mais vantajoso e mais barato é comprar um semi-novo/usado e comparar com custo de aluguel de carros.

### ShinyApp Construido

[Clique Aqui](https://felipelbhering.shinyapps.io/201908-cursor4ds2-entrega-final/?_ga=2.9124017.585984912.1600133237-412420558.1600133237)

Carregando Bibliotecas
----------------------

``` r
#library(readr)
library(magrittr) #uso do pipe
library(dplyr) #manipulação dados
library(ggplot2)
#library(plotly)
library(fipe)
library(DataExplorer)
```

### Carregando Dados

``` r
data_vehicules <-
  readr::read_rds("data/2020_09_fipe_top_veiculos.rds")

data_vehicules %>% head() %>% data.frame()
```

    ##                          model   make year       date price
    ## 1   ETIOS 1.3 Flex 16V 5p Mec. Toyota 2014 2020-09-01 29517
    ## 2 ETIOS X 1.3 Flex 16V 5p Mec. Toyota 0 km 2020-09-01 52710
    ## 3 ETIOS X 1.3 Flex 16V 5p Mec. Toyota 2014 2020-09-01 28978
    ## 4 ETIOS X 1.3 Flex 16V 5p Mec. Toyota 2015 2020-09-01 31007
    ## 5 ETIOS X 1.3 Flex 16V 5p Mec. Toyota 2016 2020-09-01 32560
    ## 6 ETIOS X 1.3 Flex 16V 5p Mec. Toyota 2017 2020-09-01 34372

### Filtrando modelos que tem valor de 0km e são abaixo 55mil reais

``` r
#filtrando carros que ainda produzem (tem 0km) e abaixo 55mil
modelos_abaixo_55 <- 
  data_vehicules %>% 
  filter(year=="0 km") %>% 
  filter(price < 55000) %>% 
  pull(model)

data_vehicules <- data_vehicules %>% 
                    filter((model %in% modelos_abaixo_55)) %>%
                    distinct()
```

### Descrição do dataset

#### model: O modelo do carro (Ex: Etios 1.3 Flex )

#### date: data Ano/meda avaliação na FIPE (Ex: 2020-09-01)

#### make: fabricante (Ex: Toyota)

#### price: preço em reais na coluna price (Ex: 36500)

``` r
glimpse(data_vehicules )
```

    ## Rows: 140
    ## Columns: 5
    ## $ model <chr> "ETIOS X 1.3 Flex 16V 5p Mec.", "ETIOS X 1.3 Flex 16V 5p Mec....
    ## $ make  <chr> "Toyota", "Toyota", "Toyota", "Toyota", "Toyota", "Toyota", "...
    ## $ year  <fct> 0 km, 2014, 2015, 2016, 2017, 2018, 2019, 0 km, 2015, 2016, 2...
    ## $ date  <date> 2020-09-01, 2020-09-01, 2020-09-01, 2020-09-01, 2020-09-01, ...
    ## $ price <dbl> 52710, 28978, 31007, 32560, 34372, 36353, 38913, 49547, 31013...

#### Não tem dados Missing

``` r
plot_missing(data_vehicules, title="No Missing Data")
```

![](README_files/figure-markdown_github/missing-1.png)

#### Carros mais baratos do Brasil hoje (0 km)

``` r
data_vehicules %>% filter(year=="0 km") %>% 
  arrange(price) %>% 
  select(model, make, price)
```

    ## # A tibble: 40 x 3
    ##    model                               make    price
    ##    <chr>                               <chr>   <dbl>
    ##  1 KWID Life 1.0 Flex 12V 5p Mec.      Renault 34319
    ##  2 QQ 1.0 Look FL 12V/1.0 12V Flex 5p  CHERY   34455
    ##  3 QQ 1.0  ACT FL 12V/1.0 12V Flex  5p CHERY   34745
    ##  4 MOBI EASY 1.0 Fire Flex 5p.         Fiat    35060
    ##  5 MOBI EASY COMFORT 1.0 Flex 5p.      Fiat    36832
    ##  6 MOBI LIKE 1.0 Fire Flex 5p.         Fiat    40602
    ##  7 KWID Zen 1.0 Flex 12V 5p Mec.       Renault 41323
    ##  8 MOBI WAY 1.0 Fire Flex 5p.          Fiat    41992
    ##  9 Kwid ConnecTV 1.0 Flex 12V Mec.     Renault 42289
    ## 10 530 TALENT 1.5 16V 103cv 4p         LIFAN   43245
    ## # ... with 30 more rows

#### Carros com 7 anos histórico (apenas 4)

``` r
#Carros com dados de todos anos
carros_completos <- data_vehicules %>%
  group_by(model) %>% 
  count(sort=TRUE)  

#mostrando carros com dados completos
carros_completos7 <- carros_completos %>%
    filter(n==7)
carros_completos7
```

    ## # A tibble: 4 x 2
    ## # Groups:   model [4]
    ##   model                                      n
    ##   <chr>                                  <int>
    ## 1 ETIOS X 1.3 Flex 16V 5p Mec.               7
    ## 2 Grand Siena ATTRAC. 1.4 EVO F.Flex 8V      7
    ## 3 ONIX HATCH LT 1.0 8V FlexPower 5p Mec.     7
    ## 4 ONIX HATCH LT 1.4 8V FlexPower 5p Mec.     7

#### Carros com 7 anos histórico (apenas 10)

``` r
#mostrando carros com dados de 5 anos
carros_completos5 <- carros_completos %>%
  filter(n>=5)
carros_completos5
```

    ## # A tibble: 10 x 2
    ## # Groups:   model [10]
    ##    model                                      n
    ##    <chr>                                  <int>
    ##  1 ETIOS X 1.3 Flex 16V 5p Mec.               7
    ##  2 Grand Siena ATTRAC. 1.4 EVO F.Flex 8V      7
    ##  3 ONIX HATCH LT 1.0 8V FlexPower 5p Mec.     7
    ##  4 ONIX HATCH LT 1.4 8V FlexPower 5p Mec.     7
    ##  5 Ka 1.0 SE/SE Plus TiVCT Flex 5p            6
    ##  6 Ka+ Sedan 1.0 SE/SE PLUS TiVCT Flex 4p     6
    ##  7 MARCH SV 1.0 12V Flex 5p                   6
    ##  8 QQ 1.0  ACT FL 12V/1.0 12V Flex  5p        6
    ##  9 QQ 1.0 Look FL 12V/1.0 12V Flex 5p         6
    ## 10 530 TALENT 1.5 16V 103cv 4p                5

#### Filtrando carro com pelo menos 0km +2019 (2 dados)

``` r
data_vehicules <- 
  data_vehicules %>%
  filter(model %in% (carros_completos %>% filter(n>1) %>% pull(model)))
```

``` r
#Função calcula variacao apos x anos uso
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
                      "Comprou com 1 Ano Uso" = "N_1",
                      "Comprou com 2 Anos Uso" = "N_2",
                      "Comprou com 3 Anos Uso" = "N_3",
                      "Comprou com 4 Anos Uso" = "N_4",
                      "Comprou com 5 Anos Uso" = "N_5")) %>% 
  filter(! is.na(pct_change ))
  }
```

#### Variação Preço Carro Após 1 Anos Compra (n=4 carros)

#### Carro é muito desvalorizado ao comprar 0km e ficar 1 ano com ele

#### podendo chegar a 20% a desvalorização após 1 ano

``` r
variacao_ano(data_vehicules,carros_completos7, 1) %>% 
  group_by(year) %>% 
  summarize(pct_change = median(pct_change)) %>% 
  ggplot(aes(x=year, y=pct_change)) +
  geom_bar(stat = 'identity', fill="red")+
  labs(title= "Redução do Preço do Carro após 1 Anos da Compra (Mediana)",
       y="Variação (%) do Preço do Carro após 1 anos", 
       x="Quando Comprou")+
   geom_text(aes(label=paste0(round(pct_change,0), "%")), vjust=0, hjust=1, size=3) +
   coord_flip()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Variação Preço Carro Após 1 Anos Compra

### (Carros com 5 anos histórico) (n=10)

``` r
variacao_ano(data_vehicules,carros_completos5 ,1) %>% 
  group_by(year) %>% 
  summarize(pct_change = median(pct_change)) %>% 
    filter(!year =="Comprou com 5 Anos Uso") %>% 
  filter(!year =="Comprou com 4 Anos Uso") %>% 
  ggplot(aes(x=year, y=pct_change)) +
  geom_bar(stat = 'identity', fill="red")+
  labs(title= "Redução do Preço do Carro após 1 Anos da Compra (Mediana)",
       y="Variação (%) do Preço do Carro após 1 anos", 
       x="Quando Comprou")+
   geom_text(aes(label=paste0(round(pct_change,0), "%")), vjust=0, hjust=1, size=3) +
   coord_flip()
```

    ## Warning: Unknown levels in `f`: N_3

    ## `summarise()` ungrouping output (override with `.groups` argument)

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

### Variação Preço Carro Após 2 Anos Compra

### Histórico dos 4 Carros com Histórico completo

    ## `summarise()` ungrouping output (override with `.groups` argument)

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

#### Carros tem custos, consideramos

#### 1)Custo oportunidade do dinheiro que poderia ser investido

#### Considerando Selic 2% e 15% Imposto Renda

#### 2)custo seguro em geral 0.065 até 0.08, considerando 7% na média

#### 3)ipva dpvat. Normalmente, para carros, ela é de 2% a 4% IPVA utilizei Carros movidos a gasolina e biocombustíveis 4%

#### 5)manuntencao 0.01 1o ano \#0.02 2o ano \#0.03 3o e 4o ano.

#### Após 60mil km o custo pode ser maior, por isso consideramos carros até 5 anos.

#### 6)considerando uso até 1500km/mes

#### 7)Outras possibilidades. Financiamento 20% entrada e 19.46% (media brasil ano) de juros anual.

``` r
taxa_ipva = 0.04
taxa_selic =0.02

custos_carro <- variacao_ano(data_vehicules, carros_completos, 1) %>%
   # mutate(custo_depreciacao =  custo_depreciacao) %>%
  mutate(custo_nao_alocacao_selic = price* taxa_selic*0.85) %>% 
  mutate(custo_ipva_e_dpvat = price * taxa_ipva +5.23) %>% 
  mutate(custo_seguros = price * 0.07) %>% 
    #seguros  #0.03 a 0.04 muito barato e 0.09 caro
  mutate(custo_licenciamento_emplacamento = 93.87+138.94) %>%  # SP 
  mutate(custo_revisoes_manuntencao = 
           #https://autopapo.uol.com.br/noticia/valor-de-revisao-10-mais-vendidos/
           case_when(
           year == "Comprou Novo" ~ 0.01*price,
           year == "Comprou com 1 Ano Uso" ~ 0.01*price,
           year == "Comprou com 2 Anos Uso" ~ 0.02*price,
           year == "Comprou com 3 Anos Uso" ~ 0.03*price,
           year == "Comprou com 4 Anos Uso" ~ 0.03*price,
           TRUE ~ 0) 
  ) %>% 
  mutate(custo_juros_financiamento_1ano = price*0.1946*0.8 ) %>% 
  mutate(custo_juros_financiamento_2anos = price*0.1946*0.8 *0.7) %>% 
  mutate(custo_juros_financiamento_3anos = price*0.1946*0.8*0.7*0.7 ) %>% 
  mutate(custo_juros_financiamento_4anos = price*0.1946*0.8*0.7*0.7*0.7 ) %>% 
  mutate(custo_carro_mes  = 
           (custo_depreciacao +
           custo_nao_alocacao_selic +
           custo_ipva_e_dpvat+
           custo_seguros +
           custo_revisoes_manuntencao)/12
  ) %>% 
  mutate(custo_carro_mes = 
           ifelse(year == "Comprou Novo",
                      custo_carro_mes  +
                        custo_licenciamento_emplacamento/12,
           custo_carro_mes )
          
  ) 

#filtrar 1.0 ou custo novo <50mil e fazer mediana custo_mensal
#pegar comparar com 869,949 do unidas livre/mes
custos_carro %>% select(model, year, custo_carro_mes )
```

    ## # A tibble: 100 x 3
    ## # Groups:   model [31]
    ##    model                        year                   custo_carro_mes
    ##    <chr>                        <fct>                            <dbl>
    ##  1 530 TALENT 1.5 16V 103cv 4p  Comprou Novo                      905.
    ##  2 530 TALENT 1.5 16V 103cv 4p  Comprou com 1 Ano Uso             554.
    ##  3 530 TALENT 1.5 16V 103cv 4p  Comprou com 2 Anos Uso           1207.
    ##  4 530 TALENT 1.5 16V 103cv 4p  Comprou com 4 Anos Uso            621.
    ##  5 ARGO 1.0 6V Flex.            Comprou Novo                     1352.
    ##  6 ARGO 1.0 6V Flex.            Comprou com 1 Ano Uso             556.
    ##  7 ARGO DRIVE 1.0 6V Flex       Comprou Novo                     1623.
    ##  8 ARGO DRIVE 1.0 6V Flex       Comprou com 1 Ano Uso             619.
    ##  9 ETIOS X 1.3 Flex 16V 5p Mec. Comprou Novo                     1771.
    ## 10 ETIOS X 1.3 Flex 16V 5p Mec. Comprou com 1 Ano Uso             658.
    ## # ... with 90 more rows

``` r
#%>% 
 # mutate( custo_novo_financiado =
 # ) %>% 
 # mutate(custo_usado_financiado =)

#aluguel popular faixa 
#kwid  879
#gol mpi 949
#polo mpi 1189
```

### Qual carro sofre mais desvalorização (%) no 1o ano &lt;55mil

``` r
custos_carro %>% 
    filter(year == "Comprou Novo") %>% 
    arrange(pct_change) %>% 
    select(model, make, year, price, pct_change)
```

    ## # A tibble: 31 x 5
    ## # Groups:   model [31]
    ##    model                              make          year        price pct_change
    ##    <chr>                              <chr>         <fct>       <dbl>      <dbl>
    ##  1 UNO DRIVE 1.0 Flex 6V 5p           Fiat          Comprou No~ 48143      -28.2
    ##  2 UNO WAY 1.3 Flex 8V 5p             Fiat          Comprou No~ 53871      -27.9
    ##  3 SANDERO GT line Flex 1.0 12V 5p    Renault       Comprou No~ 54418      -27.6
    ##  4 ETIOS X 1.3 Flex 16V 5p Mec.       Toyota        Comprou No~ 52710      -26.2
    ##  5 Gol 1.0 Flex 12V 5p                VW - VolksWa~ Comprou No~ 48542      -25.1
    ##  6 Ka+ Sedan 1.0 SE/SE PLUS TiVCT Fl~ Ford          Comprou No~ 53404      -24.1
    ##  7 Ka 1.0 SE/SE Plus TiVCT Flex 5p    Ford          Comprou No~ 49547      -23.1
    ##  8 ARGO DRIVE 1.0 6V Flex             Fiat          Comprou No~ 52813      -22.7
    ##  9 UNO ATTRACTIVE 1.0 Fire Flex 8V 5p Fiat          Comprou No~ 44485      -22.7
    ## 10 UNO WAY 1.0 Flex 6V 5p             Fiat          Comprou No~ 50445      -21.2
    ## # ... with 21 more rows

``` r
    head(5)
```

    ## [1] 5

### ShinyApp Construido

### Saiba o custo mensal do seu carro

[Clique Aqui](https://felipelbhering.shinyapps.io/201908-cursor4ds2-entrega-final/?_ga=2.9124017.585984912.1600133237-412420558.1600133237)

#### Resultado:: Verificamos que comprar carro novo tem um custo alto de redução após 1 ano. Certamente, você tem menos risco do carro ter problemas sendo o único e primeiro usuário, mas como negócio vale a pena comprar carro usado (nem que seja 1 ano ou menos) do que novo, sempre verificando o estado atual do carro e se foi feita as revisões.

### Ao mesmo tempo, carros de valores abaixo 40mil tem aluguel de cerca de 900 e 1000 reais, abaixo do valor de gasto de um carro novo, porém acima do valor de usados. Financeiramente, a melhor compra de carros com pelo menos um ano de usado.

#### Próximos Passos

#### 1) to do pegar qtdade carros vendidos

#### <http://www.fenabrave.org.br/portal/files/2020_08_2.pdf>
