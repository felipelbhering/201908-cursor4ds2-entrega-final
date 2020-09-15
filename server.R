
server <- function(input, output) {
  
  #input$model
  taxa_ipva = 0.04
  #taxa_selic = input$taxa_selic
  #taxa_selic =eventReactive(as.numeric(input$taxa_selic))
  #https://stackoverflow.com/questions/44615406/error-in-as-data-frame-default-cannot-coerce-class-creactiveexpr-reactive
  
  
  custos_carro <- reactive({
    taxa_selic = input$taxa_selic1 %>% as.numeric()
    data =variacao_ano(data_vehicules, carros_completos, 1) %>%
      # filter(model ==input$model) %>% 
      # mutate(custo_depreciacao =  custo_depreciacao) %>%
      mutate(custo_nao_alocacao_selic = price* taxa_selic *0.85) %>% 
      mutate(custo_ipva_e_dpvat = price * taxa_ipva +5.23) %>% 
      mutate(custo_seguros = price * 0.07) %>% 
      #seguros  #0.03 a 0.04 muito barato e 0.09 caro
      mutate(custo_licenciamento_emplacamento = 93.87+138.94) %>%  # SP 
      mutate(custo_revisoes_manuntencao = 
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
                  custo_revisoes_manuntencao)/12 %>% round(0)
      ) %>% 
      mutate(custo_carro_mes = 
               ifelse(year == "Comprou Novo",
                      custo_carro_mes  +
                        custo_licenciamento_emplacamento/12,
                      custo_carro_mes ) %>% round(0) 
             
      ) %>% select( marca=make,model, year,
                    custo_carro_mes,  valor_carro = price ) %>% 
      arrange(custo_carro_mes ) %>% 
      mutate(custo_carro_mes = custo_carro_mes  %>% format_real(),
             valor_carro = valor_carro %>% format_real())
    return(data)
  })
  
  custos_carro_filtro <- reactive({
    taxa_selic = input$taxa_selic2 %>% as.numeric()
    model_filtro = input$model %>% as.character()
    custos_seguro= input$custos_seguro %>% as.numeric()
    data =variacao_ano(data_vehicules, carros_completos, 1) %>%
      # filter(model ==input$model) %>% 
      # mutate(custo_depreciacao =  custo_depreciacao) %>%
      mutate(custo_nao_alocacao_selic = price* taxa_selic *0.85) %>% 
      mutate(custo_ipva_e_dpvat = price * taxa_ipva +5.23) %>% 
      mutate(custo_seguros = custos_seguro) %>% 
      #seguros  #0.03 a 0.04 muito barato e 0.09 caro
      mutate(custo_licenciamento_emplacamento = 93.87+138.94) %>%  # SP 
      mutate(custo_revisoes_manuntencao = 
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
                  custo_revisoes_manuntencao)/12 %>% round(0)
      ) %>% 
      mutate(custo_carro_mes = 
               ifelse(year == "Comprou Novo",
                      custo_carro_mes  +
                        custo_licenciamento_emplacamento/12,
                      custo_carro_mes ) %>% round(0) 
             
      ) %>% select( marca=make,model, year,
                    custo_carro_mes,  valor_carro = price ) %>% 
      arrange(custo_carro_mes ) %>% 
      mutate(custo_carro_mes = custo_carro_mes  %>% format_real(),
             valor_carro = valor_carro %>% format_real()) %>% 
      filter(model ==  model_filtro)
    return(data)
  })
  
  
  
  output$plot <- renderPlot(
    variacao_ano(data_vehicules, carros_completos5, 1) %>% 
      group_by(year) %>% 
      summarize(pct_change = median(pct_change)) %>% 
      ggplot(aes(x=year, y=pct_change)) +
      geom_bar(stat = 'identity', fill="red")+
      labs(title= "Redução do Preço do Carro após 1 Anos da Compra (Mediana)",
           y="Variação (%) do Preço do Carro após 1 anos", 
           x="Quando Comprou")+
      geom_text(aes(label=paste0(round(pct_change,0), "%")), vjust=0, hjust=1, size=3) +
      coord_flip()
  )
  
  
  #filtrar 1.0 ou custo novo <50mil e fazer mediana custo_mensal
  #pegar comparar com 869,949 do unidas livre/mes
  
  output$table <-   DT::renderDataTable(
    #  output$table <-  renderDataTable({
    custos_carro() , rownames = FALSE,
    
    # column filter on the top
    filter = 'top',# server = TRUE,
    
    # autoWidth
    options = list(  pageLength = 5)#,
    # options = list(pageLength = 5)#reactive nao é valor sim funcao
  )
  output$table2 <-  DT::renderDataTable(
    #  output$table <-  renderDataTable({
    custos_carro_filtro(), rownames = FALSE,
    
    # column filter on the top
    #filter = 'top', server = TRUE,
    
    # autoWidth
    options = list(autoWidth = TRUE,  pageLength = 5)
    #%>% head(5)#reactive nao é valor sim funcao
  )
  
  
}
