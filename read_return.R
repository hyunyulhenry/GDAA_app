numeric_to_perc = function(number) {
  number %>% round(., 4) %>%
    multiply_by(., 100) %>% paste(., '%')
}

return_pr = function() {
  
  t1 = input$date[[1]] %>% as.character()
  t2 = input$date[[2]] %>% as.character()
  
  df = port_net[paste0(t1, "::", t2)]
  
  return(df)
  
}

return_period = function() {
  
  pr = input$tz
  period = paste0('apply.', pr)
  
  df_mod = return_pr() %>%
    get(period)(., Return.cumulative) %>%
    fortify.zoo() %>%
    mutate(Index = str_sub(Index, 1, 7)) 
  
  return(df_mod)
  
}

output$cum_graph = renderHighchart({
  
  req(input$type)
  
  df = return_pr()
  
  # return graph
  df_ret = df %>% 
    fortify.zoo() %>%
    mutate_at(vars(-Index), list(~cumprod(1+.) - 1)) %>%
    mutate(Returns =  multiply_by(Returns, 100))
  
  # drawdown graph
  df_dd = df %>%
    Drawdowns() %>%
    fortify.zoo() %>%
    mutate(Returns =  multiply_by(Returns, 100))
  
  # chart
  if (input$type == 'a') {
    df_graph = df_ret
  } else {
    df_graph = df_dd
  }
  
  df_graph %>%
    hchart(., type = 'area',
           hcaes(x = Index, y = Returns)) %>%
    hc_title(text = 'Graph') %>%
    hc_yAxis(title = '',
             opposite = TRUE,
             labels = list(format = '{value}%')) %>%
    hc_xAxis(title = '') %>%
    hc_tooltip(pointFormat = '{point.x:%Y-%m-%d}
               {point.y: .2f}%')
  
})

output$stat = function() {
  
  df = return_pr()
  
  list(
    'Cumulative Return' = Return.cumulative(df) %>% numeric_to_perc(),
    'Annualized Return' = Return.annualized(df) %>% numeric_to_perc(),
    'Annualized Vol' = StdDev.annualized(df) %>% numeric_to_perc(),
    'Sharpe Ratio' = SharpeRatio.annualized(df) %>% round(., 4),
    'MDD' = maxDrawdown(df) %>% numeric_to_perc()
  ) %>% do.call(rbind, .) %>%
    data.frame() %>%
    set_colnames('Value') %>%
    mutate(Index = rownames(.)) %>%
    select(Index, Value) %>%
    # mutate(Value = round(Value, 4) ) %>%
    kable(., align = "c", esacpe = F) %>%
    kable_styling(bootstrap_options =
                    c("striped", "hover", "condensed", "responsive")) %>%
    collapse_rows(columns = 1:1, valign = "middle")
}


output$period_graph = renderHighchart({
  
  req(input$tz)
  df_mod = return_period()
  
  df_mod %>%
    mutate(Returns = multiply_by(Returns, 100)) %>%
    hchart(., 
           type = "column",
           hcaes(x = Index, 
                 y = Returns 
                 )
           )%>% 
    hc_plotOptions(column =list(dataLabels =
                               list(enabled = TRUE,
                                    format="{point.y: .2f}%"))) %>%
    hc_title(text = "Returns by period") %>%
    hc_yAxis(title = '', 
             opposite = TRUE,
             labels = list(format = "{value}%")) %>%
    hc_xAxis(title = '') %>%
    hc_tooltip(pointFormat = '{point.y: .2f}%') 
  
  
})

output$period_return = renderDT({
  
  req(input$tz)
  df_mod = return_period()
  
  df_mod %>% 
    mutate(Returns = numeric_to_perc(Returns)) %>%
    datatable(rownames = FALSE,
              options = list(pageLength = 50, dom = 'tip',
                             columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  
  
})
  