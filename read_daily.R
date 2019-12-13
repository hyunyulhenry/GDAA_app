output$daily_hist = renderHighchart({
  
  req(input$type)
  
  df = return_pr()
  
  hist(df, plot = FALSE, breaks = nrow(df) / 10) %>%
    hchart(., showInLegend = F) %>%
    hc_title(text = 'Return Distribution') %>%
    hc_xAxis(title = '')
  
})

output$daily_table = renderDT({
  
  req(input$type)
  
  df = return_pr()
  
  df %>% fortify.zoo() %>% 
    mutate(Returns = numeric_to_perc(Returns)) %>%
    datatable(rownames = FALSE,
              options = list(pageLength = 50, dom = 'tip',
                             columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  
  
})