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
    mutate(Returns = round(Returns, 4)) %>%
    datatable(rownames = FALSE,
              options = list(pageLength = 50, dom = 'tip'))
  
  
})