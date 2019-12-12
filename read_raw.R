return_raw = function() {
  
  t1 = input$date[[1]] %>% as.character()
  t2 = input$date[[2]] %>% as.character()
  
  df_raw = rets[paste0(t1, "::", t2)]
  
  return(df_raw)
  
}

output$raw_graph = renderHighchart({
 
  df_raw = return_raw()
  
  df_raw = df_raw %>%
    fortify.zoo() %>%
    mutate_at(vars(-Index), list(~cumprod(1+.) - 1)) %>%
    gather(key, value, -Index) %>%
    mutate(key = factor(key, levels = unique(key)))
  
  df_raw %>%
    hchart(., type = 'line',
           hcaes(x = Index, y = value, group = key)) %>%
    hc_title(text = 'ETF Raw Return') %>%
    hc_yAxis(title = '',
             opposite = TRUE,
             labels = list(format = '{value}')) %>%
    hc_xAxis(title = '') %>%
    hc_tooltip(pointFormat = '{point.x:%Y-%m-%d}
               {point.y:.4f}')
  
  
})

output$raw_table = renderDT({

  req(input$date)

  df_raw = return_raw()

  df_raw %>% fortify.zoo() %>%
    mutate_at(vars(-Index), list(~(round(., 4)))) %>%
    datatable(rownames = FALSE,
              options = list(pageLength = 50, dom = 'tip'))


})

output$downloadData = downloadHandler(
  filename = function() {
    paste("price_data", ".csv", sep="")
  },
  content = function(file) {
    write.csv(data.frame(round(rets,4)), file)
  }
)