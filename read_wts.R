output$wts_now = renderHighchart({
  
  wt_last = tail(wts, 1) %>% data.frame() %>%
    select_if(~any(. > 0)) %>%
    mutate_all(~list(round(., 4))) %>% gather()
  
  wt_last %>% 
    hchart(type = 'pie', hcaes(key, value)) %>% 
    hc_plotOptions(pie =list(dataLabels =
                               list(enabled = TRUE,
                                    format="{point.name}<br>{point.percentage:.2f} %"))) %>%
    hc_title(text = 'Current Portfolio Ratio Composition',
             align = 'center') %>%
    hc_xAxis(title = '') %>%
    hc_yAxis(title = '') %>%
    hc_tooltip(pointFormat = "{point.name}: {point.percentage:.2f} %") %>%
    hc_add_series(dataLabels  = list(format = '{series.name}: {point.percentage:.2f}'))
  
})


output$wts_list = renderHighchart({
  
  wts %>%
    fortify.zoo %>%
    gather(key, value, -Index) %>%
    mutate(key = factor(key, levels = unique(key))) %>%
    mutate(value = ifelse(value == 0, NA, value)) %>%
    hchart(., type = "area",
             hcaes(x = Index, y = value, group = key)) %>%
    hc_title(text = 'Historical Portfolio Weight') %>%
    hc_xAxis(title = '') %>%
    hc_yAxis(title = '') %>%
    hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b>{point.percentage:.2f}%</b><br/>",
               shared = TRUE) %>%
    hc_plotOptions(area = list(
      stacking = "percent",
      lineColor = "#ffffff",
      lineWidth = 1,
      marker = list(
        lineWidth = 1,
        lineColor = "#ffffff"
      ))) 
  
})


output$wts_table = renderDT({
  
  t1 = input$date[[1]] %>% as.character()
  t2 = input$date[[2]] %>% as.character()
  
  wts[paste0(t1, "::", t2)] %>% fortify.zoo() %>%
    mutate_at(vars(-Index), list(~(round(., 4)))) %>%
    datatable(rownames = FALSE,
              options = list(pageLength = 30, dom = 'tip'))
  
  
})
