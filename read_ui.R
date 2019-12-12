output$date = renderUI({
  dateRangeInput('date', 'Date Range',
                 start = '2007-01-01',
                 end = Sys.Date(),
                 min = '2007-01-01',
                 max = Sys.Date(),
                 format = "yyyy-mm-dd",
                 separator = " - ")
})


output$type = renderUI({
  prettyRadioButtons(
    inputId = 'type',
    label = 'Select a graph type',
    choices = list('Return' = 'a', 'Drawdown' = 'b'),
    selected = 'a',
    shape = 'round',
    inline = TRUE,
    status = 'primary',
    fill = TRUE,
    animation = 'pulse'
  )
})

output$tz = renderUI({
  prettyRadioButtons(
    inputId = "tz",
    label = HTML("Select a period</br>(applies to the bar graph)"), 
    choices = list("Year" = 'yearly',
                   "Quarter" = 'quarterly',
                   "Month" = 'monthly'),
    selected = 'yearly',
    shape = 'curve',
    inline = TRUE, 
    status = "primary",
    fill = TRUE,
    animation = 'smooth'
  )
})