source('read_pak.R', local = TRUE)

ui = navbarPage(
 
  "Global Dynamic Asset Allocation",
  theme = shinythemes::shinytheme("cosmo"),
  
  source('output_portfolio.R', local = TRUE)$value,
  source('output_desc.R', local = TRUE)$value,
  source('output_author.R', local = TRUE)$value
  
)


server = function(input, output) {
  
  source('univ.R', local = TRUE)
  source('read_ui.R', local = TRUE)
  source('read_data.R', local = TRUE)
  source('read_return.R', local = TRUE)
  source('read_daily.R', local = TRUE)
  source('read_wts.R', local = TRUE)
  source('read_raw.R', local = TRUE)
  
}

shinyApp(ui = ui, server = server)
