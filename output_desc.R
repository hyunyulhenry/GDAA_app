# Description for strategy
tabPanel("Description",
         tabsetPanel(
           type = "tabs",
           tabPanel("Strategy",
                    br(), includeMarkdown('stra.Rmd')
           ),
           tabPanel("Universe",
                    br(), tableOutput('univ')
           )
         ))