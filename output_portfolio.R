tabPanel("Portfolio",
         source('output_ui.R', local = TRUE)$value,
         mainPanel(
           tabsetPanel(
             type = "tabs",
             tabPanel("Cumulative Return",
                      br(),
                      highchartOutput('cum_graph'),
                      br(),
                      tableOutput('stat'),
                      br(),
                      hr(),
                      highchartOutput('period_graph'),
                      dataTableOutput('period_return')
             ),
             tabPanel("Daily Return",
                      br(),
                      highchartOutput('daily_hist'),
                      dataTableOutput('daily_table')
             ),
             tabPanel("Weights",
                      br(),
                      highchartOutput('wts_now'),
                      br(),
                      highchartOutput('wts_list'),
                      dataTableOutput('wts_table')
             ),
             tabPanel("Raw Data",
                      br(),
                      # highchartOutput('raw_graph'),
                      dataTableOutput('raw_table'),
                      downloadButton("downloadData", "Download Data")
             )
           )
         )
)