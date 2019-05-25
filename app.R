pkg = c('DT', 'quadprog', 'plotly', 'quantmod', 'PerformanceAnalytics')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
  install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)

ui = navbarPage(
  "Global Dynamic Asset Allocation",
  theme = shinythemes::shinytheme("cosmo"),

  # Main Page: Portfolio Return
  tabPanel("Portfolio",
           tabsetPanel(
             type = "tabs",

             tabPanel("Cumulative Return",
                      br(),
                      dateRangeInput('range', 'Date Range',
                                  start = '2008-01-01',
                                  end = Sys.Date(),
                                  min = '2008-01-01',
                                  max = Sys.Date(),
                                  format = "yyyy-mm-dd",
                                  separator = " - "),
                      br(),
                      plotlyOutput("port_ret"),
                      br(),
                      plotlyOutput("port_ret_yr"),
                      br(),
                      br(),
                      fluidRow(
                        column(6, DT::dataTableOutput("port_table")),
                        column(6, DT::dataTableOutput("port_table_year"))
                      )
                      
                      ),
             tabPanel("Weight",
                      br(),
                      plotlyOutput("wts_now"),
                      br(),
                      dateRangeInput('range2', 'Date Range',
                                     start = '2008-01-01',
                                     end = Sys.Date(),
                                     min = '2008-01-01',
                                     max = Sys.Date(),
                                     format = "yyyy-mm-dd",
                                     separator = " - "),
                      plotlyOutput("wts_hist"),
                      br(),
                      DT::dataTableOutput("wts_table")
                      ),
             tabPanel("Raw Data",
                      br(),
                      dateRangeInput('range3', 'Date Range',
                                     start = '2008-01-01',
                                     end = Sys.Date(),
                                     min = '2008-01-01',
                                     max = Sys.Date(),
                                     format = "yyyy-mm-dd",
                                     separator = " - "),
                      plotlyOutput("raw_ret_chart"),
                      br(),
                      DT::dataTableOutput("raw_data"),
                      br(),
                      fluidRow(
                        column(1, offset = 10,
                               downloadButton("downloadData", "Download Data")
                      )),
                      br())

             )
  ),

  # Description for strategy
  tabPanel("Description",
           tabsetPanel(
             type = "tabs",
             
             tabPanel("Strategy",
                      br(),
                      strong("Global Dynamic Asset Allocation"),
                      br(),
                      tags$ul(
                        tags$li("Strategy to perform asset allocation using momentum"),
                        tags$li("Invested in top 5 assets, which were among the top 10 global assets"),
                        tags$li("Calculate momentum indices using returns from 3 months to 12 months")
                        ),
                      br(),
                      strong("Weight of each asset"),
                      withMathJax(),
                      br(),
                      tags$ul(
                        tags$li("Variance of the portfolio is minimized"),
                        tags$li("The sum of the total weights is 1"),
                        tags$li("At least 10% and maximum 30% for each category to prevent corner solution")
                        ),
                      uiOutput('ex1'),
                      br(),
                      strong("ETC"),
                      br(),
                      tags$ul(
                        tags$li("Use a adjusted stock price that includes dividends"),
                        tags$li("Buy / sell commission 30bp"),
                        tags$li("Rebalancing by the end of the month")
                      )
                      ),

             tabPanel("Universe",
                      br(),
                      tableOutput("univ")
             )
           )),

  # Author: Henry
  tabPanel("About Henry",
           strong("Education, Certificate"),
           tags$ul(
             tags$li("Ph.D. student, Finance, Hanyang University"),
             tags$li("M.S., Financial Engineering, KAIST"),
             tags$li("B.A., Business Management, Hanyang University"),
             tags$li("CFA, FRM")
           ),
           div(),
           strong("Job Experience"),
           tags$ul(
             tags$li("2019 ~ : Meritz Insurance, Data Analysis"),
             tags$li("2016 ~ 2019 : NH-Amundi Asset Management, Quant Manager"),
             tags$li("2014 ~ 2016 : Korea Investment & Securities, Equity Manager")
           ),
           div(),
           strong("Contact Information"),
           tags$ul(
             tags$li("http://henryquant.blogspot.com"),
             tags$li("https://kr.linkedin.com/in/hyunyul-lee-34952096"),
             tags$li("https://github.com/hyunyulhenry"),
             tags$li("leebisu@gmail.com")
           )
           )

)

server = function(input, output) {

  # Download Price Data
  symbols = c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")

  withProgress(message = 'Download Data', value = 0, {
    n = length(symbols)
    for (i in 1:n) {

      getSymbols(symbols[i], src='yahoo')
      incProgress(1/n, detail = paste0(symbols[i]) )
    }
  })

  # Bind Price Data
  prices = do.call(merge, lapply(symbols, function(x) Ad(get(x))))
  rets = na.omit(Return.calculate(prices))
  names(rets) = unlist(strsplit(names(rets), ".Adjusted"))

  # Min Vol Function
  wt_minvol = function(covmat) {
    n1 = n2 = 5
    lb = rep(0.1, 5)
    ub = rep(0.3, 5)

    Amat_mv = cbind(rep(1, n1), diag(n1), -diag(n1))
    bvec_mv = c(1, lb, -ub)
    w_mv = solve.QP(covmat,c(rep(0,n1)),Amat_mv,bvec_mv,1)$solution

    return(w_mv)
  }

  # Get Weight
  lookback = 12 # Momentum Period - Former 12 Months
  fee = 0.003
  wts = list()
  ep = endpoints(rets, on = "months") # Rebalancing Frequency

  for(i in (lookback+1) : (length(ep)) ) {

    # Calculate Momentum using 3~12 Months
    ret_z = lapply(3:12, function(x) {
      scale(rank(Return.cumulative( rets[c(ep[i-x] : ep[i]) , ] )))
    })

    K = (rank(- apply(do.call(cbind, ret_z), 1, sum), ties.method = "first") <= 5 )
    subret = rets[c(ep[i-12] : ep[i]) , K]
    covs = cov(rets[c(ep[i-12] : ep[i]) , K]) # Covariance Matrix

    temp = wt_minvol(covs)

    wt = rep(0, ncol(rets))
    wt[K] = temp
    names(wt) = colnames(rets)
    wt = xts(t(wt), order.by = index(rets[ep[i]]))

    wts[[i]] = wt

  }

  wts = do.call(rbind, wts)
  
  # Present Weight
  wts_now = t(coredata(round(last(wts)[, last(wts) != 0], 4)))
  wts_now = data.frame(ticker = rownames(wts_now), wts = wts_now)

  output$wts_now = renderPlotly({
    plot_ly(x = ~wts_now$wts,
            y = ~reorder(wts_now$ticker, wts_now$wts),
            type = 'bar', orientation = 'h') %>%
      layout(title = 'Current Portfolio Ratio Composition',
             xaxis = list(title = "",
                          tickformat = "%"),
             yaxis = list(title = "")) %>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = wts_now$wts + 0.01, y = wts_now$ticker,
                      text = paste(round(wts_now$wts * 100, 2), '%'),
                      font = list(family = 'Arial', size = 12),
                      showarrow = FALSE)
  })
  
  # Historical Weight
  output$wts_hist = renderPlotly({
    plot_ly(data = data.frame(wts[paste0(input$`range2`[1],"::",input$`range2`[2])]) %>%
              cbind('Date' = rownames(.), .),
            x = ~Date, y = ~SPY, name = 'SPY', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~IEV, name = 'IEV', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~EWJ, name = 'EWJ', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~EEM, name = 'EEM', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~TLT, name = 'TLT', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~IEF, name = 'IEF', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~IYR, name = 'IYR', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~RWX, name = 'RWX', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~GLD, name = 'GLD', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      add_trace(y = ~DBC, name = 'DBC', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
      layout(title = 'Historical Portfolio Weight',
             xaxis = list(title = "",
                          type = 'date',
                          tickformat = '%y-%m'),
             yaxis = list(title = "",
                          tickformat = '%'))
  })
  
  # Weight Table
  output$wts_table = DT::renderDataTable({
    data.frame(round(wts[paste0(input$`range2`[1],"::",input$`range2`[2])], 4)) %>%
      cbind('Date' = rownames(.), .) %>%
      `rownames<-` (NULL)
    
  })
  
  # Raw Return Chart
  output$raw_ret_chart = renderPlotly({
    plot_ly(data = data.frame(cumprod(1 + rets[paste0(input$`range3`[1],"::",input$`range3`[2])])-1) %>%
              cbind('Date' = rownames(.), .),
            x = ~Date, y = ~SPY, name = 'SPY', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IEV, name = 'IEV', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~EWJ, name = 'EWJ', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~EEM, name = 'EEM', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~TLT, name = 'TLT', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IEF, name = 'IEF', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IYR, name = 'IYR', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~RWX, name = 'RWX', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~GLD, name = 'GLD', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~DBC, name = 'DBC', type = 'scatter', mode = 'line') %>%
      layout(title = 'ETF Raw Return',
             xaxis = list(title = "",
                          type = 'date',
                          tickformat = '%y-%m'),
             yaxis = list(title = "",
                          tickformat = '%'))
  })
  
  
  # Raw Return Data Table
  output$raw_data = DT::renderDataTable({
    data.frame(round(rets,4)) %>%
      cbind('Date' = rownames(.), .) %>%
      `rownames<-` (NULL)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("price_data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(round(rets,4)), file)
    }
  )
  
  # Portfolio Return
  port_gross = Return.portfolio(rets, wts, verbose = TRUE)
  port_turnover = xts(rowSums(abs(port_gross$BOP.Weight - lag(port_gross$EOP.Weight)), na.rm = TRUE),
                      order.by = index(port_gross$BOP.Weight))
  port_net = port_gross$returns - (port_turnover * fee)
  names(port_net) = 'Returns'
  port_net_yr = round(apply.yearly(port_net, Return.cumulative), 5)
  port_net_yr = data.frame(port_net_yr) %>%
    cbind('Year' = rownames(.), .)
  port_net_yr$Year = substring(port_net_yr$Year, 1, 4)
  rownames(port_net_yr) = NULL
  
  # Portfolio Return Graph
  output$port_ret = renderPlotly({
    plot_ly(data = data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
              cbind('Date' = rownames(.), .),
            x = ~ Date,
            y = ~ Returns,
            type = 'scatter', mode = 'lines') %>%
      layout(title = 'Portfolio Cumulative Return',
             xaxis = list(title = "",
                          type = 'date',
                          tickformat = '%y-%m'),
             yaxis = list(title = "")) 
    })
  
  # Portfolio Yearly Graph
  output$port_ret_yr = renderPlotly({
    plot_ly(data = port_net_yr,
            x = ~ Year,
            y = ~ Returns,
            type = "bar") %>%
      layout(title = 'Yearly Return',
             xaxis = list(title = ""),
             yaxis = list(title = "",
                          tickformat = '%')) 
  })
  
  # Portfolio Sub period
  output$port_table = DT::renderDataTable({
    data.frame(round(port_net[paste0(input$`range`[1],"::",input$`range`[2])], 5)) %>%
      cbind('Date' = rownames(.), .) %>%
      `rownames<-` (NULL)
  })
  
  # Portfolio Yearly Return
  output$port_table_year = DT::renderDataTable({
    port_net_yr
  })
  
  
  # Min vol syntex
  output$ex1 <- renderUI({
    withMathJax(helpText('$$min\\ \\sigma_p $$'),
                helpText(('$$s.t. \\sum w_i = 1, 0.1 ≤ w_i ≤ 0.3$$'))
                         )
  })

  # Universe Data Frame
  output$univ <- renderTable({
    data.frame(
      'Asset' = c('Stock', 'Stock', 'Stock', 'Stock', 'Bond', 'Bond',
                  'Alternative', 'Alternative', 'Alternative', 'Alternative'),
      'Specific' = c('US Stock', 'Europe Stock', 'Japan Stock', 'Emerging Stock',
                     'US Longterm Bond', 'US Int Bond', 'US REITs', 'Global REITs',
                     'Gold', 'Commodities'),
      'ETF' = c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT', 'IEF', 'IYR', 'RWX', 'GLD', 'DBC'),
      stringsAsFactors = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)