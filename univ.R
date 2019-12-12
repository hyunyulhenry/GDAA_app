output$univ = function() {
  
  data.frame(
  'Asset' = c('Stock', 'Stock', 'Stock', 'Stock', 'Bond', 'Bond',
              'Alternative', 'Alternative', 'Alternative', 'Alternative'),
  'Specific' = c('US Stock', 'Europe Stock', 'Japan Stock', 'Emerging Stock',
                 'US Longterm Bond', 'US Int Bond', 'US REITs',
                 'Global REITs', 'Gold', 'Commodities'),
  'ETF' = c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT', 'IEF',
            'IYR', 'RWX', 'GLD', 'DBC'),
  stringsAsFactors = FALSE) %>% kable(., align = "c", esacpe = F) %>%
  kable_styling(bootstrap_options =
                  c("striped", "hover", "condensed", "responsive")) %>%
  collapse_rows(columns = 1:1, valign = "middle")
}
