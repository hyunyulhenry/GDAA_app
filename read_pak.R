pkg = c('shiny', 'DT', 'quadprog', 'highcharter', 'quantmod', 'PerformanceAnalytics',
        'shinythemes', 'knitr', 'kableExtra', 'magrittr', 'shinyWidgets',
        'lubridate', 'stringr', 'dplyr', 'tidyr', 'plotly', 'tibble')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
 if (length(new.pkg)) 
   install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
