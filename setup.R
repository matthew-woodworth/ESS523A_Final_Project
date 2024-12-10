#Script for installing (if needed) and loading packages 
#for this project

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

packages <- c('tidyverse',
              'palmerpenguins',
              'rmarkdown',
              'plotly',
              'lubridate',
              'rstatix',
              'ggthemes',
              'lterdatasampler',
              'dataRetrieval',
              'httr',
              'jsonlite',
              'purrr',
              'jsonlite',
              'httr',
              'plotly',
              'scales',
              'shiny')

packageLoad(packages)
