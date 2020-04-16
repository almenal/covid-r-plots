setwd("~/Documents/covid-r-plots")
library(dplyr)
datasource = "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"

spain = data.table::fread(datasource, fill = T) %>% 
  mutate(CCAA = factor(CCAA), 
         FECHA = as.Date(FECHA)) %>%
  slice(1:(nrow(.)-4)) # Remove the "NOTAS" section in the end. Might want a more elegant solution to this

