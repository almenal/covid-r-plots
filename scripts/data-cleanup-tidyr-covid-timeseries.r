#!/usr/bin/R
setwd("~/Documents/covid-r-plots/scripts")
sapply(c('dplyr', 'ggplot2', 'tidyr', 'stringr'), require, character.only = T)

data_conf = read.csv('../rawdata/time_series_covid19_confirmed_global.csv')
data_died = read.csv('../rawdata/time_series_covid19_deaths_global.csv')
data_recov = read.csv('../rawdata/time_series_covid19_recovered_global.csv')

# Merge regional data into nation-wide
locations_conf <- paste(data_conf$Province.State, data_conf$Country.Region, sep = '-') %>%
  str_replace(., '^-', '')
locations_recov <- paste(data_recov$Province.State, data_recov$Country.Region, sep = '-') %>%
  str_replace(., '^-', '')
missing_locs <- locations_conf[!(locations_conf %in% locations_recov)]