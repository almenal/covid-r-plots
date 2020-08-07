sapply(c('dplyr', 'ggplot2', 'tidyr', 'plotly'), require, character.only = T)
# ccaa <- readr::read_csv('https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv') # Outdated
ccaa <- readr::read_csv('https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_isciii_nueva_serie.csv')
ccaa <- ccaa %>%
  arrange(ccaa, fecha) #%>%
  # mutate(nuevos = c(NA, diff(num_casos)),
  #        nuevos = ifelse(fecha == min(fecha), NA, num_casos))
  # num_casos muestra los casos nuevos

ggplot(ccaa, aes(fecha, num_casos, color = ccaa)) + geom_line()

ggplot(ccaa %>% filter(ccaa == 'Andalucía'),
       aes(fecha, num_casos, color = ccaa)) + 
  geom_line()
ggplotly()


lastdate = max(ccaa$fecha)
ccaa %>%
  filter(fecha >= (lastdate - 15)) %>%
  ggplot(., aes(fecha, num_casos, color = ccaa)) + geom_line()
