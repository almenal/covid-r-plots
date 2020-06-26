ccaa <- readr::read_csv('https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_isciii_nueva_serie.csv')
ccaa <- ccaa %>%
  arrange(ccaa, fecha) %>%
  mutate(nuevos = c(NA, diff(num_casos)),
         nuevos = ifelse(fecha == min(fecha), NA, num_casos))


ggplot(ccaa, aes(fecha, num_casos, color = ccaa)) + geom_line()
ggplot(ccaa, aes(fecha, nuevos, color = ccaa)) + geom_line()

ccaa %>%
  filter(fecha >= (Sys.Date() - 7)) %>%
  ggplot(., aes(fecha, num_casos, color = ccaa)) + geom_line()
