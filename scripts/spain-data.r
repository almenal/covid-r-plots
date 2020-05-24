setwd("~/Documents/covid-r-plots")
library(dplyr)
library(ggplot2)
datasource = "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"

spain = read.csv(datasource, stringsAsFactors = F)  %>%
  as_tibble() %>%
  filter(!(stringr::str_detect(CCAA, "NOTA|\\*"))) %>%
  rename(PCR = PCR.,
         TestAc = TestAc.) %>%
  mutate(FECHA = as.Date(FECHA, format = "%d/%m/%Y")) %>%
  arrange(CCAA, FECHA) %>%
  mutate(
    CCAA = factor(CCAA),
    ACTIVOS = apply(.[, c("PCR", "TestAc")], 1, function(x) {
      if (is.na(x[2])) {
        x[1]
      } else{
        x[1] - x[2]
      }
    })
  )

# Relative growth
pcr_0 <- spain$PCR[1:(nrow(spain)-1)]
pcr_1 <- spain$PCR[2:nrow(spain)]
pcr_growth <- (pcr_1 - pcr_0) / pcr_0
pcr_growth[is.infinite(pcr_growth)] <- NA
spain <- spain %>% mutate(GROWTH_PCR = c(NA, pcr_growth))

#writeRDS(spain, file = "./RDS-data/spain.RDS")
ggplot(spain, aes(x = FECHA, y = PCR, col = CCAA)) + 
  geom_line()

spain %>%
  filter(FECHA > "2020-05-01") %>%
  ggplot(., aes(x = FECHA, col = CCAA)) + 
  geom_line(aes(y = GROWTH_PCR))


# Andalucia
leyend <- c("Activos" = "orange", "Hosp" = "darkgreen", 
            "UCI" = "purple", "Fallecidos" = "darkred",
            "PCR" = "blue", "Ac" = "pink", "Gr_PCR" = "black")
spain %>%
  filter(CCAA == "AN") %>%
  ggplot(., aes(x = FECHA)) + 
  geom_line(aes(y = PCR, color = "PCR")) + 
  geom_line(aes(y = TestAc, color = "Ac")) + 
  geom_line(aes(y = Hospitalizados, color = "Hosp")) + 
  geom_line(aes(y = UCI, color = "UCI")) +
  geom_line(aes(y = Fallecidos, color = "Fallecidos")) + 
  geom_line(aes(y = ACTIVOS, color = "Activos")) +
  scale_color_manual(values = leyend)

spain %>%
  filter(CCAA == "AN", FECHA > "2020-04-01") %>%
  ggplot(., aes(x = FECHA)) + 
  geom_line(aes(y = GROWTH_PCR))
