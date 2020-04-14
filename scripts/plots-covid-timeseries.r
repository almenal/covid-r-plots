#!/usr/bin/R

library(dplyr)
library(ggplot2)

setwd("~/Documents/covid-r-plots/scripts")
df_sub = readRDS('../RDS-data/covid-cases-timenorm-small.RDS')
days0_sub = readRDS('../RDS-data/covid-day0-small.RDS')
metadata = file.info('../RDS-data/covid-cases-timenorm-small.RDS')
thresh_pat = 100

if(as.Date(metadata$mtime) != Sys.Date()){
  source('data-cleanup-covid-timeseries.r')
}

df_labels = df_sub %>% group_by(loc) %>% 
  summarise(conf = max(conf), maxtimediff = max(timenorm),
            growth_con_1 = max(growth_con_1, na.rm = T),
            growth_con_5 = max(growth_con_5, na.rm = T),
            latestday = max(time_base)) %>%
  mutate(day0 = days0_sub$day0)

subgroup = setNames(nm = sort(c('Spain', 'Italy', 'Germany', 'United Kingdom_global', 'Netherlands_global', 'China_global'), decreasing = F) )
maxgrowthdaynorm = sapply(subgroup, function(x){
  maxgrowth = df_sub %>% filter(loc == x) %>% select(growth_con_1) %>% max(na.rm = T)
  day = df_sub %>% filter(loc == x, growth_con_1 == maxgrowth) %>% select(timenorm_num) %>% unlist()
  return(day)
})

df_labels = df_labels %>% mutate(maxgrowthday = maxgrowthdaynorm)

### Time evolution of cases normalized by detection of kth case ------
p1 = ggplot(df_sub, aes(timenorm_num, conf, col = loc)) + 
  geom_line() + geom_point() + 
  geom_label(data = df_labels, show.legend = F,
             aes(x = maxtimediff, label = conf, hjust = 1, vjust = 0)) + 
  geom_label(data = df_labels, show.legend = F,
             aes(y = seq((0.6*max(df_sub$conf, na.rm = T)), (0.9*max(df_sub$conf, na.rm = T)), length.out = nrow(df_labels)),
                 x = 0, label = paste(loc, day0, sep = ': ')), hjust = 0) + 
  annotate(geom = 'text', x = 0, y = (0.95*max(df_sub$conf, na.rm = T)), col = 'black', hjust = 0,
           label = paste0('Day of detection of ', thresh_pat, 'th case')) +
  annotate(geom = 'label', x = 0.9*max(df_sub$timenorm_num), y = 0.2*max(df_sub$conf), 
           label = paste0('Current date: ', max(df_sub$time_base)), col = 'black') + 
  scale_x_continuous( breaks = seq(-20, max(df_sub$timenorm_num), 5), limits = c(0,max(df_sub$timenorm_num)) ) +
  #scale_y_log10() + # Comment this line to make the scale linear
  theme_grey(base_family = 'Montserrat-Medium') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        legend.position = 'right') + 
  labs(x = paste0('Days after detection of ', thresh_pat, 'th case'), 
       title = 'Number of confirmed cases by country',
       col = 'Country')

p1

ggsave(plot = p1, width = 1920/320, height = 1080/320, units = 'in', scale = 1.75,
       filename = paste0('../plots/conf_cases_after_', thresh_pat, 'th_patient.svg'))
ggsave(plot = p1, filename = paste0('../plots/conf_cases_after_', thresh_pat, 'th_patient.png'),
       dpi = 300, width = 1920/300, height = 1080/300, units = 'in', scale = 1.75)

### Growth rate evolution ----------

p2 = ggplot(df_sub, aes(timenorm_num, y = growth_con_5, col = loc)) + 
  geom_line() + geom_point() +
  scale_x_continuous( limits = c(0, max(df_sub$timenorm_num)), breaks = seq(-20, max(df_sub$timenorm_num), 5) ) +
  theme_grey() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  labs(x = paste0('Days after detection of ', thresh_pat, 'th case'),
       y = 'Growth rate respective to previous 5 days',
       title = 'Growth rate of cases by country',
       col = 'Country')
p2
ggsave(plot = p1, filename = '../plots/growth-rates.png',
       dpi = 300, width = 1920/300, height = 1080/300, units = 'in', scale = 1.75)

p2p = plotly::ggplotly(p2)
pdf(NULL) # Prevent the automatic generation of "Rplots.pdf"
#htmlwidgets::saveWidget(plotly::as_widget(p2p), "../plots/growth-rate.html")

### Detection of kth patient per country -----
days_sub = days0_sub %>% filter(loc %in% subgroup) %>% arrange(day0) %>%
  mutate(loc = factor(loc, levels = loc))

p3 = ggplot(days_sub, aes(x = loc, y = day0)) + 
  geom_label(aes(label = loc), hjust = 0.5, vjust = 0) + 
  geom_label(aes(label = day0), hjust = 0.5, vjust = 1) + 
  geom_point() +
  scale_y_date(date_breaks = '3 days') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  labs(y = 'Day of detection of 100th patient', x = 'Country') +
  coord_flip()

p3

### Active cases -------------------
p4 = ggplot(df_sub, aes(x = timenorm_num, y = active, col = loc)) + 
  geom_point() + geom_line()
p4

#F8766D
cols = c('Active' = '#E08B00', 'Recovered-cumul' = '#7CAE00', 'Died-cumul' = 'red', 'Total-cumul' = 'darkgrey')
p5 = ggplot(df_sub, aes(x = timenorm_num)) + 
  geom_col(aes(y = conf, fill = 'Total-cumul'), alpha = 1) + 
  geom_col(aes(y = active, fill = 'Active'), alpha = 1) + 
  geom_col(aes(y = recov, fill = 'Recovered-cumul'), alpha = 1) + 
  geom_col(aes(y = died, fill = 'Died-cumul'), alpha = 1) + 
  scale_fill_manual(name="Legend",values=cols) + 
  theme(legend.position = 'top')+
  facet_wrap(~loc)
p5

ggsave('../plots/epidemic-curves.svg', plot=p5, scale = 1.65)
ggsave('../plots/epidemic-curves.png', plot=p5, scale = 1.65)
