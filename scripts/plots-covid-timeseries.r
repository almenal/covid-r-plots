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

subgroup = setNames(nm = sort(as.character(unique(df_sub$loc)), decreasing = F) )
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

#p1

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
#p2
ggsave(plot = p2, filename = '../plots/growth-rates.png',
       dpi = 300, width = 1920/300, height = 1080/300, units = 'in', scale = 1.75)

pdf(NULL) # Prevent the automatic generation of "Rplots.pdf"
#p2p = plotly::ggplotly(p2)
#htmlwidgets::saveWidget(plotly::as_widget(p2p), "../plots/growth-rate.html")

# Growth rates of last 20 days
cur_date = max(df_sub$time_base)
p2_5 = ggplot(df_sub %>% filter(time_base >= (cur_date - 30)), 
              aes(x = time_base, y = growth_con_5, col = loc)) + 
  geom_line() + geom_point(size = 1) +
  scale_x_date(breaks = "3 days") + 
  theme_grey() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  labs(x = paste0('Days after detection of ', thresh_pat, 'th case'),
       y = 'Growth rate respective to previous 5 days',
       title = 'Growth rate of cases by country (last 30 days)',
       col = 'Country')
#p2_5
ggsave(plot = p2_5, filename = '../plots/growth-rates-recent.png',
       dpi = 300, width = 1920/300, height = 1080/300, units = 'in', scale = 1.75)

pdf(NULL) # Prevent the automatic generation of "Rplots.pdf"


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

#p3

### Active cases -------------------
p4 = ggplot(df_sub, aes(x = timenorm_num, y = active, col = loc)) + 
  geom_point() + geom_line()
#p4

#F8766D

cols = c('Active' = '#E08B00', 'Recovered-cumul' = '#7CAE00', 'Died-cumul' = 'red', 'Total-cumul' = 'darkgrey')
p5 = ggplot(df_sub, aes(x = timenorm_num)) + 
  geom_col(aes(y = recov, fill = 'Recovered-cumul'), alpha = .7) + 
  geom_col(aes(y = active, fill = 'Active'), alpha = .7) + 
  geom_col(aes(y = died, fill = 'Died-cumul'), alpha = .7) + 
  scale_fill_manual(name="Legend",values=cols) + 
  scale_y_continuous(labels = scales::label_number()) +
  labs(y = "", x = paste0('Days after detection of ', thresh_pat, 'th case')) +
  theme(legend.position = 'top') +
  facet_wrap(~loc, scales = "free", ncol = 2)

ggsave('../plots/epidemic-curves.svg', plot=p5, scale = 1.65)
ggsave('../plots/epidemic-curves.png', plot=p5, width = 9, height = 16)

# New detected cases
df_sub %>%
  #filter(loc == "United Kingdom_global") %>%
  filter(loc == "Spain") %>%
  mutate('new' = c(NA, diff(conf))) %>%
  ggplot(., aes(x = timenorm_num)) + 
  geom_col(aes(y = new, fill = 'Active'), alpha = .7, col = 'black', lwd = .15) + 
  #geom_col(aes(y = died, fill = 'Died-cumul'), alpha = .7) + 
  #geom_col(aes(y = recov, fill = 'Recovered-cumul'), alpha = .7) + 
  scale_fill_manual(name="Legend",values=cols) + 
  labs(y = "", x = paste0('Days after detection of ', thresh_pat, 'th case')) +
  theme(legend.position = 'top')


# Italy epidemic curve
df_sub %>%
  #filter(loc == "Spain", time_base >= (Sys.Date() - 30)) %>%
  filter(loc == "Italy", timenorm_num >= 1) %>%
  ggplot(., aes(x = time_base)) + 
  #geom_col(aes(y = recov, fill = 'Recovered-cumul'), alpha = .7) + 
  #geom_col(aes(y = active, fill = 'Active'), alpha = .7, color = 'black', size = 0.25) +
  geom_col(aes(y = active, fill = 'Active'), alpha = .7) + 
  geom_col(aes(y = died, fill = 'Died-cumul'), alpha = .7) + 
  geom_line(aes(y = active), col = 'black', lwd = .5) +
  scale_fill_manual(name="Legend",values=cols) + 
  labs(y = "", x = paste0('Days after detection of ', thresh_pat, 'th case')) +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 45))


#
# plot for Vale
#

#pandemic_start = df_sub %>% filter(loc == 'Italy', timenorm_num == 0) %>% select(time_base) %>% unlist() %>% as.Date(origin = "1970-01-01")
cols = c('Active' = '#E08B00', 'Recovered-cumul' = '#7CAE00', 'Died-cumul' = 'red', 'Total-cumul' = 'darkgrey')
p_cumul = df_sub %>%
  filter(loc == "Italy", timenorm_num >= 1) %>%
  ggplot(., aes(x = time_base)) + 
  geom_col(aes(y = conf, fill = 'Total-cumul'), alpha = .7) + 
  geom_col(aes(y = died, fill = 'Died-cumul'), alpha = .7) + 
  geom_vline(aes(xintercept = as.Date("2020-03-09")), linetype = 2) +
  annotate(x = as.Date("2020-03-09"), hjust = 0.5, geom = 'label',
           y = df_sub[which(df_sub$loc == 'Italy' & df_sub$time_base == max(df_sub$time_base)), 'conf'],
           label = "Lockdown starts") +
  geom_vline(aes(xintercept = as.Date("2020-05-04")), linetype = 2) +
  annotate(x = as.Date("2020-05-04"), hjust = 0.5, geom = 'label',
           y = df_sub[which(df_sub$loc == 'Italy' & df_sub$time_base == max(df_sub$time_base)), 'conf'],
           label = "Lockdown ends") + 
  scale_fill_manual(name="Legend", values=cols) + 
  scale_y_continuous(labels = scales::label_number()) + 
  scale_x_date(breaks = c(as.Date(c("2020-03-09", "2020-05-04")),
                           seq(min(df_italy_new$time_base), max(df_italy_new$time_base), length.out = 6))
               ) + 
  labs(y = "", x = "") +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

cols_2 = c('New cases' = '#E08B00', 'New deaths' = 'red')
df_italy_new = df_sub %>%
  filter(loc == "Italy", timenorm_num >= 1) %>%
  mutate('new_cases' = c(NA, diff(conf)),
         'new_deaths' = c(NA, diff(died))) %>%
  select(time_base, new_cases, new_deaths)

p_new = ggplot(df_italy_new, aes(x = time_base)) + 
  geom_col(aes(y = new_cases, fill = 'New cases'), alpha = .7) + 
  geom_col(aes(y = new_deaths, fill = 'New deaths'), alpha = .7) + 
  #geom_line(aes(y = new_cases), col = 'black', lwd = .5) +
  geom_vline(aes(xintercept = as.Date("2020-03-09")), linetype = 2) +
  annotate(x = as.Date("2020-03-09"), hjust = 0.5, geom = 'label',
           y = max(df_italy_new[, 2:3], na.rm = T),
           label = "Lockdown starts") +
  geom_vline(aes(xintercept = as.Date("2020-05-04")), linetype = 2) +
  annotate(x = as.Date("2020-05-04"), hjust = 0.5, geom = 'label',
           y = max(df_italy_new[, 2:3], na.rm = T),
           label = "Lockdown ends") + 
  scale_fill_manual(name="Legend", values=cols_2) + 
  scale_x_date(breaks = c(as.Date(c("2020-03-09", "2020-05-04")),
                          seq(min(df_italy_new$time_base), max(df_italy_new$time_base), length.out = 6))
  ) + 
  scale_y_continuous(labels = scales::label_number()) + 
  labs(y = "", x = "") +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p_combi = gridExtra::arrangeGrob(grobs = list(p_cumul, p_new), ncol = 2)
#ggsave('~/Documents/covid-r-plots/italy-new-cumul-cases-deaths.png', plot = p_combi, height = 7, width = 14)
