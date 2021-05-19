# covid-r-plots

Hi! You will find a couple of R scripts that I've been writing about the SARS-CoV-2 epidemic that originated in China in late 2019 (more notoriously known as *Coronavirus*).

I got the raw data from the [John's Hopkins University's repository](https://github.com/CSSEGISandData/COVID-19) and worked with it using `R`.

For now, this repo consists in one script reshaping the data as a ggplot-friendly dataframe and exporting it to .csv and another one to plot that same data.

> It ain't much, but it's honest work.

## Plots

### Evolution of cumulative number of cases
![](https://github.com/almenal/covid-r-plots/blob/master/plots/conf_cases_after_100th_patient.png)

### Growth rates
![](https://github.com/almenal/covid-r-plots/blob/master/plots/growth-rates.png)

### Growth rates of last month
![](https://github.com/almenal/covid-r-plots/blob/master/plots/growth-rates-recent.png)

### Epidemic curves of some countries

> Note that countries like the UK or the USA do not report active cases, only the cumulative ones, so the graphs are not directly comparable with the ones from the other countries.

![](https://github.com/almenal/covid-r-plots/blob/master/plots/epidemic-curves.png)

