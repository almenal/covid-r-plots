library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

setwd('~/Documents/COVID-19')
data_conf = read.csv('./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
data_died = read.csv('./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
data_recov = read.csv('./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')
pop = read.csv('population-by-broad-age-group.csv') #Population data per country

# I realized that there is an inconsistency in the naming of Diamond Princess:
data_recov = data_recov %>% 
  mutate(Country.Region = replace(as.character(Country.Region), 
                                  Country.Region == 'Cruise Ship', 'Diamond Princess'))

#Temporal data in R is just messy
time = colnames(data_conf[5:ncol(data_conf)])
time_char = str_replace(time, "X", "") %>% str_replace_all("\\.", "/")
time = chron::as.dates(time_char) %>% as.Date()

"
countries_conf = as.character(unique(data_conf$Country.Region))
countries_recov = as.character(unique(data_recov$Country.Region))
countries_died = as.character(unique(data_died$Country.Region))
identical(countries_died, countries_conf) # Out: TRUE

#all_countries = unique(c(countries_conf, countries_recov))
#names(all_countries) = all_countries
all_countries = setNames(nm = unique(c(countries_conf, countries_recov)))

"

#### Filling the gaps ----------------------
# Adding NAs if lengths of columns do not match (gives problems later)
if(ncol(data_conf) > ncol(data_died)){
  diff_dates = ncol(data_conf) - ncol(data_died)
  fill = matrix(NA, nrow = nrow(data_died), ncol = diff_dates)
  colnames(fill) = colnames(data_conf[(ncol(data_conf) - diff_dates + 1):ncol(data_conf)])
  data_died = cbind(data_died,fill)
} else if(ncol(data_conf) < ncol(data_died)){
  diff_dates = ncol(data_died) - ncol(data_conf)
  fill = matrix(NA, nrow = nrow(data_died), ncol = diff_dates)
  colnames(fill) = colnames(data_died[(ncol(data_died) - diff_dates + 1):ncol(data_died)])
  data_conf = cbind(data_conf,fill)
}

if(ncol(data_conf) > ncol(data_recov)){
  diff_dates = ncol(data_conf) - ncol(data_recov)
  fill = matrix(NA, nrow = nrow(data_recov), ncol = diff_dates)
  colnames(fill) = colnames(data_conf[(ncol(data_conf) - diff_dates + 1):ncol(data_conf)])
  data_recov = cbind(data_recov,fill)
} else if(ncol(data_conf) < ncol(data_recov)){
  diff_dates = ncol(data_recov) - ncol(data_conf)
  fill = matrix(NA, nrow = nrow(data_conf), ncol = diff_dates)
  colnames(fill) = colnames(data_recov[(ncol(data_recov) - diff_dates + 1):ncol(data_recov)])
  data_conf = cbind(data_conf,fill)
}

### Keeping track of the countries where data is inconsistent -----

# National
countries_conf = as.character(unique(data_conf$Country.Region))
countries_recov = as.character(unique(data_recov$Country.Region))
countries_died = as.character(unique(data_died$Country.Region))
identical(countries_died, countries_conf) # Out: TRUE
all_countries = setNames(nm = unique(c(countries_conf, countries_recov)))

# Regional
regions_conf = as.character(unique(data_conf$Province.State))
regions_recov = as.character(unique(data_recov$Province.State))
regions_died = as.character(unique(data_died$Province.State))
all_regions = setNames(nm = unique(c(regions_conf, regions_recov)))


### Filling the gaps in the rows --------
"
## Filling the missing countries in confirmed & died
if(length(countries_conf) < length(countries_recov)){
  # National
  countries_recov_only = setNames(nm = countries_recov[which(!(countries_recov %in% countries_conf))])
  countries_conf_fill = lapply(countries_recov_only, function(x){
    # Regional
    if(data_recov %>% filter(Country.region == x) %>% nrow() > 1){
      regs = data_recov %>% filter(Country.Region == x) %>% 
        select(Province.State) %>% unlist() %>% as.character()
      names(regs) = regs
      
      #Iterate over regions to get regional data
      regdat = lapply(regs, function(y){
        regfill = c(y, x, rep(NA, ncol(data_recov)-2) )
        }) %>% bind_rows()
      return(regdat)
    }else{
      fill = c(x, x, rep(NA, ncol(data_recov)-2) )
      return(fill)
    }
  }) %>% bind_rows() %>% t() %>% as.data.frame()
  colnames(countries_conf_fill) = colnames(data_conf)
  data_conf = data_conf %>% bind_rows(countries_conf_fill)
  data_died = data_died %>% bind_rows(countries_conf_fill)
  
} else if (length(countries_conf) > length(countries_recov)){
  # Filling the missing countries in recovered
  countries_conf_only = setNames(nm = countries_conf[which(!(countries_conf %in% countries_recov))])
  countries_recov_fill = lapply(countries_conf_only, function(x){
    # Regional
    if(data_conf %>% filter(Country.region == x) %>% nrow() > 1){
      regs = data_conf %>% filter(Country.Region == x) %>% 
        select(Province.State) %>% unlist() %>% as.character()
      names(regs) = regs
      
      #Iterate over regions to get regional data
      regdat = lapply(regs, function(y){
        regfill = c(y, x, rep(NA, ncol(data_conf)-2) )
      }) %>% bind_rows()
      return(regdat)
    }else{
      fill = c(x, x, rep(NA, ncol(data_conf)-2) )
      return(fill)
    }
  }) %>% bind_rows() %>% t() %>% as.data.frame()
  
  colnames(countries_recov_fill) = colnames(data_recov)
  data_recov = data_recov %>% bind_rows(countries_recov_fill)
} else if( (length(countries_conf) == length(countries_recov)) & 
           length(regions_conf) > length(regions_recov) ){
  regions_conf_only = setNames(nm = regions_conf[which(!(regions_conf %in% regions_recov))])
  incomplete_countries = setNames(nm = data_conf %>% filter(Province.State %in% regions_conf_only) %>%
                              select(Country.Region) %>% unlist() %>% as.character() %>% unique()  )
  region_recov_fill = sapply(incomplete_countries, function(x){
    
    #
    #
    #   CONTINUE HERE!!!!!!!!
    #
    #
    
  })
  
} else if( (length(countries_conf) == length(countries_recov)) & 
          length(regions_conf) < length(regions_recov) ){}

"

#### Merging all data into single dataframe  -------------

dfl = lapply(all_countries, function(x){
  #If the country has more than 1 region, get the data for each region
  if(data_conf %>% filter(Country.Region == x) %>% nrow() > 1){
    
    #Get the regions of that country into a character vector to iterate over
    regs = data_conf %>% filter(Country.Region == x) %>% 
      select(Province.State) %>% unlist() %>% as.character()
    names(regs) = regs
    
    #Iterate over regions to get regional data
    regdat = lapply(regs, function(y){
      con = data_conf %>% filter(Country.Region == x, Province.State == y) %>% #Confirmed
         select(5:ncol(data_conf)) %>% unlist() %>% as.numeric()
      #Growth rate
      growth_con_1 = sapply(2:length(con), function(y){ #We start from 2 to avoid conversion to list
        diff = con[y]-con[y-1]
        rate = diff/con[y-1]
        return(rate)
      })
      growth_con_5 = sapply(6:length(con), function(y){ #We start from 2 to avoid conversion to list
        oldest = con[y-5]
        recent = con[y]
        rate = ((recent/oldest)**(1/5))
        return(rate)
      })
      ded = data_died %>% filter(Country.Region == x, Province.State == y) %>% #Deaths
        select(5:ncol(data_died)) %>% unlist() %>% as.numeric()
      rec = data_recov %>% filter(Country.Region == x, Province.State == y) %>% #Recovered
        select(5:ncol(data_recov)) %>% unlist() %>% as.numeric()
      if(length(rec) == 0){rec = rep(NA, length(ded))}
      
      reg = data.frame('loc' = paste(x,y, sep = '_'), 'time_base' = time,
                       'conf' = con, 'died' = ded, 'recov' = rec,
                       'growth_con_1' = c(NaN, growth_con_1), 'growth_con_5' = c(rep(NA,5),growth_con_5) )
      return(reg)
    }) %>% bind_rows() #Merge regional data_conf into one data_confframe
    
    #Sum all regional data_conf to get the entire country's data
    #Confirmed
    glob_con = data_conf %>% filter(Country.Region == x) %>% select(5:ncol(data_conf)) %>% as.matrix()
    glob_con = apply(glob_con, 2, sum, na.rm = T)
    glob_growth_con_1 = sapply(2:length(glob_con), function(y){ #We start from 2 to avoid conversion to list
      diff = glob_con[y]-glob_con[y-1]
      rate = diff/glob_con[y-1]
      return(rate)
    })
    glob_growth_con_5 = sapply(6:length(glob_con), function(y){ #We start from 6 to avoid conversion to list
      oldest = glob_con[y-5]
      recent = glob_con[y]
      rate = ((recent/oldest)**(1/5))
      return(rate)
    })
    #Deaths
    glob_ded = data_died %>% filter(Country.Region == x) %>% select(5:ncol(data_conf)) %>% as.matrix()
    glob_ded = apply(glob_ded, 2, sum, na.rm = T)
    #Recovered
    glob_rec = data_recov %>% filter(Country.Region == x) %>% select(5:ncol(data_conf)) %>% as.matrix()
    if(is.null(glob_rec)){
      glob_rec = rep(NA, length(glob_ded))
    }else if(!(is.null(glob_rec)) & nrow(glob_rec) > 1) {
      glob_rec = apply(glob_rec, 2, as.numeric)
      glob_rec = apply(glob_rec, 2, sum, na.rm = T)
    } else if(!(is.null(glob_rec)) & nrow(glob_rec) == 1){
      glob_rec = apply(glob_rec, 2, as.numeric)
    }
    
    globb = data.frame('loc' = paste(x, 'global', sep = '_'), 'time_base' = time,
                       'conf' = glob_con, 'died' = glob_ded, 'recov' = glob_rec,
                       'growth_con_1' = c(NaN, glob_growth_con_1), 'growth_con_5' = c(rep(NA,5), glob_growth_con_5) )
    
    #Merge regional and national data into one dataframe 
    datt = bind_rows(regdat, globb)
    return(datt)
    
  }else{ 
    
    #In case the country provides only nation-wide data, get that data
    con = data_conf %>% filter(Country.Region == x) %>% select(5:ncol(data_conf)) %>% unlist() %>% as.numeric()
    # Add growth rate
    growth_con_1 = sapply(2:length(con), function(y){ #We start from 2 to avoid conversion to list
      diff = con[y]-con[y-1]
      rate = diff/con[y-1]
      return(rate)
    })
    growth_con_5 = sapply(6:length(con), function(y){ #We start from 2 to avoid conversion to list
      oldest = con[y-5]
      recent = con[y]
      rate = ((recent/oldest)**(1/5))
      return(rate)
    })
    ded = data_died %>% filter(Country.Region == x) %>% select(5:ncol(data_died)) %>% unlist() %>% as.numeric()
    rec = data_recov %>% filter(Country.Region == x) %>% select(5:ncol(data_recov)) %>% unlist() %>% as.numeric()
    if(length(rec) == 0){rec = rep(NA, length(ded))}
    
    datt = data.frame('loc' = x, 'time_base' = time, 'conf' = con, 'died' = ded, 'recov' = rec,
                      'growth_con_1' = c(NaN, growth_con_1), 'growth_con_5' = c(rep(NA,5), growth_con_5) )
    return(datt)
  }
})

### Normalizing by time: Days after detection of kth patient -------
thresh_pat = 100
df_norm = lapply(dfl, function(x){
  
  #If the country's data is divided into regions, normalize regional data
  if(length(unique(x$loc)) > 1){
    regions = unique(x$loc) ; names(regions) = regions
    days_reg = lapply(regions, function(y){
      
      days = x %>% filter(loc == y) %>% select(time_base)
      days = as.Date(days[,1]) #dplyr keeps objects as tibbles, so we have to coerce it into Date
      day0 = x %>% filter(loc == y, conf >= thresh_pat) %>% select(time_base) %>% slice(1)
      day0 = day0[1,1]
      time_since_kth_case = days - day0
      
      norm = x %>% filter(loc == y) %>%
        mutate(timenorm = time_since_kth_case,
               timenorm_num = as.numeric(timenorm))
      return(norm)
    }) %>% bind_rows() #Merge regional and global data together
    return(days_reg)
    
  }else{
    days = x %>% select(time_base)
    days = as.Date(days[,1]) #dplyr keeps objects as tibbles, so we have to coerce it into Date
    day0 = x %>% filter(conf >= thresh_pat) %>% select(time_base) %>% slice(1)
    day0 = day0[1,1]
    time_since_kth_case = days - day0
    
    norm = x %>%
      mutate(timenorm = time_since_kth_case,
             timenorm_num = as.numeric(timenorm))
    return(norm)
  }
}) %>% bind_rows() %>%
mutate(growth_con_1 = replace(growth_con_1, growth_con_1 == Inf, NaN),
       growth_con_5 = replace(growth_con_5, growth_con_5 == Inf, NaN),
       active = conf - died - recov)


saveRDS(df_norm, file = 'covid-cases-timenorm.RDS') # Save the data in R format
write.csv(df_norm, file = 'covid-cases-timenorm.csv') # Save it as csv

#### List of each country and its day 0 -------------

days0 = lapply(dfl, function(x){
  #If the country's data is divided into regions, normalize regional data
  if(length(unique(x$loc)) > 1){ 
    
    regions = unique(x$loc) ; names(regions) = regions
    days_reg = lapply(regions, function(y){
      
      day0_reg = x %>% filter(loc == y, conf >= thresh_pat) %>% select(time_base) %>% slice(1)
      day0_reg = day0_reg[1,1]
      k_reg = data.frame('loc' = y, 'day0' = day0_reg)
      return(k_reg)
    })
    
    #Merge regional data into one data frame
    #country_global is also within the loc column, so the global data is also computed
    days_reg = days_reg %>% bind_rows()
    return(days_reg)
    
  }else{
    day0 = x %>% filter(conf >= thresh_pat) %>% select(time_base) %>% slice(1)
    day0 = day0[1,1]
    loc = as.character(x$loc)[1]
    k = data.frame('loc' = loc, 'day0' = day0)
    return(k)
  }
}) %>% bind_rows()

### Smaller dataframe with """"""relevant"""""" countries -------
subgroup = setNames(nm = sort(c('Spain', 'Italy', 'Germany', 'United Kingdom_global', 'Netherlands_global', 'China_global'), decreasing = F) )
df_sub = df_norm %>% filter(loc %in% subgroup)
days0_sub = days0 %>% filter(loc %in% subgroup)

saveRDS(days0, file = 'covid-day0.RDS')
saveRDS(days0_sub, file = 'covid-day0-small.RDS')
saveRDS(df_sub, file = 'covid-cases-timenorm-small.RDS')
write.csv(df_sub, file = 'covid-cases-timenorm-small.csv')