
# https://www.kaggle.com/nicholasjhana/energy-consumption-generation-prices-and-weather 

# * Visualize the load and marginal supply curves.
# * What weather measurements, and cities influence most the electrical demand, prices, generation capacity?
# * Can we forecast 24 hours in advance better than the TSO?
# * Can we predict electrical price by time of day better than TSO?
# * Forecast intraday price or electrical demand hour-by-hour.
# * What is the next generation source to be activated on the load curve?

# Libraries ---------------------------------------------------------------

# install.packages('tidymodels')
# devtools::install_github('Takfes/takmeR')

pacman::p_load(tidyverse,tidymodels,skimr,modeltime,lubridate,assertthat,
               janitor,devtools,takmeR,data.table,purrr,magrittr,timetk,zoo,install = F)

# Files -------------------------------------------------------------------

weather <- fread('weather_features.csv') %>% 
  janitor::clean_names()

energy <- fread('energy_dataset.csv') %>% 
  select_if(~!is.logical(.)) %>%
  janitor::clean_names() 

# Impute missing ----------------------------------------------------------

energy %>% isna()

energy_na1 <- energy %>% 
  mutate_if(~any(is.na(.)),~na.spline(.)) %>% 
  setNames(paste0('imp_',names(.))) %>% 
  bind_cols(energy)

energy_na2 <- energy %>% 
  mutate_if(~any(is.na(.)),~na.approx(.)) %>% 
  setNames(paste0('imp_',names(.))) %>% 
  bind_cols(energy)

energy_na1 %>%
  select(time, contains('generation')) %>%
  pivot_longer(-time, names_to = 'variable') %>% 
  mutate(imputed = grepl('^imp', variable),
         variable = str_replace(variable,'^imp_','')) %>% 
  ggplot() + aes(x=time,y=value,color = imputed) + geom_line() + facet_grid(variable~.,scales = 'free')

energy_nna <- energy %>% 
  mutate_if(~any(is.na(.)),~na.spline(.))

energy_nna %>% isna() %>% sum()

# Visualizations ----------------------------------------------------------

energy_nna %>% 
  select(time,starts_with("generation")) %>%
  pivot_longer(cols = starts_with('generation_'), names_to = 'variable') %>% 
  ggplot() + aes(x=time,y=value) + geom_line() + facet_grid(variable~., scales='free')
  
energy_nna %>% 
  select(time,starts_with("generation")) %>%
  pivot_longer(cols = starts_with('generation'), names_to = 'variable') %>% 
  ggplot() + aes(x=value,fill=variable) + geom_density() + facet_grid(variable~., scales='free')

energy_nna %>% select(time,starts_with('price_')) %>% 
  pivot_longer(cols = -c(time), names_to = 'variable') %>% 
  ggplot() + aes(x=time,y=value,color = variable) + geom_line()


# Forecast Dataset --------------------------------------------------------

forecast_steps <- 24
target_variable <- 'price_actual'

time_min <- energy_nna %>% pull(time) %>% min()
time_max <- energy_nna %>% pull(time) %>% max()
time_dts <- energy_nna %>% pull(time) %>% n_distinct()

# cycles <- seq(time_min,length.out = time_dts + forecast_steps, by='hour') %>% data.frame(cycle_time = .)
# assert_that(cycles %>% nrow == time_dts + forecast_steps)

cycles <- seq(time_min,length.out = time_dts, by='hour') %>% data.frame(cycle_time = .)
assert_that(cycles %>% nrow == time_dts)

calendar <- 1:forecast_steps %>%
  map_df(~ cycles  %>% 
           mutate(
             step = .x,
             target_time = cycle_time + hours(step-1))
         ) %>% 
  arrange(cycle_time,step)
assert_that(calendar %>% nrow == cycles %>% nrow * forecast_steps)

df <- calendar %>%
  tk_augment_timeseries_signature(target_time) %>%
  select(cycle_time,step,target_time,
         half,quarter,month,day,hour,am.pm,wday,mday,qday,yday,mweek,week) %>% 
  left_join(energy_nna %>% select(time,price_actual), by = c('target_time'  = 'time')) %>% 
  left_join(energy_nna %>% select(time,starts_with('generation')), by = c('cycle_time' = 'time')) %>% 
  janitor::clean_names() %>% 
  select(cycle_time,step,target_time,target_variable,everything(),starts_with('generation'))

df %>% names()
df %>% isna()



  