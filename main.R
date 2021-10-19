
# https://www.kaggle.com/nicholasjhana/energy-consumption-generation-prices-and-weather 

# * Visualize the load and marginal supply curves.
# * What weather measurements, and cities influence most the electrical demand, prices, generation capacity?
# * Can we forecast 24 hours in advance better than the TSO?
# * Can we predict electrical price by time of day better than TSO?
# * Forecast intraday price or electrical demand hour-by-hour.
# * What is the next generation source to be activated on the load curve?

# Libraries ---------------------------------------------------------------

rm(list=ls())

# install.packages('tidymodels')
# devtools::install_github('Takfes/takmeR')

pacman::p_load(tidyverse,tidymodels,skimr,modeltime,lubridate,assertthat,
               janitor,devtools,takmeR,data.table,purrr,magrittr,timetk,zoo,install = F)

# Files -------------------------------------------------------------------

weather_raw <- fread('weather_features.csv') %>% 
  janitor::clean_names()

energy_raw <- fread('energy_dataset.csv') %>% 
  select_if(~!is.logical(.)) %>%
  janitor::clean_names() 

# Impute missing ----------------------------------------------------------

energy_raw %>% isna()

energy1 <- energy_raw %>% 
  mutate_if(~any(is.na(.)),~na.spline(.)) %>% 
  setNames(paste0('imp_',names(.))) %>% 
  bind_cols(energy_raw)

energy2 <- energy_raw %>% 
  mutate_if(~any(is.na(.)),~na.approx(.)) %>% 
  setNames(paste0('imp_',names(.))) %>% 
  bind_cols(energy_raw)

energy1 %>%
  select(time, contains('generation')) %>%
  pivot_longer(-time, names_to = 'variable') %>% 
  mutate(imputed = grepl('^imp', variable),
         variable = str_replace(variable,'^imp_','')) %>% 
  ggplot() + aes(x=time,y=value,color = imputed) + geom_line() + facet_grid(variable~.,scales = 'free')

energy <- energy_raw %>% 
  mutate_if(~any(is.na(.)),~na.spline(.))

energy %>% isna() %>% sum()

# Visualizations ----------------------------------------------------------

energy %>% 
  select(time,starts_with("generation")) %>%
  pivot_longer(cols = starts_with('generation_'), names_to = 'variable') %>% 
  ggplot() + aes(x=time,y=value) + geom_line() + facet_grid(variable~., scales='free')
  
energy %>% 
  select(time,starts_with("generation")) %>%
  pivot_longer(cols = starts_with('generation'), names_to = 'variable') %>% 
  ggplot() + aes(x=value,fill=variable) + geom_density() + facet_grid(variable~., scales='free')

energy %>% select(time,starts_with('price_')) %>% 
  pivot_longer(cols = -c(time), names_to = 'variable') %>% 
  ggplot() + aes(x=time,y=value,color = variable) + geom_line()


# Forecast Dataset --------------------------------------------------------

# settings
forecast_steps <- 24
target_variable <- 'price_actual'
time_variable <- 'time'
ma_features <- seq(0,12,3)[-1]
fourier_periods <- c(1,12,24)
fourier_orders <- 2
raw_predictor_names <- energy %>% select(starts_with('generation')) %>% names()

# determine cycles
time_min <- energy %>% pull(time) %>% min()
time_max <- energy %>% pull(time) %>% max()
time_dts <- energy %>% pull(time) %>% n_distinct()
cycles <- seq(time_min,length.out = time_dts, by='hour') %>% data.frame(cycle_time = .)
assert_that(cycles %>% nrow == time_dts)

# prepare calendar
calendar <- 1:forecast_steps %>%
  map_df(~ cycles  %>% 
           mutate(
             step = .x,
             target_time = cycle_time + hours(step-1))
  ) %>% 
  tk_augment_timeseries_signature(target_time) %>%
  select(cycle_time,step,target_time,
         half,quarter,month,day,hour,am.pm,wday,mday,qday,yday,mweek,week) %>% 
  arrange(cycle_time,step)

assert_that(calendar %>% nrow == cycles %>% nrow * forecast_steps)
calendar %>% dim()
calendar %>% names()


# prepare fourier features
fcalendar <- calendar %>% 
  select(target_time) %>% 
  distinct() %>% 
  tk_augment_fourier(target_time, .period = fourier_periods, .K = fourier_orders)

fcalendar %>% dim()
fcalendar %>% names()


# prepare predictors timetk
predictors_H <- energy %>% 
  select(time_variable,raw_predictor_names) %>%
  tk_augment_lags(.value = all_of(raw_predictor_names),
                  .lags = 1) %>% 
  tk_augment_slidify(.value   = ends_with("lag1"),
                     .period  = ma_features,
                     .f       = mean,
                     .align   = 'right', 
                     .partial = TRUE
  ) %>% 
  select(-raw_predictor_names) %>% 
  rename(cycle_time = time)

predictors_H %>% dim()
predictors_H %>% names()


# join dataframes
modeldata <- calendar %>% 
  merge(fcalendar) %>% 
  merge(predictors_H)

calendar %>% dim()
modeldata %>% dim()
modeldata %>% names()
modeldata %>% isna()
modeldata %>% clopy()


modeldata %>% write_csv('modeldata.csv')



# prepare predictors using recipes
boo <- energy %>% select(time_variable,target_variable,starts_with('generation'))

rec_obj <- recipe(data = boo, as.formula(paste(target_variable,"~."))) %>% 
  update_role('time', new_role = 'id_column') %>% 
  step_lag(contains('generation'),lag = 1) %>% 
  step_slidify_augment(starts_with('lag_1_'),period = c(3,6,9,12), align = 'right', .f='mean')
  
  # step_slidify(starts_with('lag_1_'),period = c(3,6), align = 'right', .f='mean', 
  #              names = paste0("ra3_", rec_prep$template %>% select(starts_with('lag_1_')) %>% names()), f_name='rolling_average')

rec_prep <- rec_obj %>% prep()
rec_prep %>% juice() %>% names()


# prepare predictors mutate across
energy %>% 
  select(contains('biomass')) %>% 
  mutate(across(starts_with('generation'),
                list(lag01 = lag, lag02 = ~lag(.,2))))


# prepare predictors mutate at
energy %>% 
  select(contains('biomass')) %>% 
  mutate_at(vars(starts_with('generation')),
            funs(lag_1 = lag(.), lag_2 = lag(.,2))) %>% 
  mutate_at(vars(contains('lag_1')),
            funs(ra_3 = rollmean(., k = 3, align = 'right', fill = NA)))


# # prepare dataframe
# df <- calendar %>%
#   tk_augment_timeseries_signature(target_time) %>%
#   select(cycle_time,step,target_time,
#          half,quarter,month,day,hour,am.pm,wday,mday,qday,yday,mweek,week) %>% 
#   left_join(energy_nna %>% select(time,price_actual), by = c('target_time'  = 'time')) %>% 
#   left_join(energy_nna %>% select(time,starts_with('generation')), by = c('cycle_time' = 'time')) %>% 
#   janitor::clean_names() %>% 
#   select(cycle_time,step,target_time,target_variable,everything(),starts_with('generation'))
# 
# df %>% names()
# df %>% isna()



  