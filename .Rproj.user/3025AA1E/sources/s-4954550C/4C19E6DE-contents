
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

pacman::p_load(tidyverse,tidymodels,skimr,modeltime,janitor,devtools,takmeR,data.table,purrr,magrittr,timetk,zoo,install = F)

# Files -------------------------------------------------------------------

energy <- fread('energy_dataset.csv') %>% janitor::clean_names()
weather <- fread('weather_features.csv') %>% janitor::clean_names()

energy %>% skim
energy %>% head(100) %>% View()
energy %>% isna
energy %>% select_if(~!is.logical(.)) %>% isna
energy %>% glimpse()

energy %<>% select_if(~!is.logical(.))
energy %>% isna()

weather %>% skim
weather %>% head(100) %>% View()

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

energy[!complete.cases(energy)] %>% View()

energy %>% isna()

energy %>% 
  mutate(generation_biomass = na.spline(generation_biomass)) %>% 
