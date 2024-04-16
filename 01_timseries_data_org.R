## 01_timseries_data_org
# This will load the time series data sets, combine, organize save the output
rm(list = ls())

source("paths+packages.R")


##### Load data #########
clear <- read.csv('R_import_data/Clear.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood1 <- read.csv('R_import_data/Dinwoody1.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood2 <- read.csv('R_import_data/Dinwoody2.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.din2 = rollapply(EC.din2,32,mean, na.rm = TRUE, fill = NA))

dinwood4 <- read.csv('R_import_data/Dinwoody4.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.din4 = rollapply(EC.din4,32,mean, na.rm = TRUE, fill = NA))

dinwood5 <- read.csv('R_import_data/Dinwoody5.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.din5 = rollapply(EC.din5,32,mean, na.rm = TRUE, fill = NA))

dinwood6 <- read.csv('R_import_data/Dinwoody6.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.din6 = rollapply(EC.din6,32,mean, na.rm = TRUE, fill = NA))

dinwood7 <- read.csv('R_import_data/Dinwoody7.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.din7 = rollapply(EC.din7,32,mean, na.rm = TRUE, fill = NA))

dbllake <- read.csv('R_import_data/DoubleLake.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

downsfrk <- read.csv('R_import_data/DownsFork.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.dnsfk = rollapply(EC.dnsfk,32,mean, na.rm = TRUE, fill = NA))

ganet2 <- read.csv('R_import_data/Gannett2.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.gan2 = rollapply(EC.gan2,32,mean, na.rm = TRUE, fill = NA))

grass3 <- read.csv('R_import_data/Grass3.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

klndke <- read.csv('R_import_data/klondike.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'),
         Sm.kndk = rollapply(EC.kndk,32,mean, na.rm = TRUE, fill = NA))

nglgrs <- read.csv('R_import_data/NGLGrass.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

names <- c('dblk', 'din1','din2','din4','din5','din6','din7','kndk','dnsfk','gan2','grs3','clear','ngrs')

## combining all data sets into a single data frame with a single datetime column

all_wind <- dbllake %>%
  full_join(.,dinwood1, by = 'Datetime', ) %>%
  full_join(.,dinwood2, by = 'Datetime',) %>%
  full_join(.,dinwood4, by = 'Datetime',) %>%
  full_join(.,dinwood5, by = 'Datetime',) %>%
  full_join(.,dinwood6, by = 'Datetime',) %>%
  full_join(.,dinwood7, by = 'Datetime',) %>%
  full_join(.,klndke, by = 'Datetime',) %>%
  full_join(.,downsfrk, by = 'Datetime',) %>%
  full_join(.,ganet2, by = 'Datetime',) %>%
  full_join(.,grass3, by = 'Datetime',) %>%
  full_join(.,clear, by = 'Datetime',) %>%
  full_join(.,nglgrs, by = 'Datetime',) 

# Writing output to a .CSV 
readr::write_csv(all_wind, file = file.path("outputs", "01_full_wind_river_EC_TEMP_timeseries.csv"))

# generating  data frames just for temp and EC
  all_temp <- all_wind %>%
    select(Datetime,starts_with("Temp"))
  readr::write_csv(all_temp, file = file.path("outputs", "01_full_wind_river_TEMP_timeseries.csv"))
  
  all_EC <- all_wind %>%
    select(Datetime,starts_with("EC"))
  readr::write_csv(all_EC, file = file.path("outputs", "01_full_wind_river_EC_timeseries.csv"))
  
  all_Sm <- all_wind %>%
    select(Datetime,starts_with("Sm"))
  readr::write_csv(all_Sm, file = file.path("outputs", "01_full_wind_river_Smoothed_timeseries.csv"))
  

  

  
  ggplot(all_Sm) +
    #geom_point(aes(x = Datetime, y= EC.din5), alpha = 0.25)+
    geom_line(aes(x = Datetime, y= Sm.din5), col = 'blue')+
    ylab(expression(paste("Temp"))) + 
    theme_cust()
  