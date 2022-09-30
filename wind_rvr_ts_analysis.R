
##### setup #####
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)


##### Load data #########
clear <- read.csv('R_import_data/Clear.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood1 <- read.csv('R_import_data/Dinwoody1.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood2 <- read.csv('R_import_data/Dinwoody2.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood4 <- read.csv('R_import_data/Dinwoody4.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood5 <- read.csv('R_import_data/Dinwoody5.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood6 <- read.csv('R_import_data/Dinwoody6.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dinwood7 <- read.csv('R_import_data/Dinwoody7.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

dbllake <- read.csv('R_import_data/DoubleLake.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

downsfrk <- read.csv('R_import_data/DownsFork.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

ganet2 <- read.csv('R_import_data/Gannett2.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

grass3 <- read.csv('R_import_data/Grass3.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

klndke <- read.csv('R_import_data/klondike.csv') %>% 
  mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))

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

# generating  data frames just for temp and EC
  all_temp <- all_wind %>%
    select(Datetime,starts_with("Temp"))
  
  all_EC <- all_wind %>%
    select(Datetime,starts_with("EC"))
  
 ############ Calculating Stats ############
  test_stat <- all_temp %>%
    mutate(datetime_daily = cut(Datetime,'day')) %>%
    group_by(datetime_daily) %>%
    summarise_if(is.numeric,mean,na.rm = TRUE)
  
  stat_plot <- melt(test_stat,datetime_daily,id = 2:13)
  
