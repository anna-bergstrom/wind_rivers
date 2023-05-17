
##### setup #####
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)

########### Setting up details for this script #############
theme_cust <- function(base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}

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
  
  #Function to calculate stats, input datset: all_temp or all_EC and interval over which to calcualte stats: daily, weekly, monthly
  #Calculates mean, max, min, and range for the designated interval, all stats are output as a list of dataframes, one for each stat. 
  summary_stats <- function(dataset,interval) {
    int_mean <- dataset %>%    
      mutate(datetime_int = cut(Datetime,interval)) %>%
      group_by(datetime_int) %>%
      summarise_if(is.numeric,mean,na.rm = TRUE)
    
    int_max <- dataset %>%    
      mutate(datetime_int = cut(Datetime,interval)) %>%
      group_by(datetime_int) %>%
      summarise_if(is.numeric,max,na.rm = TRUE)
    
    int_min <- dataset %>%    
      mutate(datetime_int = cut(Datetime,interval)) %>%
      group_by(datetime_int) %>%
      summarise_if(is.numeric,min,na.rm = TRUE)
    
    int_amp <- int_max[2:ncol(int_min)]- int_min[2:ncol(int_min)] 
    int_amp[int_amp == -Inf] <- NA
    int_amp <- add_column(.data= int_amp, datetime_int = int_max$datetime_int, .before = 1)
    
    out <- list(int_mean, int_max, int_min, int_amp)
    return(out)
  }
  
  # Line to run the function for a given parameter and interval 
  stat_data <- summary_stats(all_temp, 'week')
  
  ############## Plotting statistics ###################
  #not paper quality figures, just to look at the data 
  
  stat <- 4 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: daily range
  stat_plot <- pivot_longer(stat_data[[stat]],cols = 2:ncol(stat_data[[stat]]),names_to = "names") %>%
    mutate(type = case_when(
      str_detect(names,"dblk|kndk|dnsfk|clr|ngrs")~"Non-gl",
      str_detect(names,"din1|din2|din4|din5|din6|din7|gan2|grs3")~"Glacial"
    ))
  
    ggplot(stat_plot, aes(x=datetime_int , y= value)) +
    geom_point(aes(color= factor(type)))+
    scale_colour_brewer(palette = "Paired")+
    ylab(expression(paste("Daily Average Temp"))) + 
    theme_cust()
    
