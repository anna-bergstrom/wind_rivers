
##### setup #####
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)
library(misty)

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

# Writing output to a .CSV 
readr::write_csv(all_wind, file = file.path("outputs", "full_wind_river_EC_TEMP_timeseries.csv"))

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
    stats <- c("mean", "max", "min", "range")
    return(out)
  }
  
  # Line to run the function for a given parameter and interval 
  stat_dataEC <- summary_stats(all_EC, 'month')
  stat_dataT <- summary_stats(all_temp, 'month')
########## Organizing stats to export for statistical modeling 
  
  stat_saveEC <- data.frame(site = colnames(stat_dataEC[[1]])[2:ncol(stat_dataEC[[1]])])
  
  for (i in 1:length(stat_dataEC)) {
  stat <- i #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
  stat_plot <- pivot_longer(stat_dataEC[[stat]],cols = 2:ncol(stat_dataEC[[stat]]),names_to = "names") %>%
    mutate(type = case_when(
      str_detect(names,"dblk|kndk|dnsfk|clr|ngrs")~"Non-gl",
      str_detect(names,"din1|din2|din4|din5|din6|din7|gan2|grs3")~"Glacial"
    ))
    
    col_names <- c("site",paste("jun",stats[i], "EC", sep = "."), paste("jul",stats[i], "EC", sep = "."), paste("aug",stats[i], "EC", sep = "."), paste("sep",stats[i], "EC", sep = "."))
    
      temp <- subset(stat_plot, select = -type) %>%
        pivot_wider(names_from = "datetime_int", values_from = value) %>%
        df.rename(from = c("names", "2018-06-01", "2018-07-01","2018-08-01", "2018-09-01"), to = col_names)
  
      stat_saveEC <- merge(stat_saveEC, temp, by = "site")
  }
  
  # Repeat for temperature
  stat_saveT <- data.frame(site = colnames(stat_dataT[[1]])[2:ncol(stat_dataT[[1]])])
  
  for (i in 1:length(stat_dataT)) {
    stat <- i #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
    stat_plot <- pivot_longer(stat_dataT[[stat]],cols = 2:ncol(stat_dataT[[stat]]),names_to = "names") %>%
      mutate(type = case_when(
        str_detect(names,"dblk|kndk|dnsfk|clr|ngrs")~"Non-gl",
        str_detect(names,"din1|din2|din4|din5|din6|din7|gan2|grs3")~"Glacial"
      ))
    
    col_names <- c("site",paste("jun",stats[i], "Temp", sep = "."), paste("jul",stats[i], "Temp", sep = "."), paste("aug",stats[i], "Temp", sep = "."), paste("sep",stats[i], "Temp", sep = "."))
    
    temp <- subset(stat_plot, select = -type) %>%
      pivot_wider(names_from = "datetime_int", values_from = value) %>%
      df.rename(from = c("names", "2018-06-01", "2018-07-01","2018-08-01", "2018-09-01"), to = col_names)
    
    stat_saveT <- merge(stat_saveT, temp, by = "site")
  }
  
  ############## Plotting statistics ###################
  #not paper quality figures, just to look at the data 
  ggplot(stat_plot, aes(x=datetime_int , y= value)) +
    geom_point(aes(color= factor(type)))+
    scale_colour_brewer(palette = "Paired")+
    ylab(expression(paste("Daily Average Temp"))) + 
    theme_cust()
  
  