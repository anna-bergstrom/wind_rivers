## 02_timeseries_summary_stats
# This will load the timeseries datasets and calculate summary statistics based on user specified intervals

rm(list = ls())

source("paths+packages.R")



##### Load data #########
all_wind <- read.csv('outputs/01_full_wind_river_EC_TEMP_timeseries.csv')
all_temp <- read.csv('outputs/01_full_wind_river_TEMP_timeseries.csv')
all_ec <- read.csv('outputs/01_full_wind_river_EC_timeseries.csv')

all_wind$Datetime <- strptime(all_wind$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
all_temp$Datetime <- strptime(all_temp$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
all_ec$Datetime <- strptime(all_ec$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')

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
  int_max[int_max == -Inf] <- NA
  
  int_min <- dataset %>%    
    mutate(datetime_int = cut(Datetime,interval)) %>%
    group_by(datetime_int) %>%
    summarise_if(is.numeric,min,na.rm = TRUE)
  int_min[int_min == Inf] <- NA
  
  int_amp <- int_max[2:ncol(int_min)]- int_min[2:ncol(int_min)] 
  int_amp[int_amp == -Inf] <- NA
  int_amp <- add_column(.data= int_amp, datetime_int = int_max$datetime_int, .before = 1)
  
  out <- list(int_mean, int_max, int_min, int_amp)
  return(out)
}

# Line to run the function for a given parameter and interval 
stat_dataEC <- summary_stats(all_ec, 'month')
stat_dataT <- summary_stats(all_temp, 'month')

########## Organizing stats to export for statistical modeling ###############
stats <- c("mean", "max", "min", "range")

stat_saveEC <- data.frame(site = colnames(stat_dataEC[[1]])[2:ncol(stat_dataEC[[1]])])

for (i in 1:length(stat_dataEC)) {
  stat <- i #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
  stat_plot <- pivot_longer(stat_dataEC[[stat]],cols = 2:ncol(stat_dataEC[[stat]]),names_to = "names") %>%
    mutate(type = case_when(
      str_detect(names,"dblk|kndk|dnsfk|clr|ngrs")~"Non-gl",
      str_detect(names,"din1|din2|din4|din5|din6|din7|gan2|grs3")~"Glacial"
    ))
  
  col_names <- c("site",paste("jun",stats[i], "EC", sep = "_"), paste("jul",stats[i], "EC", sep = "_"), paste("aug",stats[i], "EC", sep = "_"), paste("sep",stats[i], "EC", sep = "_"))
  
  temp <- subset(stat_plot, select = -type) %>%
    pivot_wider(names_from = "datetime_int", values_from = value) %>%
    df.rename(from = c("names", "2018-06-01", "2018-07-01","2018-08-01", "2018-09-01"), to = col_names)
  
  stat_saveEC <- merge(stat_saveEC, temp, by = "site")
}

stat_saveEC <- separate(stat_saveEC, col= site, into = c('type',"loc")) %>%
  subset(select = -type)


# Repeat for temperature
stat_saveT <- data.frame(site = colnames(stat_dataT[[1]])[2:ncol(stat_dataT[[1]])])

for (i in 1:length(stat_dataT)) {
  stat <- i #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
  stat_plot <- pivot_longer(stat_dataT[[stat]],cols = 2:ncol(stat_dataT[[stat]]),names_to = "names") %>%
    mutate(type = case_when(
      str_detect(names,"dblk|kndk|dnsfk|clr|ngrs")~"Non-gl",
      str_detect(names,"din1|din2|din4|din5|din6|din7|gan2|grs3")~"Glacial"
    ))
  
  col_names <- c("site",paste("jun",stats[i], "Temp", sep = "_"), paste("jul",stats[i], "Temp", sep = "_"), paste("aug",stats[i], "Temp", sep = "_"), paste("sep",stats[i], "Temp", sep = "_"))
  
  temp <- subset(stat_plot, select = -type) %>%
    pivot_wider(names_from = "datetime_int", values_from = value) %>%
    df.rename(from = c("names", "2018-06-01", "2018-07-01","2018-08-01", "2018-09-01"), to = col_names)
  
  stat_saveT <- merge(stat_saveT, temp, by = "site")
}

stat_saveT <- separate(stat_saveT, col= site, into = c('type',"loc")) %>%
  subset(select = -type)

stat_save <- merge(stat_saveT, stat_saveEC, by = "loc", all.x = TRUE)

###### Writing File ##########
readr::write_csv(stat_save, file = file.path("outputs", "02_wind_river_ECtemp_monthstat.csv"))

############## Plotting statistics ###################
#not paper quality figures, just to look at the data 
ggplot(stat_plot, aes(x=datetime_int , y= value)) +
  geom_point(aes(color= factor(type)))+
  scale_colour_brewer(palette = "Paired")+
  ylab(expression(paste("Daily Average Temp"))) + 
  theme_cust()

ggplot(all_temp, aes(x=as.POSIXct(Datetime) , y= Temp.dnsfk)) +
  geom_point()+
  ylab(expression(paste("Daily Average Temp"))) + 
  theme_cust()
