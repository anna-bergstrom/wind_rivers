## 02_timeseries_summary_stats
# This will load the timeseries datasets and calculate summary statistics based on user specified intervals

  rm(list = ls())
  
  source("paths+packages.R")
  
  
  
  ##### Load data #########
  airT <- read.csv('R_import_data/AirT_logger.csv')%>% 
    mutate(Datetime = mdy_hm(Datetime, tz = 'America/Denver'))
  
  all_wind <- read.csv('outputs/01_full_wind_river_EC_TEMP_timeseries.csv')
  all_temp <- read.csv('outputs/01_full_wind_river_TEMP_timeseries.csv')
  all_ec <- read.csv('outputs/01_full_wind_river_EC_timeseries.csv')
  LC_stat <- read.csv('sample_stats.csv') #attributes of the sampling sites 
  colnames(LC_stat) <- c('site','elev', 'dist', 'w_slope', 's_slope', 'aspect','area','rk_gl', 'lk','gl','bare', 'frst',  'shrb', 'grs', 'gnis','gran', 'gl_dep', 'rugg_whole', 'rugg_100','rugg_50','rugg_25','slr_rad')
  
  all_wind$Datetime <- strptime(all_wind$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  all_temp$Datetime <- strptime(all_temp$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  all_ec$Datetime <- strptime(all_ec$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
  

  
  #Finding start and end of time series data to pull NWIS data 
  bounds <- c(all_wind$Datetime[1], all_wind$Datetime[length(all_wind$Datetime)])
  
  #pulling Q data form NWIS server
  gauge_data <- readNWISdata(sites = '06221400', service = 'iv', parameterCd = '00060', 
                             startDate = as.Date(bounds[1]), endDate = as.Date(bounds[2])) %>%
    select(Datetime = dateTime, Q = X_00060_00000)
  
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
  stat_dataEC <- summary_stats(all_ec, 'week')
  stat_dataT <- summary_stats(all_temp, 'week')
  stat_dataQ <- summary_stats(gauge_data, 'week')
  
  airT <- airT %>%
    subset(select = -pressure)
  stat_dataAirT <- summary_stats(airT, 'week')

########## Organizing stats to export for statistical modeling ###############

 # Pulling mean weekly Q
stat <- 1 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
stat_saveQ <- pivot_longer(stat_dataQ[[stat]],cols = 2:ncol(stat_dataQ[[stat]]),names_to = "names") 
colnames(stat_saveQ)[3] <- "mean_Q"
stat_saveQ <- stat_saveQ %>%
  subset(select = -names)

stat_saveairT <- pivot_longer(stat_dataAirT[[stat]],cols = 2:ncol(stat_dataAirT[[stat]]),names_to = "names") 
colnames(stat_saveairT)[3] <- "mean_AirT"
stat_saveairT <- stat_saveairT %>%
  subset(select = -names)
  
### For EC data ###
  stat <- 1 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
  stat_saveEC <- pivot_longer(stat_dataEC[[stat]],cols = 2:ncol(stat_dataEC[[stat]]),names_to = "names") 
  colnames(stat_saveEC)[3] <- "mean_EC"
  
  stat <- 4 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
  temp<- pivot_longer(stat_dataEC[[stat]],cols = 2:ncol(stat_dataEC[[stat]]),names_to = "names") 
  colnames(temp)[3] <- "range_EC"
  
  stat_saveEC <- stat_saveEC %>% mutate(range_EC = temp$range_EC)

stat_saveEC <- separate(stat_saveEC, col= names, into = c('type',"loc")) %>%
  subset(select = -type)

colnames(stat_saveEC)[2] <- "site"


# Repeat for temperature
stat <- 1 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
stat_saveT <- pivot_longer(stat_dataT[[stat]],cols = 2:ncol(stat_dataT[[stat]]),names_to = "names") 
colnames(stat_saveT)[3] <- "mean_temp"

stat <- 4 #variable to select with stat to plot, 1: mean, 2: max, 3: min, 4: range
temp<- pivot_longer(stat_dataT[[stat]],cols = 2:ncol(stat_dataT[[stat]]),names_to = "names") 
colnames(temp)[3] <- "range_temp"

stat_saveT <- stat_saveT %>% mutate(range_temp = temp$range_temp)

stat_saveT <- separate(stat_saveT, col= names, into = c('type',"loc")) %>%
  subset(select = -type)

colnames(stat_saveT)[2] <- "site"

LC_sub <- LC_stat %>% select(site, gnis, gl, lk, rugg_whole, rugg_25, slr_rad)

#joining EC and Temp data sets
stat_saveBoth<- merge(stat_saveT, stat_saveEC, all.x = TRUE)

stat_saveBoth <- merge(stat_saveBoth, LC_sub, by = "site")
stat_saveBoth <- merge(stat_saveBoth, stat_saveQ, by= "datetime_int")
stat_saveBoth <- merge(stat_saveBoth, stat_saveairT, by= "datetime_int")

stat_saveBoth<- stat_saveBoth[complete.cases(stat_saveBoth[ , 3:4]),]

###### Writing File ##########
readr::write_csv(stat_saveBoth, file = file.path("outputs", "02.1_wind_river_weeklystats.csv"))

ggplot(stat_saveBoth) +
  geom_point(aes(x = datetime_int, y= range_temp, colour = factor(site)), alpha = 0.25)+
  #geom_line(aes(x = Datetime, y= Sm.din5), col = 'blue')+
  ylab(expression(paste("temp"))) + 
  theme_cust()

ggplot(stat_saveBoth) +
  geom_point(aes(x = mean_AirT, y= mean_temp, colour = factor(site)))+
  #geom_line(aes(x = Datetime, y= Sm.din5), col = 'blue')+
  ylab(expression(paste("temp"))) + 
  theme_cust()
