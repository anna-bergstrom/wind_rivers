## 05_timeseries_plotting
# This will load the time series data sets plot the timeseries. 

rm(list = ls())

source("paths+packages.R")

##### Load data #########
all_temp <- read.csv('outputs/01_full_wind_river_TEMP_timeseries.csv')
all_ec <- read.csv('outputs/01_full_wind_river_EC_timeseries.csv')
ec_sm <-  read.csv('outputs/01_full_wind_river_Smoothed_timeseries.csv')
air_temp <- read.csv('R_import_data/AirT_logger.csv')


all_temp$Datetime <- strptime(all_temp$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
all_ec$Datetime <- strptime(all_ec$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
ec_sm$Datetime <- strptime(ec_sm$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
air_temp$Datetime <-as.POSIXct(air_temp$Datetime, "%m/%d/%Y %H:%M", tz = 'America/Denver')

# Renaming columns so they display better in the legend
colnames(all_temp) <- c("Datetime","double lake", "dinwoody 1", "dinwoody 2", "dinwoody 4", "dinwoody 5", "dinwoody 6", "dinwoody 7", "klondike","downs fork", "ganett 2","grass 3", "clear", "grass")
colnames(all_ec) <- c("Datetime","double lake", "dinwoody 1", "dinwoody 2", "dinwoody 4", "dinwoody 5", "dinwoody 6", "dinwoody 7", "klondike","downs fork", "ganett 2","grass 3", "clear", "grass")

# Dinwoody 2 was collected hourly, doing an interpolation so it plots (geom_line doesn't work if there's NAs in the timeseries)
all_temp[,4] <- na_interpolation(all_temp[,4], option = 'linear', maxgap = 10)
all_ec[,4] <- na_interpolation(all_ec[,4], option = 'linear', maxgap = 10)
ec_sm[,2] <- na_interpolation(all_ec[,4], option = 'linear', maxgap = 10)

ec_sub<- all_ec %>%
  select(!!c(1, 4:11) )

# defining the coloramp for the plots 
col_ramp <- c("#f78555" ,"#3d9c73",'#63b179','#88c580',"#d6ec91", "#ffe27f","#fda55b","#ed6456","#de425b","#488f31","#ffff9d","#aed987","#ffc469")
#ec_ramp <- c('#88c580',"#d6ec91", "#ffe27f","#fda55b","#ed6456","#de425b","#488f31","#ffff9d")
ec_ramp <- c('#63b179' , "#88c580" ,'#d6ec91' , "#fee17e" ,  "#f7a258" , "#e4604e" , "#d43d51", "#00876c"  )


# changing datetime  to a POSIXct format
all_temp$Datetime <- as.POSIXct(all_temp$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
ec_sub$Datetime <- as.POSIXct(ec_sub$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')
ec_sm$Datetime <- as.POSIXct(ec_sm$Datetime, "%Y-%m-%dT%H:%M:%S", tz = 'UTC')


# Reorganizing data for facted plot
temp_flip <- melt(all_temp,id="Datetime",variable_name="site")
ec_flip <- melt(ec_sub,id="Datetime",variable_name="site")
sm_flip <- melt(ec_sm,id="Datetime",variable_name="site")

#Plot air temperature
ggplot(data= air_temp, aes(x= as.POSIXct(Datetime), y = airT_C, group=1))+
  geom_line(colour = "#6495ED")+
  geom_hline(yintercept= 0, linetype = "dashed")+
  ylab(bquote('Air Temperature (\u00B0C)'))+
  xlab("")+
  scale_x_datetime(breaks = breaks_pretty(10))+
  theme_cust()

# Plot with all temperature time series in the same plot 
ggplot(temp_flip, aes(Datetime, value, colour = variable)) +
  geom_line() +
  ylab(bquote('Temp (\u00B0C)'))+
  scale_color_manual(values=col_ramp)+
  theme_cust()

#faceted plot with temperature time series in individual panels 
 ggplot(temp_flip, aes(Datetime, value, colour = variable)) +
  geom_line() +
  ylab(bquote('Temp (\u00B0C)'))+
   xlab("")+
   scale_color_manual(values=col_ramp)+
   facet_wrap(~factor(variable, levels = c("downs fork", "klondike", "double lake", "dinwoody 7", "grass", "dinwoody 6", "grass 3", "dinwoody 5", "clear", "dinwoody 4", "dinwoody 2", "dinwoody 1", "ganett 2" )))+
   theme_cust()+
   theme( strip.text.x = element_blank() )

 ########### EC Plots #############
 # Plot with all EC time series in the same plot 

 # Plot with all temperature time series in the same plot 
 ggplot(sm_flip, aes(Datetime, value, colour = variable)) +
   geom_line() +
   ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
   scale_color_manual(values=ec_ramp)+
   theme_cust()
 
 #faceted plot with temperature time series in individual panels 
 ggplot(sm_flip, aes(Datetime, value, colour = variable)) +
   geom_line() +
   ylab(expression(paste("EC (" ,  mu,  "S cm"^"-1", ")"))) + 
   xlab("")+
   scale_color_manual(values=ec_ramp)+
   facet_wrap(~factor(variable, levels = c("Sm.dnsfk", "Sm.kndk", "Sm.din7",  "Sm.din6", "Sm.din5",  "Sm.din4", "Sm.din2", "Sm.din1", "Sm.gan2" )))+
   theme_cust()+
   theme( strip.text.x = element_blank() )
 
 #00876c 1
 #3d9c73 2
 #63b179 3
 #88c580 4
 #aed987 5
 #d6ec91 6
 #ffff9d 7
 #fee17e 8
 #fcc267 9
 #f7a258 10
 #ef8250 11
 #e4604e 12
 #d43d51 13
 
 #ec_names <- colnames(ec_sm)
 
 