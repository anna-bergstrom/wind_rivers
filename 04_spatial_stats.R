## 04_spatial_stats
# This will use the summary statistics and the spatial datasets and calculate correlations and linear models. 
rm(list= ls())
#setwd("/Users/anna/BSU_Drive/Projects/Wind_Rivers/wind_rivers")
source("paths+packages.R")

### Load Data####
all_wind <- read.csv('outputs/02_wind_river_ECtemp_monthstat.csv') #monthly temperature and EC stats
colnames(all_wind)[1]<-"site" 
all_wind <- all_wind[order(all_wind$site),]
LC_stat <- read.csv('sample_stats.csv') #attributes of the sampling sites 
colnames(LC_stat) <- c('site','elev', 'dist', 'w_slope', 's_slope', 'aspect','area','rk_gl', 'lk','gl','bare', 'frst',  'shrb', 'grs', 'gnis','gran', 'gl_dep', 'rugg_whole', 'rugg_100','rugg_50','rugg_25','slr_rad')
LC_stat <- LC_stat[order(LC_stat$site),]
  
# Starting with pulling out only the monthly mean and range temp and EC data 
means_stats <- select(all_wind,contains("mean"))
range_stats <- select(all_wind,contains("range"))
# Combining means and ranges  and spatial data and calculating a correlation matrix 
all_cor <- cor(cbind(LC_stat[,-1],means_stats, range_stats), use="complete.obs", method = "spearman")
corrplot(all_cor,  type = 'lower', diag = FALSE)


########### Determining highly correlated spatial variables ##########
spatial_cor <- cor(LC_stat[,-1])
spatial_cor_sort<- as.data.frame(as.table(spatial_cor)) # making a table of the correlation matrix 
spatial_cor_sort<-spatial_cor_sort[order(abs(spatial_cor_sort[,3])),]
spatial_cor_sort<-spatial_cor_sort[!duplicated(spatial_cor_sort[,3]),]

#Sorting spatial correlations for parameter pruning
#function to create a correlation matrix for a given stat and sort it  
stat_cor <- function(stat){
spat_cor<- cor(cbind(LC_stat[,-1], select(all_wind,contains(!!stat))), use="complete.obs", method = "spearman")
spat_cor_sort<- as.data.frame(as.table(spat_cor)) # making a table of the correlation matrix 
spat_cor_sort<-spat_cor_sort[order(abs(spat_cor_sort[,3])),] # sorting based on the correlation coefficient
opts<- c("Temp" , "EC") #taking out duplicates (next 3 lines)
spat_cor_sort = filter(spat_cor_sort, grepl(paste(opts, collapse = "|"), Var1))
spat_cor_sort = filter(spat_cor_sort, !grepl(paste(opts, collapse = "|"), Var2))
return(spat_cor_sort)
}

#applying function to each stat
EC_mean_cor <- stat_cor("mean_EC")
EC_range_cor <- stat_cor("range_EC")

temp_mean_cor <- stat_cor("mean_Temp")
temp_range_cor <- stat_cor("range_Temp")

# Sub-setting based on sorted correlations and removing highly correlated predictor variables 
# changing to subset a consistent dataset for EC based on hypothesized mechanisms
EC_subset <- subset(LC_stat, select = c(site, gran, gnis, gl, rugg_25, slr_rad))
#EC_range_subset <- subset(LC_stat, select = c(site, gran, rugg_100, bare, elev))
Temp_subset <- subset(LC_stat, select = c(site, gran, gnis,lk, gl, rugg_whole, dist, slr_rad))
#Temp_range_subset <- subset(LC_stat, select = c(site, dist, gnis, aspect, ice))


########## Normalizing data ##########
scaling <- function(data){
for (i in 1:length(data)){
data[,i] <- scale(data[,i] , center = min(data[,i], na.rm = TRUE ), scale = max(data[,i] , na.rm = TRUE) - min(data[,i], na.rm = TRUE))
}
return(data)
}

EC_sub_scaled <- scaling(EC_subset[,-1])
Temp_sub_scaled <- scaling(Temp_subset[,-1])
means_stats_scaled <- scaling(means_stats)
range_stats_scaled <- scaling(range_stats)

########## testing for the best general linear model ########## 
#see McManus et al., 2020 Freshwater Science https://doi.org/10.1086/710340


# try running a single model and include variable for month to see how variable these might be through time and to better compare within the model 

# function for setting up table for output
table_setup <- function(input){
glm_table <- as.data.frame(colnames(input))
colnames(glm_table)<-'param'
param <- as.data.frame('(Intercept)')
colnames(param)<-'param'
glm_table <- bind_rows(param, glm_table )
return(glm_table)
}

# applying function for each 
#EC_mean_glm <- table_setup(EC_sub_scaled)
EC_mean_glm <- table_setup(EC_subset[,-1])
#EC_range_glm <- table_setup(EC_sub_scaled)
EC_range_glm <- table_setup(EC_subset[,-1])

#Temp_mean_glm <- table_setup(Temp_sub_scaled)
Temp_mean_glm <- table_setup(Temp_subset[,-1])
#Temp_range_glm <- table_setup(Temp_sub_scaled)
Temp_range_glm <- table_setup(Temp_subset[,-1])

stat_table<- Temp_range_glm
subset_spat<-Temp_subset
param <- "range_Temp"
j<-3

# function to calculate the GLM  
glm_apply <- function(stat_table, subset_spat,  param){
stats_sub <-  select(all_wind,contains(!!param)) 
for(j in 1:(length(stats_sub))){
temp <- bestglm(cbind(subset_spat[!is.na(stats_sub[,j]),-1],stats_sub[!is.na(stats_sub[,j]),j]), IC = "BIC")
print(temp)
params <- as.data.frame(temp$BestModel$coefficients)
params$param<-rownames(params)
colnames(params)<-c('val','param')
stat_table<- merge(stat_table,params, by = 'param', all.x = TRUE)
glm_sub <- select(subset_spat, contains(!!params[2:length(params[,2]),2]))
glm_sub2 <- as.data.frame(glm_sub[!is.na(stats_sub[,j]),])
colnames(glm_sub2) <- colnames(glm_sub)
stat_diag <- stats_sub[!is.na(stats_sub[,j]),j]
plot_data <- cbind(stat_diag,glm_sub2)
preds <- colnames(glm_sub2)
back<- paste0(preds,collapse = "+")
glm_form <- paste0("stat_diag ~", back, collapse = "")
print(glm(as.formula(glm_form), data  = plot_data))
 print(plot(glm(as.formula(glm_form), data  = plot_data))) 
 
}
colnames(stat_table)<-c('param', 'Jun', 'Jul', 'Aug', 'Sep')
return(stat_table)

}

EC_mean_glm <- glm_apply(EC_mean_glm, EC_subset[,-1], "mean_EC")
EC_range_glm <- glm_apply(EC_range_glm, EC_subset[,-1], "range_EC")

Temp_mean_glm <- glm_apply(Temp_mean_glm, Temp_subset[,-1], "mean_Temp")
Temp_range_glm <- glm_apply(Temp_range_glm, Temp_subset[,-1], "range_Temp")


readr::write_csv(as.data.frame(EC_mean_glm), file = file.path("outputs", "04_wind_river_EC_mean_glm.csv"),na = "")
readr::write_csv(as.data.frame(EC_range_glm), file = file.path("outputs", "04_wind_river_EC_range_glm.csv"),na = "")

readr::write_csv(as.data.frame(Temp_mean_glm), file = file.path("outputs", "04_wind_river_Temp_mean_glm.csv"),na = "")
readr::write_csv(as.data.frame(Temp_range_glm), file = file.path("outputs", "04_wind_river_Temp_range_glm.csv"),na = "")




######## Calculating a series of Spearman rank correlations and making a table ##########
spear_table <- matrix(data=NA,nrow=length(LC_stat)-1,ncol=3)
spear <- function(spat, stat){
  for (k in 1:length(spat)) {
    temp <- cor.test(x=spat[,k], y=stat, method = "pearson")
    spear_table[k,1] <- colnames(spat[k])
    spear_table[k,2] <- temp$estimate
    spear_table[k,3] <- temp$p.value
  }
  return(spear_table)
}

range_cor_table <- colnames(LC_stat[2:length(LC_stat)])
mean_cor_table <- colnames(LC_stat[2:length(LC_stat)])


#for (j in 1:length(range_scaled)){
#  temp <- spear(LC_scaled, range_scaled[,j])
 # ind <- temp[,3]>0.1
 # temp[ind,2:3] <-NA
 # range_cor_table <- cbind(range_cor_table,temp[,2:3])
  
  
#  temp <- spear(LC_scaled, means_scaled[,j])
#  ind <- temp[,3]>0.05
#  temp[ind,2:3] <-NA
#  mean_cor_table <- cbind(mean_cor_table,temp[,2:3])
#}

readr::write_csv(as.data.frame(range_cor_table), file = file.path("outputs", "04_wind_river_range_cor_table.csv"),na = "")
readr::write_csv(as.data.frame(mean_cor_table), file = file.path("outputs", "04_wind_river_mean_cor_table.csv"),na = "")

########## Comparing temp range to other temp stats ########## 
df<- all_wind[,18:33] #2:17 = temp, 18:33 = EC 

temp_long<- reshape2::melt(df)

temp_chr<- str_split_fixed(temp_long$variable, "_", 3)
temp_long <- as.data.frame(cbind(temp_long[,2], temp_chr [,1:2]))
colnames(temp_long)<- c('temp', 'month', 'stat')
temp_long$temp <- as.numeric(temp_long$temp)
id <- rep(seq(1,length(all_wind[,1]),1), length(temp_long$temp)/length(all_wind[,1]))
temp_long<- cbind(temp_long,id)

arranged <- dcast(temp_long, month+id~ stat ,value.var = "temp")

#cols <- c("jun"= "#bae4bc" ,"jul"= "#7bccc4" ,"aug"="#43a2ca", "sep" = "#0868ac" )
cols <- c("jun"= "#b3cde3" ,"jul"= "#8c96c6" ,"aug"="#8856a7", "sep" = "#810f7c" )

ggplot(arranged, aes(x = range, y = min, colour = factor(month))) +
  geom_point( size = 5)+
  scale_colour_manual(values = cols)+
  ylab("Min EC")+
  xlab("EC Range")+
  theme_cust()+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14)) 

######### Plotting spatial variables against monthly means for a visual evaluation ##########

df<- cbind(LC_stat [,-1], means_stats[,8])
names(df) [length(df)]<- "sep_EC_mean"
juntemp_long <- reshape2::melt(df, id.vars = "sep_EC_mean")

ggplot(juntemp_long, aes(sep_EC_mean, value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(variable), scales = "free_y")
  
########## Pulling out just Dinwoody ##########
dinwood_stat = filter(all_wind, grepl("din", site))
dinwood_spat = filter(LC_stat, grepl("din", site))

dinwood_combined = cbind(dinwood_spat[,3], dinwood_stat [-1] )
colnames(dinwood_combined)[1]<-"dist" 
din_long<- reshape2::melt(dinwood_combined, id = "dist")
din_chr<- str_split_fixed(din_long$variable, "_", 3)
din_long <- as.data.frame(cbind(din_long[,-2], din_chr))
colnames(din_long)<- c('dist', 'value','month', 'stat','param')

dinplot <- function(input,meas,desc){
  subbed <- input %>%
    filter(param == !!meas & stat == !!desc)
  cols <- c("jun"= "#bae4bc" ,"jul"= "#7bccc4" ,"aug"="#43a2ca", "sep" = "#0868ac" )
  #cols <- c("jun"= "#b3cde3" ,"jul"= "#8c96c6" ,"aug"="#8856a7", "sep" = "#810f7c" )
  stat_plot <- ggplot(data = subbed)+
    geom_point(aes(x = dist, y = value, colour = factor(month) ),size=5)+
    scale_colour_manual(values = cols)+
    ylab(paste(meas, desc))+
    xlab("Distance from source")+
    theme_cust()+
    theme(axis.text = element_text(size = 14))+
    theme(axis.title = element_text(size = 14)) 
  
print(stat_plot)   
}

test <- dinplot(din_long, "EC", "range")
test <- dinplot(din_long, "EC", "mean")
test <- dinplot(din_long, "Temp", "mean")
test <- dinplot(din_long, "Temp", "range")




