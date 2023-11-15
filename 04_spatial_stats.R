## 04_spatial_stats
# This will use the summary statistics and the spatial datasets and calculate correlations and linear models. 
rm(list= ls())

source("paths+packages.R")

### Load Data####
all_wind <- read.csv('outputs/02_wind_river_ECtemp_monthstat.csv') #monthly temperature and EC stats
LC_stat <- read.csv('sample_stats.csv') #attributes of the sampling sites 
colnames(LC_stat) <- c('site','elev', 'w_slope', 's_slope', 'aspect','area','gl', 'lk', 'frst', 'ice', 'bare', 'shrb', 'grs', 'gnis','gran', 'gl_dep', 'dist')
  
# Evaluating how correlated Temp and EC data are
full_cor <- cor(all_wind[,-1], use="complete.obs") # use argument deals with the NAs 
corrplot.mixed(full_cor, lower = 'number', upper = 'circle', order = 'AOE')

# Starting with pulling out only the monthly mean temp and EC data 
means_stats <- select(all_wind,contains("mean"))
mean_cor <- cor(means_stats, use="complete.obs")
corrplot.mixed(mean_cor, lower = 'number', upper = 'circle', order = 'AOE')

range_stats <- select(all_wind,contains("range"))
range_cor <- cor(range_stats, use="complete.obs")
corrplot.mixed(range_cor, lower = 'number', upper = 'circle', order = 'AOE')

# Combining means and spatial data and calculating a correlation matrix 
spat_cor <- cor(cbind(LC_stat[,-1],means_stats), use="complete.obs")
corrplot.mixed(spat_cor, lower = 'number', upper = 'circle')

spat_cor_sort<- as.data.frame(as.table(spat_cor)) # making a table of the correlation matrix 
spat_cor_sort<-spat_cor_sort[order(spat_cor_sort[,3]),] # sorting based on the correlation coefficient

# Removing highly correlated spatial variables and recalculating the correlation matrix
LC_subset1 <- subset(LC_stat, select = -c(w_slope, gl, frst, bare, shrb, elev, gnis, lk, aspect, area))
LC_subset2 <- subset(LC_stat, select = -c(w_slope, gl, frst, bare, shrb))
spat_cor_sub <- cor(LC_subset1[,2:length(LC_subset1)], use="complete.obs")
corrplot.mixed(spat_cor_sub, lower = 'number', upper = 'circle', order = 'AOE')


# Normalizing data 
scaling <- function(data){
for (i in 1:length(data)){
data[,i] <- scale(data[,i] , center = min(data[,i], na.rm = TRUE ), scale = max(data[,i] , na.rm = TRUE) - min(data[,i], na.rm = TRUE))
}
return(data)
}

means_scaled <- scaling(means_stats)
range_scaled <- scaling(range_stats)
LC_scaled <- scaling(LC_stat[,-1])
LC_sub_scaled <- scaling(LC_subset1[,-1])


# Combining scaled means and spatial data and calculating a correlation matrix 
spat_cor <- cor(cbind(LC_scaled ,means_scaled), use="complete.obs")
corrplot.mixed(spat_cor, lower = 'number', upper = 'circle')

# Combining scaled range and spatial data and calculating a correlation matrix 
spat_cor <- cor(cbind(LC_scaled, range_scaled), use="complete.obs")
corrplot.mixed(spat_cor, lower = 'number', upper = 'circle')

# Calculating a series of Spearman rank correlations and making a table
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


for (j in 1:length(range_scaled)){
temp <- spear(LC_scaled, range_scaled[,j])
ind <- temp[,3]>0.1
temp[ind,2:3] <-NA
range_cor_table <- cbind(range_cor_table,temp[,2:3])


temp <- spear(LC_scaled, means_scaled[,j])
ind <- temp[,3]>0.05
temp[ind,2:3] <-NA
mean_cor_table <- cbind(mean_cor_table,temp[,2:3])
}

readr::write_csv(as.data.frame(range_cor_table), file = file.path("outputs", "04_wind_river_range_cor_table.csv"),na = "")
readr::write_csv(as.data.frame(mean_cor_table), file = file.path("outputs", "04_wind_river_mean_cor_table.csv"),na = "")

##Plotting spatial variables against monthly means for a visual evaluation
# Jun Temp

df<- cbind(LC_stat [,-1], means_stats[,1])
names(df) [length(df)]<- "sep_temp_mean"
juntemp_long <- reshape2::melt(df, id.vars = "sep_temp_mean")

ggplot(juntemp_long, aes(sep_temp_mean, value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(variable), scales = "free_y")


# testing for the best general linear model 
#see McManus et al., 2020 Freshwater Science https://doi.org/10.1086/710340
glm_mean_table <- as.data.frame(colnames(LC_sub_scaled))
colnames(glm_mean_table)<-'param'
param <- as.data.frame('(Intercept)')
colnames(param)<-'param'
glm_mean_table <- bind_rows(param, glm_mean_table )
glm_range_table <- glm_mean_table

for(j in 1:(length(means_stats)-1)){
temp <- bestglm(cbind(LC_sub_scaled[!is.na(means_stats[,j]),],means_stats[!is.na(means_stats[,j]),j]), IC = "AIC")
print(temp)
params <- as.data.frame(temp$BestModel$coefficients)
params$param<-rownames(params)
colnames(params)<-c('val','param')
glm_mean_table<- merge(glm_mean_table,params, by = 'param', all.x = TRUE)
}
colnames(glm_mean_table)<-c('param', 'Jun_temp', 'Jul_temp', 'Aug_temp', 'Sep_temp', 'Jun_EC','Jul_EC','Aug_EC' )

for(j in 1:(length(range_stats)-1)){
  temp <- bestglm(cbind(LC_sub_scaled[!is.na(range_stats[,j]),],range_stats[!is.na(range_stats[,j]),j]), IC = "BIC")
  print(temp)
  params <- as.data.frame(temp$BestModel$coefficients)
  params$param<-rownames(params)
  colnames(params)<-c('val','param')
  glm_range_table<- merge(glm_range_table,params, by = 'param', all.x = TRUE)
}
colnames(glm_range_table)<-c('param', 'Jun_temp', 'Jul_temp', 'Aug_temp', 'Sep_temp', 'Jun_EC','Jul_EC','Aug_EC' )

readr::write_csv(as.data.frame(glm_range_table), file = file.path("outputs", "04_wind_river_range_glm_table.csv"),na = "")
readr::write_csv(as.data.frame(glm_mean_table), file = file.path("outputs", "04_wind_river_mean_glm_table.csv"),na = "")
