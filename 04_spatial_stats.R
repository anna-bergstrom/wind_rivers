## 04_spatial_stats
# This will use the summary statistics and the spatial datasets and calculate correlations and linear models. 
rm(list= ls())

source("paths+packages.R")

all_wind <- read.csv('outputs/02_wind_river_ECtemp_monthstat.csv')
LC_stat <- read.csv('sample_stats.csv')

full_cor <- cor(all_wind[,-1], use="complete.obs")
corrplot.mixed(full_cor, lower = 'number', upper = 'circle', order = 'AOE')

means_stats <- select(all_wind,contains("mean"))
mean_cor <- cor(means_stats, use="complete.obs")
corrplot.mixed(mean_cor, lower = 'number', upper = 'circle', order = 'AOE')

spat_cor <- cor(cbind(LC_stat[,2:16],means_stats), use="complete.obs")
corrplot.mixed(spat_cor, lower = 'number', upper = 'circle', order = 'AOE')

spat_cor_sort<- as.data.frame(as.table(spat_cor))
spat_cor_sort<-spat_cor_sort[order(spat_cor_sort[,3]),]
spat_cor <- cor(LC_subset[,2:11], use="complete.obs")

LC_subset <- subset(LC_stat, select = -c(mean_wtrshdslope_deg, glacier_prcnt, woodland_prcnt, barren_prcnt, shrub_prcnt))

bestglm(cbind(LC_subset[,2:11],means_stats[,3]), IC = "AIC")