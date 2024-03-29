## 03_timeseries_clustering
# This will load the timeseries datasets and calculate summary statistics based on user specified intervals

source("paths+packages.R")

rm()

##### Load data #########
all_temp <- read.csv('outputs/01_full_wind_river_TEMP_timeseries.csv')
all_ec <- read.csv('outputs/01_full_wind_river_Smoothed_timeseries.csv')

all_temp$Datetime <- strptime(all_temp$Datetime, "%Y-%m-%dT%H:%M:%S")
all_ec$Datetime <- strptime(all_ec$Datetime, "%Y-%m-%dT%H:%M:%S")

# Filtering down to where there's more data
bounds<- as.POSIXct(c('06/25/2018 00:00:00','09/23/2018 23:45:00'), format="%m/%d/%Y %H:%M:%S", TZ = "America/Denver")

all_temp_clip <- all_temp %>%
  filter(as.POSIXct(Datetime) >= bounds[1], as.POSIXct(Datetime) <= bounds[2]) 

all_ec_clip <- all_ec %>%
  filter(as.POSIXct(Datetime) >= bounds[1], as.POSIXct(Datetime) <= bounds[2]) 


days <- yday(all_ec_clip$Datetime)
days <- matrix(days , nrow = 96)
days <- days[1,]

tdays <- yday(all_temp_clip$Datetime)
tdays <- matrix(days , nrow = 96)
tdays <- days[1,]

EC_mat = seq(1, 96, 1)
temp_mat = seq(1, 96, 1)

for (i in 2:ncol(all_ec_clip)){
 temp <- matrix(all_ec_clip[ ,i] , nrow = 96)
  colnames(temp) <-  as.vector(outer(colnames(all_ec_clip)[i], days, paste, sep="."))
  EC_mat <- cbind(EC_mat,temp)
} 

for (i in 2:ncol(all_temp_clip)){
  temp <- matrix(all_temp_clip[ ,i] , nrow = 96)
  colnames(temp) <-  as.vector(outer(colnames(all_temp_clip)[i], days, paste, sep="."))
  temp_mat <- cbind(temp_mat,temp)
} 

# Removing any day with an NaN
EC_mat <- EC_mat[ , colSums(is.na(EC_mat)) == 0] 
temp_mat <- temp_mat[ , colSums(is.na(temp_mat)) == 0] 
#transposing to do the clustering 
EC_mat_T <- t(EC_mat[,-1])
temp_mat_T <- t(temp_mat[,-1])

#calculating dissimilarity matrix using TSclust diss() function
EC_dist <- diss(SERIES=EC_mat_T, METHOD="EUCL") #DTW = Dynamic Time Warping
EC_Dist <- dist(EC_mat_T, method = "euclidean")
#adding informative column names
names(EC_dist) <- rownames(EC_mat_T)

#perform hierachical clustering on the diss object
EC_hclust <- hclust(EC_dist, method="ward.D")


#show the resulting dendrogram
plot(EC_hclust, main="EC Clustering")

#determine the optimal number of clusters
fviz_nbclust(EC_mat_T, FUN = hcut, method = "wss")
fviz_nbclust(EC_mat_T, FUN = hcut, method = "silhouette")

#cut tree to the optimal number of clusters (4?)
sub_grp <- cutree(EC_hclust, k = 3)

#Calculating stats for clustering
cluster.stats(EC_Dist, sub_grp)

#EC_classed <- cbind(sub_grp, EC_mat_T)

TEST <- melt(t(EC_mat[, sub_grp == 1]))

ggplot(melt(t(EC_mat[, sub_grp == 1]))) +
  geom_line(aes(x = Var2,y = value,color = Var1),size = 1) 
