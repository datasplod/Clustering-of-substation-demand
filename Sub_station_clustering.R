require(ggplot2)
require(tidyverse)
require(cluster)
require(factoextra)
require(rdist)
require(gridExtra)

#import data

load("January_2013.RData")
load("January_2014.RData")
load("January_2015.RData")
Characteristics <- read.csv("Characteristics.csv", stringsAsFactors = FALSE)



#QUESTION 1:  Initial Data Analysis Tasks
#1.a: Summarise the data in the CHaracteristics dataset and plot the distributions

summary(Characteristics)

#Transformer_RATING - size of total power being delivered
#Percentage_IC - percentage of industrial and commercial customers
#TRANSFORMER_TYPE - ground or pole mounted

library(hrbrthemes)

p<- Characteristics %>%
  ggplot(aes(x=Percentage_IC))+
  geom_histogram(fill = '#69b3a2', color = "#e9ecef", alpha = 0.9)+
  ggtitle("Distribution of industrial and commercial customers")+
  theme_ipsum()+
  theme(plot.title = element_text(size=12))+
  xlab("Percentage of industrial and commercial customers")+
  ylab("Frequency of customers")
p

p1<- ggplot(data = Characteristics)+
  geom_bar(mapping= aes(x=TRANSFORMER_TYPE), fill = '#69b3a2', 
           color = "#e9ecef", alpha = 0.9)+
  ggtitle("Type of Transformer")+
  theme_ipsum()+
  theme(plot.title = element_text(size=12))+
  xlab("")+
  ylab("Frequency")
p1

p2<- Characteristics %>%
  ggplot(aes(x=Transformer_RATING))+
  geom_histogram(binwidth = 150, fill = '#69b3a2', color = "#e9ecef", alpha = 0.9)+
  ggtitle("Distribution of total power delivered")+
  theme_ipsum()+
  theme(plot.title = element_text(size=12))+
  xlab("Transformer Rating")+
  ylab("Frequency")
p2

p3<- Characteristics %>%
  ggplot(aes(x=TOTAL_CUSTOMERS))+
  geom_histogram(fill = '#69b3a2', color = "#e9ecef", alpha = 0.9)+
  ggtitle("Number of customers served")+
  theme_ipsum()+
  theme(plot.title = element_text(size=12))+
  xlab("Customers served")+
  ylab("Frequency")
p3

library(gridExtra)
grid.arrange(p, p1, p2, p3, ncol=2)

## Qu 1 ii) Describe the relationships between the different substation characteristics

library(GGally)
pairplot <- ggpairs(Characteristics[,2:6], title="Correlogram of substation characteristics")

corrviz <- ggcorr(Characteristics[,3:6], method = c('everything', 'pearson'))
#only significant correlation is between the Total Customers and Feeder Count....

##QUESTION 2:  Using scaled daily measurements from Jan 2013 dataset perform hierarchical clustering for the daily average demand

##Qu 2i) 
#1.  scale daily measurements - actual measurement divided by the daily max
#remove na's
January_2013 <- na.omit(January_2013)

#as dataframe 
January_2013 <- as.data.frame(January_2013)
#remove the label data and convert to matrix ready for scaling
df_13 <- data.matrix(January_2013[,3:146])

#create daily maxs
daily_maximums <- apply(df_13,1,max)

#scale by daily max
scaled_data <- t(scale(t(df_13), center = FALSE, scale = daily_maximums))

scaled_data2 <- df_13/matrix(daily_maximums, ncol = ncol(df_13), nrow = nrow(df_13), byrow = FALSE)

#check both methods give the same result
sum(scaled_data - scaled_data2)

####################################################################
#TIDY, MERGE and AGGREGATE FOR MEAN DAILY PROFILES PER SUBSTATION

#add substation name and date back with an index to merge on
January_2013$index <- 1:nrow(January_2013)

scaled_data2 <- as.data.frame(scaled_data2)
scaled_data2$index <- 1:nrow(scaled_data2)

#move index column of scaled data2 to beginning to subset and merge
scaled_data2 <- scaled_data2 %>% select(index, everything())
January_2013 <- January_2013 %>% select(index, everything())
#merge scaled data with substation and date labels by index column
January_2013_scaled <- merge(scaled_data2, January_2013[,c(1:3)], by='index')
January_2013_scaled <- January_2013_scaled %>% select(Date, Substation, everything())

#aggregate and average by substation
January_2013_s_stav <- aggregate(January_2013_scaled[,4:147], by = list(January_2013_scaled$Substation), FUN = mean, na.rm =TRUE)
#rename column
colnames(January_2013_s_stav)[colnames(January_2013_s_stav) == 'Group.1'] <- 'Substation'


###################################OLD CODE ####################################
#find daily average scaled demands

#create a distance matrix - just on the demand numbers not inlucluding substation
dm<- dist(as.matrix(January_2013_s_stav[,2:145]))
#prepare hierarchical cluster
hc = hclust(dm)
#dendogram
plot(hc, hang = -1) #hang place the labels all at the same level

#cut the dendrogram to give 9 clusters 
clusters9 <-cutree(hc, 9)

#view cluster allocation
table(January_2013_s_stav$Substation, clusters9)  #crosstab of 9 cluster cut by Substation
table(clusters9)

#QUESTION 2.iii)  a) For each cluster (facet) plot the demand for 1) all days, 2) weekdays 3) saturdays 4) sundays
#recombine the cluster labels with the original dataset

January_2013_stav_clusters <- cbind(January_2013_s_stav, clusters9)

#move cluster label to beginning
January_2013_stav_clusters <- January_2013_stav_clusters %>% select(clusters9, everything())

#make data long to be able to plot
January_2013_stav_clusters_long <- reshape(January_2013_stav_clusters, direction = 'long',
                                           varying = list(names(January_2013_stav_clusters)[3:146]), v.names = 'demand')

#FINAL PLOT
p_alldays <- ggplot(January_2013_stav_clusters_long, aes(x = time, y = demand, colour = factor(clusters9)))+
  geom_smooth()+
  ggtitle("Daily average demand by cluster group")+
  facet_wrap(~clusters9)

#See how the points vary

p_alldays2 <- ggplot(January_2013_stav_clusters_long, aes(x = time, y = demand, colour = factor(clusters9)))+
  geom_point()+
  ggtitle("Daily average demand by cluster group")+
  facet_wrap(~clusters9)
p_alldays2


##APPLY SPline smoothing with df=48 so smoothed by every half hour
library(splines2)
theme_set(theme_minimal())
p_alldays3 <- ggplot(January_2013_stav_clusters_long, aes(x = time, y = demand, colour = factor(clusters9)))+
  geom_smooth(method= lm, formula = y~ splines2::bSpline(x, df =48))+
  ggtitle("Daily average demand by cluster group")+
  facet_wrap(~clusters9)
p_alldays3+theme(legend.position = 'none')



#########################################################################
##QU 2 iii) Plot weekdays, saturdays and sundays

#rename column
#colnames(January_2013_s_stav)[colnames(January_2013_s_stav) == 'Group.1'] <- 'Substation'

#merge data with cluster labels back onto original dataset to have date-substation pairs
Jan13_clust_date <- merge(January_2013_scaled, January_2013_stav_clusters[c("Substation","clusters9")], by="Substation", all=FALSE)

#move cluster label to beginning
Jan13_clust_date <- Jan13_clust_date %>% select(clusters9, everything())

#make data long to be able to plot
Jan13_clust_date_long <- reshape(Jan13_clust_date, direction = 'long',
                                           varying = list(names(Jan13_clust_date)[5:148]), v.names = 'demand')

#create day of the week column
Jan13_clust_date_long$day_of_week <- weekdays(Jan13_clust_date_long$Date)
Jan13_clust_date_long$day_of_week[Jan13_clust_date_long$day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')] <- 'Weekday'

theme_set(theme_minimal())
p_dayofweeks <- ggplot(Jan13_clust_date_long, aes(x = time, y = demand, group = day_of_week, colour = day_of_week))+
  geom_smooth(method= lm, formula = y~ splines2::bSpline(x, df =48))+
  ggtitle("Daily average demand by cluster group for different days of week")+
  facet_wrap(~clusters9)
p_dayofweeks




ob#################################################################################

## QU 2.4
#produce summaries of the variables in characteristics and describe clusters
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==1))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==2))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==3))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==4))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==5))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==6))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==7))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==8))
summary(subset(Characteristics, Jan13_clust_date_long$clusters9==9))

#find mean values for each cluster
Characteristics2 <- Characteristics %>% rename(Substation = SUBSTATION_NUMBER) 
final_data <- merge(January_2013_stav_clusters[c("Substation", "clusters9")], Characteristics2, by = "Substation", all=FALSE)
aggregate(final_data[,4:7], by=list(clusters9=final_data$clusters9), mean)

################################################################################

##QU 3 Allocate New Substations
# qu3 i) plot for each new substation all days, weekdays, saturdays and sundays

#scale the daily measurements
Newsubstations <- read.csv("NewSubstations.csv", stringsAsFactors = FALSE)
Newsubstations <- as.data.frame(Newsubstations)
Newsub_scale <- Newsubstations[,3:146]/apply(Newsubstations[,3:146],1,max)

Newsub_scale <- cbind(Newsubstations[,1:2], Newsub_scale)

#convert date column to date format
library(lubridate)
Newsub_scale$Date <- ymd(Newsub_scale$Date)
Newsub_scale_day <- Newsub_scale
Newsub_scale_day$day_of_week <- weekdays(Newsub_scale$Date)


#reshape to long to plot
Newsub_scale_long <- reshape(Newsub_scale_day, direction = 'long', varying = list(names(Newsub_scale)[3:146]), v.names = 'demand')
Newsub_scale_long$day_of_week[Newsub_scale_long$day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')] <- 'Weekday'

#All days

theme_set(theme_minimal())
newss_alldays <- 
  ggplot(Newsub_scale_long, aes(x = time, y= demand))+
  geom_smooth(method = lm, formula = y~splines2::bSpline(x, df = 48))+
  ggtitle('Daily demand for newsubstations for all days')+
  facet_wrap(~Substation)
newss_alldays

##Day Comparison PLOT
theme_set(theme_minimal())
newss_weekdays <- ggplot(Newsub_scale_long, aes(x=time, y = demand, group = day_of_week, colour = day_of_week))+
  geom_smooth(method = lm, formula = y~splines2::bSpline(x, df = 48))+
  ggtitle('Daily demand for new substations for different days of the week')+
  facet_wrap(~Substation)
newss_weekdays
newss_weekdays +theme(legend.position = 'none')

##qu 3 ii)
library(cluster)
library(factoextra)
library(rdist)

Newsub_scale_daily <- aggregate(Newsub_scale, by = list(Newsub_scale$Substation), FUN = 'mean')

#Do kmeans via cclust for 9 clusters

library(cclust)

set.seed(612)
kmeanscc <- cclust(as.matrix(January_2013_scaled[,4:147]),9,dist="euclidean",verbose=TRUE,method="kmeans")
kmeanscc$cluster

#Allocated new subsatations to a clusters

newcc <- predict(kmeanscc,as.matrix(Newsub_scale_daily[,4:147]))
newcc$cluster

###MERGE THE KMEANS CLUSTERS BACK ONTO JANUARY2013 spreadsheet
#plot the new clusters and compare with qu3 i

#merge cluster numbers back onto datasets
Newsubstations_ccluster <- data.frame(Newsubstations, newcc$cluster)

January_2013_ccluster <- data.frame(January_2013, kmeanscc$cluster)

#plot main dataset to see profiles of clusters and compare with plots from 3i to see how they compare
#cluster column called kmeanscc.cluster / data in col 5:148

#move cluster label to beginning
January_2013_ccluster <- January_2013_ccluster %>% select(kmeanscc.cluster, everything()) %>% rename(c_cluster = kmeanscc.cluster)

#make data long to be able to plot
January_2013_ccluster_long <- reshape(January_2013_ccluster, direction = 'long',
                                           varying = list(names(January_2013_ccluster)[5:148]), v.names = 'demand')

#plot demand profiles
##APPLY SPline smoothing with df=48 so smoothed by every half hour
library(splines2)
theme_set(theme_minimal())
p_cclust_all <- ggplot(January_2013_ccluster_long, aes(x = time, y = demand, colour = c_cluster))+
  geom_smooth(method= lm, formula = y~ splines2::bSpline(x, df =48))+
  ggtitle("Daily average demand by k-means cluster group")+
  facet_wrap(~c_cluster)
p_cclust_all <- p_cclust_all+theme(legend.position = 'none')

#view new substations and clustered substations together to see if allocated to relevant clusters
library(gridExtra)
grid.arrange(newss_weekdays, p_cclust_all, ncol=1, nrow=2)

#looks like a good allocation
