####################################
#       Code Discription
####################################
# King County Metro Capstone - OneBusLater Team
# Muse Wu 2020 Spring Quarter
# The code uses pdc package to generate clustering of bus stops
# The code uses elbow method to evaluate the optimal number of clusters


####################################
#       Data Discription
####################################
# King County Metro South RRE Q3 raw Data
# Each row represents each day, there are total 30 days in a month
# Each column represents a bus stop


####################################
#       PDC Discription
####################################
# PDC: Permutation Distribution Clustering
# PDC formalize similarity as small relative complexity between two time series.
# The PD assigns a probability to the occurrence of certain patterns of the ranks of values in a time series
# A probabilistic interpretation allows the determination of the number of significant different clusters.


####################################
#            Library
####################################
library(tidyr)
library(dplyr)
library(pdc)
library(gridGraphics)
library(factoextra)
library(NbClust)
library(ggmap)
library(devtools)
library(rlang)
library(mapproj)
library(maps)

####################################
#        Data Preparation
####################################

# Aggregate the average load of customers at the same day in 1 bus stop
# The row is the date, column is the bus stop

data <- read.csv(file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/static_dynamic_rre_south_2019Q3_unrounded.csv", header  = TRUE)

# Calculate the average load of customers
data_agg <- data %>%
  group_by(OPD_DATE, STOP_CODE) %>%
  summarise(Load_num = mean(LOAD, na.rm = TRUE))

# Create a new empty dataset(data_date_stop) with the date as row and bus stop as column
# All the cells are filled by NA
data_date_stop <- data.frame(matrix(NA, nrow = length(unique(data_agg$OPD_DATE)), 
                                    ncol = length(unique(data_agg$STOP_CODE))))
colnames(data_date_stop) <- c(as.character(unique(data_agg$STOP_CODE)))
rownames(data_date_stop) <- c(as.character(unique(data_agg$OPD_DATE)))

# Filling up average load of customers in the new dataset(data_date_stop) by matching the date and bus stop
for (row in 1:nrow(data_agg)){
  date <- data_agg[row, "OPD_DATE"]
  row_index <- grep(date, rownames(data_date_stop))
  station <- data_agg[row, "STOP_CODE"]
  col_index <- grep(station, colnames(data_date_stop))
  load_num <- as.numeric(data_agg[row, "Load_num"])
  data_date_stop[row_index, col_index] <- load_num
}

#write.csv(data_date_stop, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/static_dynamic_rre_south_2019Q3_meanLoad.csv")


####################################
#              PDC
#       Cluster: mean_Load
####################################


# load data
#data <- read.csv(file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/static_dynamic_rre_south_2019Q3_meanLoad.csv", row.name = 1, header  = TRUE)
data <- data_date_stop

# 515 bus stop information located at the last column. Due to 515 bus stop lacks of data, clustering may not be accurated
# To exclude 515 bus stop, run the following line of code
# data <- data[, -ncol(data)]

# PDC: Auto generate dimension
# Dimension: permutation size
clustering <- pdclust(data) 
clustering

# Manual set up number of dimension
# Dimension: how many row in a experiment
# The more number of row in one group, the larger different between each time series 
clustering <- pdclust(data,4) 

# Seperate in 5 group, k = 5
# number of clustering can be modified
groups <- as.data.frame(cutree(clustering, k = 5))
# View(groups)

# clustering information
# match with the graph(see the code below)
str(clustering)

# put order and the matching bus stop in dictonary for future seaching
# order is following with the clustering plot
# label is matching with the order 
# order 1 bus stop is in the top of the clustering tree
dictionary <- data.frame()
order <- clustering['order']
order <- as.numeric(as.character(unlist(order)))
order <- data.frame(matrix(unlist(order), nrow=length(order), byrow=T))
labels <- clustering['labels']
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
dictionary <- cbind(order, labels, groups)
colnames(dictionary) <- c("orders", "labels", "groups")
dictionary[order(dictionary$groups, decreasing = FALSE),]
write.csv(dictionary, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south_load4.csv")

####################################
#         Visualization
####################################


# Graph clusters
plot(clustering, cols = dictionary$groups)

legend("topleft",
       legend = dictionary$labels,
       col = dictionary$labels,
       pch = c(17,19),
       bty = "n",
       pt.cex = 1,
       cex = 0.8,
       text.col = "black",
       horiz = F,
       xpd = TRUE)

# # Rotation
grab_grob <- function(){
  grid.echo()
  grid.grab()
}
g <- grab_grob()
grid.newpage()
pushViewport(viewport(width=0.7,angle=270))
grid.draw(g)
 
# # Graph bus stop
plot(clustering, timeseries.as.labels = FALSE, labels = names(colnames(data)))
 
 
# ####################################
# #           Evaluation
# ####################################
# 
# # Elbow method: a heuristic used in determining the number of clusters in a data set. 
# #               The method consists of plotting the explained variation as a function of the number of clusters, 
# #               and picking the elbow of the curve as the number of clusters to use.
# # FUNcluster method: kmeans, cluster::pam, cluster::clara, cluster::fanny, hcut
# # Method to be used for estimating the optimal number of clusters. Possible values are
# # "silhouette" (for average silhouette width), "wss" (for total within sum of square), "gap_stat" (for gap statistics)
# # Conclusion: All the combination of evaluation points out 4 is the best number of clustering
# # Source: https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# data_t <- t(data)
# 
# fviz_nbclust(data_t, hcut, method = "gap_stat") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# fviz_nbclust(data_t, kmeans, method = "gap_stat") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# fviz_nbclust(data_t, kmeans, method = "silhouette") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# fviz_nbclust(data_t, cluster::pam, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")


####################################
#        Output Clusters
####################################
data <- read.csv(file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/static_dynamic_rre_south_2019Q3_unrounded.csv", header  = TRUE)

label1 <- dictionary %>% filter(groups == 1) %>% select(labels)
cluster1 <- subset(data, STOP_CODE %in% label1$labels)
write.csv(cluster1, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-1.csv")

label2 <- dictionary %>% filter(groups == 2) %>% select(labels)
cluster2 <- subset(data, STOP_CODE %in% label2$labels)
write.csv(cluster2, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-2.csv")

label3 <- dictionary %>% filter(groups == 3) %>% select(labels)
cluster3 <- subset(data, STOP_CODE %in% label3$labels)
write.csv(cluster3, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-3.csv")

label4 <- dictionary %>% filter(groups == 4) %>% select(labels)
cluster4 <- subset(data, STOP_CODE %in% label4$labels)
write.csv(cluster4, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-4.csv")

label5 <- dictionary %>% filter(groups == 5) %>% select(labels)
cluster5 <- subset(data, STOP_CODE %in% label5$labels)
write.csv(cluster5, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-5.csv")

label6 <- dictionary %>% filter(groups == 6) %>% select(labels)
cluster6 <- subset(data, STOP_CODE %in% label6$labels)
write.csv(cluster6, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south_7_4-6.csv")

label7 <- dictionary %>% filter(groups == 7) %>% select(labels)
cluster7 <- subset(data, STOP_CODE %in% label7$labels)
write.csv(cluster7, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster7_4/pdc_south-7_4-7.csv")

label8 <- dictionary %>% filter(groups == 8) %>% select(labels)
cluster8 <- subset(data, STOP_CODE %in% label8$labels)
write.csv(cluster8, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster9_4/pdc_south_9_4-8.csv")

label9 <- dictionary %>% filter(groups == 9) %>% select(labels)
cluster9 <- subset(data, STOP_CODE %in% label9$labels)
write.csv(cluster9, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/pdc_south/pdc_cluster9_4/pdc_south_9_4-9.csv")


####################################
#               PDC
# Cluster: mean_TIME_ELAPSED_TRAVEL 
####################################

data <- read.csv(file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/static_dynamic_rre_south_2019Q3_unrounded.csv", header  = TRUE)

# Calculate the average load of customers
data_agg <- data %>%
  group_by(OPD_DATE, STOP_CODE) %>%
  summarise(Load_num = mean(TIME_ELAPSED_TRAVEL, na.rm = TRUE))

# Create a new empty dataset(data_date_stop) with the date as row and bus stop as column
# All the cells are filled by NA
data_date_stop <- data.frame(matrix(NA, nrow = length(unique(data_agg$OPD_DATE)), 
                                    ncol = length(unique(data_agg$STOP_CODE))))
colnames(data_date_stop) <- c(as.character(unique(data_agg$STOP_CODE)))
rownames(data_date_stop) <- c(as.character(unique(data_agg$OPD_DATE)))

# Filling up average load of customers in the new dataset(data_date_stop) by matching the date and bus stop
for (row in 1:nrow(data_agg)){
  date <- data_agg[row, "OPD_DATE"]
  row_index <- grep(date, rownames(data_date_stop))
  station <- data_agg[row, "STOP_CODE"]
  col_index <- grep(station, colnames(data_date_stop))
  load_num <- as.numeric(data_agg[row, "Load_num"])
  data_date_stop[row_index, col_index] <- load_num
}

#write.csv(data_date_stop, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/rounded60_static_dynamic_rre_south_2019Q3_meanTIME_ELAPSED_TRAVEL.csv")

# 515 bus stop information located at the last column. Due to 515 bus stop lacks of data, clustering may not be accurated
# To exclude 515 bus stop, run the following line of code
data <- data_date_stop
# data <- data[, -ncol(data)]

# PDC: Auto generate dimension
# Dimension: permutation size
# 6990 all NaN
clustering <- pdclust(data) 
clustering

# Manual set up number of dimension
# Dimension: how many row in a experiment
# The more number of row in one group, the larger different between each time series 
# clustering <- pdclust(data,8) 

# Seperate in 5 group, k = 5
# number of clustering can be modified
groups <- as.data.frame(cutree(clustering, k = 5))
#View(groups)

# clustering information
# match with the graph(see the code below)
str(clustering)

# put order and the matching bus stop in dictonary for future seaching
# order is following with the clustering plot
# label is matching with the order 
# order 1 bus stop is in the top of the clustering tree
dictionary <- data.frame()
order <- clustering['order']
order <- as.numeric(as.character(unlist(order)))
order <- data.frame(matrix(unlist(order), nrow=length(order), byrow=T))
labels <- clustering['labels']
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
dictionary <- cbind(order, labels, groups)
colnames(dictionary) <- c("orders", "labels", "groups")
dictionary[order(dictionary$groups, decreasing = FALSE),]
write.csv(dictionary, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/bus_cluster_traveltime.csv")

data_t <- t(data)

fviz_nbclust(data_t, hcut, method = "gap_stat") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


####################################
#               PDC
#   Cluster: mean_ACT_METERS 
####################################

data <- read.csv(file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/rounded60_static_dynamic_rre_south_2019Q3.csv", header  = TRUE)

# Calculate the average load of customers
data_agg <- data %>%
  group_by(OPD_DATE, STOP_CODE) %>%
  summarise(Load_num = mean(ACT_METERS, na.rm = TRUE))

# Create a new empty dataset(data_date_stop) with the date as row and bus stop as column
# All the cells are filled by NA
data_date_stop <- data.frame(matrix(NA, nrow = length(unique(data_agg$OPD_DATE)), 
                                    ncol = length(unique(data_agg$STOP_CODE))))
colnames(data_date_stop) <- c(as.character(unique(data_agg$STOP_CODE)))
rownames(data_date_stop) <- c(as.character(unique(data_agg$OPD_DATE)))

# Filling up average load of customers in the new dataset(data_date_stop) by matching the date and bus stop
for (row in 1:nrow(data_agg)){
  date <- data_agg[row, "OPD_DATE"]
  row_index <- grep(date, rownames(data_date_stop))
  station <- data_agg[row, "STOP_CODE"]
  col_index <- grep(station, colnames(data_date_stop))
  load_num <- as.numeric(data_agg[row, "Load_num"])
  data_date_stop[row_index, col_index] <- load_num
}

# 515 bus stop information located at the last column. Due to 515 bus stop lacks of data, clustering may not be accurated
# To exclude 515 bus stop, run the following line of code
data <- data_date_stop
data <- data[, -ncol(data)]

# PDC: Auto generate dimension
# Dimension: permutation size
clustering <- pdclust(data) 
clustering

# Manual set up number of dimension
# Dimension: how many row in a experiment
# The more number of row in one group, the larger different between each time series 
clustering <- pdclust(data, 4) 

# Seperate in 5 group, k = 5
# number of clustering can be modified
groups <- as.data.frame(cutree(clustering, k = 9))
#View(groups)

# clustering information
# match with the graph(see the code below)
str(clustering)

# put order and the matching bus stop in dictonary for future seaching
# order is following with the clustering plot
# label is matching with the order 
# order 1 bus stop is in the top of the clustering tree
dictionary <- data.frame()
order <- clustering['order']
order <- as.numeric(as.character(unlist(order)))
order <- data.frame(matrix(unlist(order), nrow=length(order), byrow=T))
labels <- clustering['labels']
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
labels <- data.frame(matrix(unlist(labels), nrow=length(labels), byrow=F))
dictionary <- cbind(order, labels, groups)
colnames(dictionary) <- c("orders", "labels", "groups")
dictionary[order(dictionary$groups, decreasing = FALSE),]
write.csv(dictionary, file = "C:/Users/82124/OneDrive - UW/2020Spring_INDE495/Permutation_Distribution_Clustering/bus_cluster_act_meters.csv")

plot(clustering, cols = dictionary$groups)

legend("topleft",
       legend = dictionary$labels,
       col = dictionary$labels,
       pch = c(17,19),
       bty = "n",
       pt.cex = 1,
       cex = 0.8,
       text.col = "black",
       horiz = F,
       xpd = TRUE)


# Rotation
grab_grob <- function(){
  grid.echo()
  grid.grab()
}
g <- grab_grob()
grid.newpage()
pushViewport(viewport(width=0.7,angle=270))
grid.draw(g)

# Graph bus stop
plot(clustering, timeseries.as.labels = FALSE, labels = names(colnames(data)))


data_t <- t(data)

fviz_nbclust(data_t, hcut, method = "gap_stat") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

