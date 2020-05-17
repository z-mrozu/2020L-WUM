set.seed(123)

#import data
clustering_raw <- read.csv( "clustering.csv", header = 0)

#prepere data
clustering_raw <- na.omit(clustering_raw) # listwise deletion of missing
clustering <- scale(clustering_raw) # standardize variables

#look at data
plot( clustering)
#no significant outlires

#max predicted number of clusters sqrt of number of points
k_max <- round( sqrt( dim( clustering)[1]))


#kmeans

# Determine number of clusters
wss <- rep(0, k_max)

for (i in 1:k_max) wss[i] <- sum(kmeans(clustering, centers=i)$withinss)

plot(1:k_max, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
#nstart 10 -> 100 for better performacne
#good becouse group of poits have "center"
#at 8 they clusters have the same size

km_5 <- kmeans(clustering, 5, nstart = 100) # 5 cluster solution
plot( clustering[,1:2], col=km_5$cluster,
      main = "K-Means 5 groups")
points( km_5$centers, pch=4, cex=2)

km_6 <- kmeans(clustering, 6, nstart = 100) # 6 cluster solution
plot( clustering[,1:2], col=km_6$cluster,
      main = "K-Means 6 groups")
points( km_6$centers[,1:2], pch=4, cex=2)

km_7 <- kmeans(clustering, 7, nstart = 100) # 7 cluster solution
plot( clustering[,1:2], col=km_7$cluster,
      main = "K-Means 7 groups")
points( km_7$centers[,1:2], pch=4, cex=2)

km_8 <- kmeans(clustering, 8, nstart = 100) # 8 cluster solution
plot( clustering[,1:2], col=km_8$cluster,
      main = "K-Means 8 groups")
points( km_8$centers[,1:2], pch=4, cex=2)

d <- dist(clustering, method = "euclidean") # distance matrix

# Hierarchical Agglomerative single
hc_single <- hclust(d, method="single")

hc_single_5 <- cutree(hc_single, k=5) # cut tree into 5 clusters
plot( clustering[,1:2], col=hc_single_5,
      main = "Hierarchical 5 groups method single")

hc_single_6 <- cutree(hc_single, k=6) # cut tree into 6 clusters
plot( clustering[,1:2], col=hc_single_6,
      main = "Hierarchical 6 groups method single")

hc_single_7 <- cutree(hc_single, k=7) # cut tree into 7 clusters
plot( clustering[,1:2], col=hc_single_7,
      main = "Hierarchical 7 groups method single")

hc_single_8 <- cutree(hc_single, k=8) # cut tree into 8 clusters
plot( clustering[,1:2], col=hc_single_8,
      main = "Hierarchical 8 groups method single")


# Hierarchical Agglomerative complete
hc_complete <- hclust(d, method="complete")

hc_complete_5 <- cutree(hc_complete, k=5) # cut tree into 5 clusters
plot( clustering[,1:2], col=hc_complete_5,
      main = "Hierarchical 5 groups method complete")

hc_complete_6 <- cutree(hc_complete, k=6) # cut tree into 6 clusters
plot( clustering[,1:2], col=hc_complete_6,
      main = "Hierarchical 6 groups method complete")

hc_complete_7 <- cutree(hc_complete, k=7) # cut tree into 7 clusters
plot( clustering[,1:2], col=hc_complete_7,
      main = "Hierarchical 7 groups method complete")

hc_complete_8 <- cutree(hc_complete, k=8) # cut tree into 8 clusters
plot( clustering[,1:2], col=hc_complete_8,
      main = "Hierarchical 8 groups method complete")


# Hierarchical Agglomerative ward
hc_ward <- hclust(d, method="ward.D")

hc_ward_5 <- cutree(hc_ward, k=5) # cut tree into 5 clusters
plot( clustering[,1:2], col=hc_ward_5,
      main = "Hierarchical 5 groups method ward")

hc_ward_6 <- cutree(hc_ward, k=6) # cut tree into 6 clusters
plot( clustering[,1:2], col=hc_ward_6,
      main = "Hierarchical 6 groups method ward")

hc_ward_7 <- cutree(hc_ward, k=7) # cut tree into 7 clusters
plot( clustering[,1:2], col=hc_ward_7,
      main = "Hierarchical 7 groups method ward")

hc_ward_8 <- cutree(hc_ward, k=8) # cut tree into 8 clusters
plot( clustering[,1:2], col=hc_ward_8,
      main = "Hierarchical 8 groups method ward")

# DBSCAN
library(dbscan)
#good with outliers
#not baesed on centers
#pick nuber of cluusters by himself

dbscan_list_clusters <- list()
for (i in 0:5) {
  eps <- i*0.02 + 0.15
  dbs <- dbscan( d, eps)
  
  plot( clustering[,1:2], col=dbs$cluster,
        main = paste0("DBSCAN eps=", eps))
  #0 color are outliers
  outliers <- clustering[,1:2][which( dbs$cluster==0),]
  points( outliers, pch=4, cex=1)
  
  dbscan_list_clusters[[i+1]] <- dbs$cluster
}

hc_complete_list_clusters <- list( hc_complete_5, hc_complete_6, hc_complete_7, hc_complete_8)
hc_single_list_clusters <- list( hc_single_5, hc_single_6, hc_single_7, hc_single_8)
hc_ward_list_clusters <- list( hc_ward_5, hc_ward_6, hc_ward_7, hc_ward_8)
km_list_clusters <- list( km_5$cluster, km_6$cluster, km_7$cluster, km_8$cluster)


#metric

library(ggplot2)
library( clValid)

df_col <- data.frame(
  groups = rep( 5:8, 4),
  algorithm = rep( c("hc_complete", "hc_single", "hc_ward", "kmeans"), each=4)
)

#connectivity
Connectivity <- c(
  unlist( lapply( hc_complete_list_clusters, function(x){connectivity( distance = d, clusters = x)})),
  unlist( lapply( hc_single_list_clusters, function(x){connectivity( distance = d, clusters = x)})),
  unlist( lapply( hc_ward_list_clusters, function(x){connectivity( distance = d, clusters = x)})),
  unlist( lapply( km_list_clusters, function(x){connectivity( distance = d, clusters = x)}))
)

#dunn index
Dunn_index <- c(
  unlist( lapply( hc_complete_list_clusters, function(x){dunn( distance = d, clusters = x)})),
  unlist( lapply( hc_single_list_clusters, function(x){dunn( distance = d, clusters = x)})),
  unlist( lapply( hc_ward_list_clusters, function(x){dunn( distance = d, clusters = x)})),
  unlist( lapply( km_list_clusters, function(x){dunn( distance = d, clusters = x)}))
)
  
#Silhouette Width
silhouette_width <- function(x){summary( silhouette( x, dist = d))$avg.width}

Silhouette_width <- c(
  unlist( lapply( hc_complete_list_clusters, silhouette_width)),
  unlist( lapply( hc_single_list_clusters, silhouette_width)),
  unlist( lapply( hc_ward_list_clusters, silhouette_width)),
  unlist( lapply( km_list_clusters, silhouette_width))
)

dbscan_metric_df <- data.frame(
  groups = unlist(lapply(dbscan_list_clusters, max)),
  algorithm = c( "DBSCAN eps=1.5", "DBSCAN eps=1.7", "DBSCAN eps=1.9", "DBSCAN eps=2.1", "DBSCAN eps=2.3", "DBSCAN eps=2.5"),
  Connectivity = unlist(lapply(dbscan_list_clusters, function(x){connectivity( distance = d, clusters = x)})),
  Dunn_index = unlist(lapply(dbscan_list_clusters, function(x){dunn( distance = d, clusters = x)})),
  Silhouette_width =unlist(lapply(dbscan_list_clusters, silhouette_width))
)

df_col <- cbind( df_col, Connectivity, Dunn_index, Silhouette_width)
df_metric <- rbind( df_col, dbscan_metric_df)


#Connectivity
ggplot( data = df_metric, aes( x = groups, y = Connectivity, color = algorithm))+
  geom_point()+
  geom_line()

#dunn index
ggplot( data = df_metric, aes( x = groups, y = Dunn_index, color = algorithm))+
  geom_point()+
  geom_line()

#Silhouette Width
ggplot( data = df_metric, aes( x = groups, y = Silhouette_width, color = algorithm))+
  geom_point()+
  geom_line()













