
set.seed(123)

#import data
clustering_raw <- read.csv( "https://raw.githubusercontent.com/Miki-Mal/2020L-WUM/master/Prace_domowe/Praca_domowa6/clustering_R3.csv", header = 1)

#prepere data
clustering <- as.data.frame( scale(clustering_raw)) # standardize variables

d <- dist(clustering, method = "euclidean") # distance matrix

library(plotly)
#look at data
plot_ly( data = clustering_raw, x=~X1, y=~X2, z=~X3)

MASS::parcoord( clustering_raw)

#by looking there shold be k =4










factoextra::fviz_nbclust( clustering, kmeans, method = "wss", k.max = k_max)
factoextra::fviz_nbclust( clustering, kmeans, method = "silhouette", k.max = k_max)


gap_stat <- cluster::clusGap(clustering, FUN = kmeans, nstart = 25,
                    K.max = k_max, B = 50)
factoextra::fviz_gap_stat(gap_stat)

#k = 8

#kmeans
km_8 <- kmeans(clustering, 8, nstart = 100) # 8 cluster solution
plot_ly( data = clustering_raw, x=~X1, y=~X2, z=~X3, color = km_8$cluster)

MASS::parcoord( clustering_raw, col = km_8$cluster)











hc_ward_fun_single <- function(x,k){
  out <- list()
  
  d <- dist(x, method = "euclidean")
  hc_ward <- hclust(d, method="single")
  out$cluster <- cutree(hc_ward, k=k)
  
  return(out)
}

factoextra::fviz_nbclust( clustering, hc_ward_fun_single, method = "wss", k.max = k_max)
factoextra::fviz_nbclust( clustering, hc_ward_fun_single, method = "silhouette", k.max = k_max)


gap_stat <- cluster::clusGap(clustering, FUN = hc_ward_fun_single,
                             K.max = k_max, B = 50)
factoextra::fviz_gap_stat(gap_stat)

k = 4

# Hierarchical Agglomerative single
hc_single <- hclust(d, method="single")
hc_single_4 <- cutree(hc_ward, k=4) # cut tree into 4 clusters

plot_ly( data = clustering_raw, x=~X1, y=~X2, z=~X3, color = hc_single_4, colors = rainbow( 8))

MASS::parcoord( clustering_raw, col = hc_single_4)



#kmeans 4 cluster
km_4 <- kmeans(clustering, 4, nstart = 100) # 4 cluster solution
plot_ly( data = clustering_raw, x=~X1, y=~X2, z=~X3, color = km_4$cluster)

MASS::parcoord( clustering_raw, col = km_4$cluster)








Silhouette <- c( 
  summary(cluster::silhouette( km_4$cluster, d))$avg.width,
  summary(cluster::silhouette( hc_single_4, d))$avg.width
)

Dunn <- c(
  clValid::dunn( distance = d, km_4$cluster),
  clValid::dunn( distance = d, hc_single_4)
)

Connectivity <- c(
  clValid::connectivity( distance = d, km_4$cluster),
  clValid::connectivity( distance = d, hc_single_4)
)

data.frame(
  Dunn, Connectivity, Silhouette, 
  row.names = c("kmean", "hclust")
)


