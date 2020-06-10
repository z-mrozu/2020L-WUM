library(mclust)
library(dplyr)
library(genie)
library(RSpectra)
library(stringi)
library(genie)
library(cluster)
library(mlr)
library(ddpcr)


suma_poza_klastrem <- function(X, y){
  uy <- unique(y)
  
  A <- dist(X, upper=TRUE) %>% as.matrix
  
  for(i in uy){
    A[y==i, y==i] <- 0
  }
  
  sum(A)
}


suma_wewnatrz_klastra <- function(X, y){
  uy <- unique(y)
  
  A <- dist(X, upper=TRUE) %>% as.matrix
  
  out <- 0
  for(i in uy){
    out <- out + sum(A[y==i, y==i])
  }
  
  out
}

activity_labels <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")



# chunk1
train_raw <- read.csv(file = "data/train.csv", header = TRUE, sep = ",")
test_raw <- read.csv(file = "data/test.csv", header = TRUE, sep = ",")
label_train <- read.csv(file = "data/y_train.txt", header = FALSE, sep = ";")
label_test <- read.csv(file = "data/y_test.txt", header = FALSE, sep = ";")
subjectID_train <- read.csv(file = "data/subject_train.txt", header = FALSE, sep = ";")
subjectID_test <- read.csv(file = "data/subject_test.txt", header = FALSE, sep = ";")

quiet( library(dplyr) )
quiet( library(ggplot2) )

train <- train_raw
test <- test_raw

train$label <- as.factor(label_train$V1)
test$label <- as.factor(label_test$V1)
train$subjectID <- as.factor(subjectID_train$V1)
test$subjectID <- as.factor(subjectID_test$V1)
train$partition <- as.factor("train")
test$partition <- as.factor("test")

full_data <- rbind(train, test)

full_data_do_PCA <- full_data %>% dplyr::select(X1:X561)
full_data_PCA <- prcomp(full_data_do_PCA, scale. = TRUE, center = TRUE)


# chunk2
full_data_PCA$x %>% as.data.frame() %>% 
  mutate(czynnosc = activity_labels[full_data$label]) %>%
  ggplot(aes(x=PC1, y=PC2, color = czynnosc)) +
  geom_point() +
  ggtitle("PCA - pierwsza oraz druga kolumna")


# chunk3
full_data_PCA$x %>% as.data.frame() %>% 
  mutate(czynnosc = activity_labels[full_data$label]) %>%
  ggplot(aes(x=PC1, y=PC4, color = czynnosc)) +
  geom_point() +
  ggtitle("PCA - pierwsza oraz czwarta kolumna")


# tsne
library(tsne)

ktore <- sample(1:10299, 1000) # losowanie 1000 wierszy ze wszystkich

tsne_data <- as.data.frame(full_data_PCA$x[ktore, 1:30])
tsne_labels <- full_data$label[ktore]

tsne_punkty <- tsne(tsne_data)
tsne_punkty <- as.data.frame(tsne_punkty) %>% mutate(czynnosc = activity_labels[tsne_labels])
ggplot(data = tsne_punkty, aes(x = V1, y = V2, color = czynnosc)) +
  geom_point()

# chunk4
train_do_PCA <- train %>% dplyr::select(X1:X561)
test_do_PCA <- test %>% dplyr::select(X1:X561)

train_pca <- prcomp(train_do_PCA, scale. = FALSE, center = FALSE)

test_pca <- as.data.frame(as.matrix(test_do_PCA) %*% train_pca$rotation)

dane <- test_pca[, 1:100]


# chunk4b
data <- dane

sil_scores <- numeric(15)
scores_elbow <- numeric(15)
kmeans_klaster <- matrix(nrow=dim(dane)[1], ncol=15)

task <- makeClusterTask(data = data)
for(k in 2:15){
  learner <- makeLearner("cluster.kmeans", centers = k)
  model <- train(learner, task)
  pred <- predict(model, task)
  
  scores_elbow[k] <- suma_wewnatrz_klastra(dane, pred$data$response)
  Sil <- silhouette(x = pred$data$response, dist = dist(dane))
  sil_scores[k] <- (sum(Sil[,3]))/dim(Sil)[1]
  
  kmeans_klaster[,k] <- pred$data$response
}


# chunk4c
scores_elbow <- scores_elbow[-1]
tmp <- data.frame(scores_elbow/1000000) %>% mutate(k = row_number()+1)
colnames(tmp) <- c("scores_milions", "k")

tmp %>% 
  ggplot(aes(x=k, y=scores_milions)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  geom_vline(xintercept = 6, linetype="dashed", color = "red") +
  ylab("score w milionach") +
  ggtitle("Metoda łokcia dla clusteringu k-means.")

sil_scores <- sil_scores[-1]
tmp <- data.frame(sil_scores) %>% mutate(k = row_number()+1)
colnames(tmp) <- c("scores", "k")

tmp %>%
  ggplot(aes(x=k, y=scores)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  geom_vline(xintercept = 6, linetype="dashed", color = "red") +
  ylab("score") +
  ggtitle("Metoda silhouette dla clusteringu k-means.")


# unnamed 3
data %>% mutate(klastry = as.factor(kmeans_klaster[,7])) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=klastry)) +
  geom_density_2d() +
  ggtitle("Gęstość dla 7 klastrów według kmeans")


# clusterkmeans6
set.seed(4321)

data <- dane

task <- makeClusterTask(data = data)
learner <- makeLearner("cluster.kmeans", centers = 6)
model <- train(learner, task)

pred <- predict(model, task)


# unnamed 4
data %>% mutate(klastry = as.factor(pred$data$response)) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=klastry)) +
  geom_point() +
  ggtitle("podział wygenerowany")


# unnamed 5
data %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=czynnosc)) +
  geom_point() +
  ggtitle("podział oryginalny")


# unnamed 6
data %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=czynnosc)) +
  geom_point() +
  ggtitle("podział oryginalny")


# chunk50a
Sil <- silhouette(x = pred$data$response, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]


# chunk51a
quiet( library(clusterSim) )
index.DB(x = dane, cl = pred$data$response)$DB


# chunk52a}
quiet( library(clValid) )
dunn(dist(dane), pred$data$response)


# chunk53a}
quiet( library(labelled))
FM_index(pred$data$response, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$data$response, label_test$V1)


# unnamed
dane <- test_pca[,1:100]

sil_scores_g <- numeric(15)
scores <- numeric(15)
genie_klaster <- matrix(nrow=dim(dane)[1], ncol=15)
for(k in 2:15){
  tmp <- cutree(hclust2(dist(dane)), k = k)
  scores[k] <- suma_wewnatrz_klastra(dane, tmp)
  Sil <- silhouette(x = tmp, dist = dist(dane))
  sil_scores_g[k] <- (sum(Sil[,3]))/dim(Sil)[1]
  genie_klaster[,k] <- tmp
}


# unnamed 
scores <- scores[-1]
tmp <- data.frame(scores/1000000) %>% mutate(k = row_number()+1)
colnames(tmp) <- c("scores_milions", "k")

tmp %>% 
  ggplot(aes(x=k, y=scores_milions)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  geom_vline(xintercept = 6, linetype="dashed", color = "red") +
  ylab("score w milionach") +
  ggtitle("Metoda łokcia dla clusteringu genie.")

sil_scores_g <- sil_scores_g[-1]
tmp <- data.frame(sil_scores_g) %>% mutate(k = row_number()+1)
colnames(tmp) <- c("scores", "k")

tmp %>%
  ggplot(aes(x=k, y=scores)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  geom_vline(xintercept = 6, linetype="dashed", color = "red") +
  ylab("score") +
  ggtitle("Metoda silhouette dla clusteringu genie.")


# unnamed 
pred <- cutree(hclust2(dist(dane)), k = 6)


# unnamed
data %>% mutate(klastry = as.factor(pred)) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=klastry)) +
  geom_point() +
  ggtitle("podział wygenerowany")

# unnamed
data %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>% 
  ggplot(aes(x=PC3, y=PC2, colour=czynnosc)) +
  geom_point() +
  ggtitle("podział oryginalny")


# chunk50b}
Sil <- silhouette(x = pred, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]


# chunk51b}
index.DB(x = dane, cl = pred)$DB

# chunk52b}
dunn(dist(dane), pred)


# chunk53b}
FM_index(pred, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred, label_test$V1)

# chunk11}
na_dwa <- pam(dane, 2)$clustering

dane %>% as.data.frame() %>% 
  mutate(klaster = as.factor(na_dwa)) %>%
  ggplot(aes(x=PC1, y=PC2, color = klaster)) +
  geom_point() +
  ggtitle("PAM podział na 2 klastry")

# chunk12}
dane %>% as.data.frame() %>% 
  mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC1, y=PC2, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na czynności")


# chunk13}
silhouette_score_jeden <- numeric(15)
pam_klaster_jeden <- matrix(nrow=((na_dwa==1) %>% sum), ncol=15)

for(k in 2:15){
  tmp <- pam(dane[na_dwa==1,], k)$clustering
  Sil <- silhouette(x = tmp, dist = dist(dane[na_dwa==1,]))
  silhouette_score_jeden[k] <- (Sil[,3] %>% sum())/dim(Sil)[1]
  
  pam_klaster_jeden[,k] <- tmp
}

# chunk14}
silhouette_score_jeden <- silhouette_score_jeden[-1] %>% data.frame()
colnames(silhouette_score_jeden) <- "score"
mutate(silhouette_score_jeden, k = row_number()+1) %>% 
  ggplot(aes(x=k, y=score)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  ggtitle("PAM silhouette dla pierwszego zbioru")

# chunk15}
p1 <- dane[na_dwa==1,] %>% as.data.frame() %>% 
  mutate(klaster = as.factor(pam_klaster_jeden[,3])) %>%
  ggplot(aes(x=PC1, y=PC2, color = klaster)) +
  geom_point() +
  ggtitle("Otrzymany podział dla pierwszego zbioru")

p2 <- (dane)[na_dwa==1,] %>% as.data.frame() %>% 
  mutate(czynnosc = as.factor(activity_labels[(label_test$V1)[na_dwa==1]])) %>%
  ggplot(aes(x=PC1, y=PC2, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalne czynności dla pierwszego zbioru")


grid.arrange(p1, p2, ncol=1)



# chunk17}
silhouette_score_dwa <- numeric(10)
pam_klaster_dwa <- matrix(nrow=((na_dwa==2) %>% sum), ncol=10)

for(k in 2:10){
  tmp <- pam(dane[na_dwa==2,], k)$clustering
  Sil <- silhouette(x = tmp, dist = dist(dane[na_dwa==2,]))
  silhouette_score_dwa[k] <- (Sil[,3] %>% sum())/dim(Sil)[1]
  
  pam_klaster_dwa[,k] <- tmp
}


# chunk18}
silhouette_score_dwa <- silhouette_score_dwa[-1] %>% data.frame()
colnames(silhouette_score_dwa) <- "score"
mutate(silhouette_score_dwa, k = row_number()+1) %>% 
  ggplot(aes(x=k, y=score)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10) +
  ggtitle("PAM silhouette dla drugiego")


# chunk19}
p1 <- (dane)[na_dwa==2,] %>% as.data.frame() %>% 
  mutate(klaster = as.factor(pam_klaster_dwa[,3])) %>%
  ggplot(aes(x=PC1, y=PC2, color = klaster)) +
  geom_point() +
  ggtitle("Otrzymany podział dla pierwszego zbioru")

p2 <- (dane)[na_dwa==2,] %>% as.data.frame() %>% 
  mutate(czynnosc = as.factor(activity_labels[(label_test$V1)[na_dwa==2]])) %>%
  ggplot(aes(x=PC1, y=PC2, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalne czynności dla drugiego zbioru")

grid.arrange(p1, p2, ncol=1)


# chunk20}
train_do_PCA <- train %>% dplyr::select(X1:X561)
test_do_PCA <- test %>% dplyr::select(X1:X561)

train_pca <- prcomp(train_do_PCA, scale. = FALSE, center = FALSE)

test_pca <- as.data.frame(as.matrix(test_do_PCA) %*% train_pca$rotation)


# chunk21, dependson="chunk20"}
quiet( library(ClusterR) )
quiet( library(clValid) )
quiet( library(labelled) )

dane <- test_pca[1:100]

# łokieć z miarą BIC - Bayesian information criterion
opt_gmm <- Optimal_Clusters_GMM(dane, max_clusters = 15, criterion = "BIC", dist_mode = "eucl_dist",
                                seed_mode = "random_spread", km_iter = 10, em_iter = 10,
                                var_floor = 1e-10, plot_data = T)


# chunk22, dependson="chunk20"}
silhouette_score <- numeric(15)
pam_klaster <- matrix(nrow=2947, ncol=15)
for(k in 2:15){
  gmm <- GMM(dane, k, dist_mode = "eucl_dist", seed_mode = "random_spread", km_iter = 10, em_iter = 10, verbose = F, seed=420)
  tmp <- predict_GMM(dane, gmm$centroids, gmm$covariance_matrices, gmm$weights)$cluster_labels
  Sil <- silhouette(x = tmp, dist = dist(dane))
  silhouette_score[k] <- (Sil[,3] %>% sum())/dim(Sil)[1]
  
  pam_klaster[,k] <- tmp
}


# chunk23, dependson="chunk20"}
silhouette_score <- silhouette_score[-1] %>% data.frame()
colnames(silhouette_score) <- "score"
mutate(silhouette_score, k = row_number()+1) %>% 
  ggplot(aes(x=k, y=score)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  geom_vline(xintercept = 6, linetype="dashed", color = "red") +
  ggtitle("GMM silhouette")


# chunk24}
gmm <- GMM(dane, 6, dist_mode = "eucl_dist", seed_mode = "random_spread", km_iter = 10, em_iter = 10, verbose = F, seed=420)
pred <- predict_GMM(dane, gmm$centroids, gmm$covariance_matrices, gmm$weights)


# chunk25}
p1 <- dane %>% mutate(klaster = as.factor(pred$cluster_labels)) %>%
  ggplot(aes(x=PC2, y=PC3, color = klaster )) +
  geom_point() +
  ggtitle("GMM podział na 6 klastrów")
p2 <- dane %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC2, y=PC3, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na 6 czynności")

grid.arrange(p1, p2, ncol=1)


# chunk26}
Sil <- silhouette(x = pred$cluster_labels, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]


# chunk27}
index.DB(x = dane, cl = pred$cluster_labels)$DB


# chunk28}
dunn(dist(dane), pred$cluster_labels)

# chunk29}
FM_index(pred$cluster_labels, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$cluster_labels, label_test$V1)


# chunk30}
gmm <- GMM(dane[, 1:90], 6, dist_mode = "maha_dist", seed_mode = "random_spread", km_iter = 10, em_iter = 10, verbose = F, seed=420)
pred <- predict_GMM(dane[, 1:90], gmm$centroids, gmm$covariance_matrices, gmm$weights)


# chunk31}
p1 <- dane %>% mutate(klaster = as.factor(pred$cluster_labels)) %>%
  ggplot(aes(x=PC2, y=PC3, color = klaster )) +
  geom_point() +
  ggtitle("GMM podział na 6 klastrów")
p2 <- dane %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC2, y=PC3, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na 6 czynności")

grid.arrange(p1, p2, ncol=1)


# chunk32}
Sil <- silhouette(x = pred$cluster_labels, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]


# chunk33}
index.DB(x = dane, cl = pred$cluster_labels)$DB

# chunk34}
dunn(dist(dane), pred$cluster_labels)

# chunk35}
FM_index(pred$cluster_labels, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$cluster_labels, label_test$V1)

# chunk36}
gmm <- GMM(dane, 6, dist_mode = "eucl_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10, verbose = F, seed=420)
pred <- predict_GMM(dane, gmm$centroids, gmm$covariance_matrices, gmm$weights)

# chunk37}
p1 <- dane %>% mutate(klaster = as.factor(pred$cluster_labels)) %>%
  ggplot(aes(x=PC2, y=PC3, color = klaster )) +
  geom_point() +
  ggtitle("GMM podział na 6 klastrów")
p2 <- dane %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC2, y=PC3, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na 6 czynności")

grid.arrange(p1, p2, ncol=1)


# chunk38}
Sil <- silhouette(x = pred$cluster_labels, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]

# chunk39}
index.DB(x = dane, cl = pred$cluster_labels)$DB

# chunk40}
dunn(dist(dane), pred$cluster_labels)

# chunk41}
FM_index(pred$cluster_labels, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$cluster_labels, label_test$V1)

# chunk42}
gmm <- GMM(dane, 6, dist_mode = "eucl_dist", seed_mode = "static_spread", km_iter = 10, em_iter = 10, verbose = F, seed=420)
pred <- predict_GMM(dane, gmm$centroids, gmm$covariance_matrices, gmm$weights)


# chunk43}
p1 <- dane %>% mutate(klaster = as.factor(pred$cluster_labels)) %>%
  ggplot(aes(x=PC2, y=PC3, color = klaster )) +
  geom_point() +
  ggtitle("GMM podział na 6 klastrów")
p2 <- dane %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC2, y=PC3, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na 6 czynności")

grid.arrange(p1, p2, ncol=1)


# chunk44}
Sil <- silhouette(x = pred$cluster_labels, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]

# chunk46}
dunn(dist(dane), pred$cluster_labels)

# chunk47}
FM_index(pred$cluster_labels, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$cluster_labels, label_test$V1)

# chunk48}
gmm <- GMM(dane, 6, dist_mode = "maha_dist", seed_mode = "static_subset", km_iter = 10, em_iter = 10, verbose = F, seed=420)
pred <- predict_GMM(dane, gmm$centroids, gmm$covariance_matrices, gmm$weights)

# chunk49}
p1 <- dane %>% mutate(klaster = as.factor(pred$cluster_labels)) %>%
  ggplot(aes(x=PC2, y=PC3, color = klaster )) +
  geom_point() +
  ggtitle("GMM podział na 6 klastrów")
p2 <- dane %>% mutate(czynnosc = as.factor(activity_labels[label_test$V1])) %>%
  ggplot(aes(x=PC2, y=PC3, color = czynnosc)) +
  geom_point() +
  ggtitle("Oryginalny podział na 6 czynności")

grid.arrange(p1, p2, ncol=1)

# chunk50}
Sil <- silhouette(x = pred$cluster_labels, dist = dist(dane))
(sum(Sil[,3]))/dim(Sil)[1]

# chunk51}
index.DB(x = dane, cl = pred$cluster_labels)$DB

# chunk52}
dunn(dist(dane), pred$cluster_labels)

# chunk53}
FM_index(pred$cluster_labels, label_test$V1) %>% remove_attributes("E_FM") %>% remove_attributes("V_FM")
adjustedRandIndex(pred$cluster_labels, label_test$V1)








