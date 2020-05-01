
#library used for knn
library( class)

#load
x_test <- read.csv( "x_test.csv")[-1]
x_train <- read.csv( "x_train.csv")[-1]
y_test <- read.csv( "y_test.csv")[-1]
y_train <- read.csv( "y_train.csv")[-1]

#normaliacion to ~(0,1)
for( coli in 1:dim(x_train)[2]){
  #x and y have o be noemalized in the same way
  c_min <- min( c(x_train[,coli], x_test[,coli]))
  c_max <- max( c(x_train[,coli], x_test[,coli]))
  
  x_train[,coli] <- (x_train[,coli] - c_min) / (c_max - c_min)
  x_test[,coli] <- (x_test[,coli] - c_min) / (c_max - c_min)
}

#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

##looking for best k = number of groups
n <- sqrt( dim(x_train)[1]) #arbitrary number for top number of groups
acc <- rep(0,n)
for(k in 1:n){
  test_pred <- knn( train = x_train[1:800,], test = x_test, cl = y_train[1:800,], k=k)
  
  tab <-  table( y_test[,1], test_pred)
  acc[k] <- accuracy(tab)
}
k=1:n #for plot
#k vs. accuracy
plot( k, acc, type = "l")

#best k
best_k <- which.max( acc)

test_pred <- knn( train = x_train[1:800,], test = x_test, cl = y_train[1:800,], k=best_k)
tab <-  table( y_test[,1], test_pred)

best_k
accuracy( tab)
tab

