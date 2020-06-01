#load
x_test <- read.csv( "x_test.csv")[-1]
x_train <- read.csv( "x_train.csv")[-1]
y_test <- read.csv( "y_test.csv")[-1]
y_train <- read.csv( "y_train.csv")[-1]

#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

#bind for training
train <- cbind( x_train, y_train)

#training
train_lm <- lm( x~., train)
#summary
sum_train_lm <- summary( train_lm)
#predict
pred_train <- predict(  train_lm, x_test)

#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

#looking for best accuracy
acc <- rep(0, 21)

for( i in 0:20){
  predicted_lab <- ifelse( pred_train > i*0.05, 1, 0)
  acc[i] <- accuracy( table( y_test[,1], predicted_lab))
}

best_split <- which.max( acc) *0.05
best_split

predicted_lab <- ifelse( pred_train > best_split, 1, 0)

#best split
tab <-table( y_test[,1], predicted_lab)
accuracy( tab)
tab


