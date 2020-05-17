train_path <- file.path(getwd(), "UCI HAR Dataset", "train", "X_train.txt")
train_original <- read.csv2(file = train_path, header = FALSE, sep = " ", stringsAsFactors = FALSE)


# przerabia dane tak, żeby pominąć NA, gdyż są one powodem błędu
data_train <- data.frame(matrix(nrow = 7352, ncol = 561))
i_data <- 1
j_data <- 1
for(i in 1:dim(train_original)[1]){
  for(j in 1:dim(train_original)[2]){
    if(train_original[i,j] != ""){
      data_train[i_data,j_data] <- as.numeric(train_original[i,j])
      
      j_data <- j_data+1
      if(j_data == 562){
        print(i_data)
        j_data <- 1
        i_data <- i_data+1
      }
    }
  }
}

write.csv(data_train, file.path(getwd(), "train.csv"), row.names = FALSE)




test_path <- file.path(getwd(), "UCI HAR Dataset", "test", "X_test.txt")
test_original <- read.csv2(file = test_path, header = FALSE, sep = " ", stringsAsFactors = FALSE)

data_test <- data.frame(matrix(nrow = 10299, ncol = 561))

i_data_test <- 1
j_data_test <- 1
for(i in 1:dim(test_original)[1]){
  for(j in 1:dim(test_original)[2]){
    if(test_original[i,j] != ""){
      data_test[i_data_test,j_data_test] <- as.numeric(test_original[i,j])
      
      j_data_test <- j_data_test+1
      if(j_data_test == 562){
        print(i_data_test)
        j_data_test <- 1
        i_data_test <- i_data_test+1
      }
    }
  }
}

write.csv(data_test[1:2947,], file.path(getwd(), "test.csv"), row.names = FALSE)


