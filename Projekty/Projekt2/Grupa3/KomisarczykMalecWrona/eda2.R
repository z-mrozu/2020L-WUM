options(stringsAsFactors = FALSE)

# TODO!
books_labeled <- read.csv("./AllBooks_baseline_DTM_Labelled.csv", header = FALSE)
books_unlabeled <- read.csv("./AllBooks_baseline_DTM_Unlabelled.csv", header = FALSE)

aux <- read.csv("./AllBooks_baseline_DTM_Labelled.csv")

head(books_labeled)
str(books_unlabeled)
length(books_labeled$V1)

dim(books_labeled)

aux$X[1:46] <- "Buddhism"
aux$X[47:127] <- "TaoTeChing"
aux$X[128:289] <- "Upanishad"
aux$X[290:478] <- "YogaSutra"
aux$X[479:509] <- "BookOfProverb"
aux$X[510:521] <- "BookOfEcclesiastes"
aux$X[522:571] <- "BookOfEccleasiasticus"
aux$X[572:590] <- "BookOfWisdom"

saveRDS(aux, "books.RDS")




#@@@@@@@@@@@@@@@@@@

books <- readRDS("./books.RDS")
cl <- colnames(books)


# zbiory danych ze strony internetowej - bibliografia(1)

adverbs <- read.csv("./6K adverbs.txt", header = FALSE, stringsAsFactors = FALSE)
adverbs <- adverbs$V1

adjectives <- read.csv("./28K adjectives.txt", header = FALSE, stringsAsFactors = FALSE)
adjectives <- adjectives$V1
  
verbs <- read.csv("./31K verbs.txt", header = FALSE, stringsAsFactors = FALSE)
verbs <- verbs$V1

nouns <- read.csv("./91K nouns.txt", header = FALSE, stringsAsFactors = FALSE)
nouns <- nouns$V1


AUX <- as.data.frame(rbind(cl[-1], NA))

AUX[2,] <- ifelse( AUX[1,] %in% adverbs, "adverb", AUX[2,] )
AUX[2,] <- ifelse( AUX[1,] %in% adjectives, "adjective", AUX[2,] )
AUX[2,] <- ifelse( AUX[1,] %in% verbs, "verb", AUX[2,] )
AUX[2,] <- ifelse( AUX[1,] %in% nouns, "noun", AUX[2,] )


write.csv(t(AUX), "./toChange.txt", row.names = FALSE)


res <- read.csv("./toChange.txt", sep = ",")


kolumny <- c("X", res$V2)



saveRDS(books, "speech_parts.RDS")

books <- readRDS("./speech_parts.RDS")


getIndexes <- function(wektorek){
  aux <- numeric(0)
  for (i in 1:length(wektorek)){
    if(wektorek[i] == 1)aux <- c(aux,i)
  }
  return(aux)
}


verb_indexes <- as.numeric("verb" == kolumny)


v_indexes <- getIndexes(as.numeric("verb" == kolumny))
adj_indexes <- getIndexes(as.numeric("adjective" == kolumny))
adv_indexes <- getIndexes(as.numeric("adverb" == kolumny))
noun_indexes <- getIndexes(as.numeric("noun" == kolumny))


# verbs

verbs <- books$verb
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in v_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  verbs[r] <- chapterSum
}




# nouns

nouns <- books$noun
for (r in 1:nrow(books)){
  chapterSum<- 0
  for(i in noun_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  nouns[r] <- chapterSum
}

# adjectives

adjectives <- books$adjective
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in adj_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  adjectives[r] <- chapterSum
}

# adverbs

adverbs <- books$adverb
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in adv_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  adverbs[r] <- chapterSum
}

books2 <- books[,1:5]

colnames(books2) <- c("X" , "noun", "verb", "adjective", "adverb")

books2$noun <- nouns
books2$verb <- verbs
books2$adjective <- adjectives
books2$adverb <- adverbs


saveRDS(books2, "./speech_parts_aggregated.RDS")




# EMOTIONS:

# z odpowiedniej strony internetowej - bibliografia (2).
emotions <- read.csv("./emotions.csv")


emotion <- 0
for(i in 1:nrow(emotions)){
  emotion[i] <- which.max(emotions[i,2:8])
}

emotions2 <- emotions[,1:2]

colnames(emotions2) <- c("word", "emotion")
emotions2$emotion <- emotion

emotions2$emotion <- ifelse(emotions2$emotion == 1,"disgust" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "2","surprise" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "3","neutral" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "4","anger" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "5","sad" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "6","happy" ,emotions2$emotion)
emotions2$emotion <- ifelse(emotions2$emotion == "7","fear" ,emotions2$emotion)



# create indexes
indexes <- numeric(0)
for( k in 1:length(cl)){
  if (paste(cl[k]," ", sep = "", collapse = NULL) %in% emotions2$word) {
    indexes <- c(indexes, k)
  }
}



kolumny <- colnames(books)

for(k in 1:length(kolumny)){
  for(w in 1:length(emotions2$word)){
    if (emotions2$word[w] == paste(kolumny[k], " ", sep = "")) kolumny[k] <- emotions2$emotion[w]
  }
}


sur_indexes <- getIndexes(as.numeric("surprise" == kolumny))
sad_indexes <- getIndexes(as.numeric("sad" == kolumny))
neu_indexes <- getIndexes(as.numeric("neutral" == kolumny))
happy_indexes <- getIndexes(as.numeric("happy" == kolumny))
anger_indexes <- getIndexes(as.numeric("anger" == kolumny))
fear_indexes <- getIndexes(as.numeric("fear" == kolumny))
disgust_indexes <- getIndexes(as.numeric("disgust" == kolumny))


# sur
surprise <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in sur_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  surprise[r] <- chapterSum
}

# sad
sad <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in sad_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  sad[r] <- chapterSum
}

# neutral
neutral <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in neu_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  neutral[r] <- chapterSum
}

# happy
happy <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in happy_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  happy[r] <- chapterSum
}

# anger
anger <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in anger_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  anger[r] <- chapterSum
}

# fear
fear <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in fear_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  fear[r] <- chapterSum
}

# disgust
disgust <- books$matrix
for (r in 1:nrow(books)){
  chapterSum <- 0
  for(i in disgust_indexes){
    chapterSum <- chapterSum + books[r,i]
  }
  disgust[r] <- chapterSum
}

books3 <- books[,1:8]

colnames(books3) <- unique(kolumny)

books3$surprise <- surprise
books3$sad <- sad
books3$neutral <- neutral
books3$happy <- happy
books3$anger <- anger
books3$fear <- fear
books3$disgust <- disgust

tail(books3,20)

saveRDS(books3, "./emotions.RDS")




