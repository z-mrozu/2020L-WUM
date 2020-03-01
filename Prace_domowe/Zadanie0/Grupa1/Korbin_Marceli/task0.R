abalone <- read.csv("abalone_weka_dataset.csv")
library(dplyr)
library(ggplot2)

ggplot(abalone, aes(x=sex, y=rings)) + geom_point()
ggplot(abalone, aes(x=length, y=rings)) + geom_point()

ggplot(abalone) + geom_histogram(aes(x=rings), bins=max(abalone$rings))

ggplot(abalone, aes(x=sex, y=rings)) + geom_boxplot()
