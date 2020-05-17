#install.packages('DALEX')
library(DALEX)
data(apartments)
write.csv(data, "apartments.csv", row.names=FALSE)