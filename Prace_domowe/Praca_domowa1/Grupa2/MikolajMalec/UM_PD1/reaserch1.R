library(rpivotTable)
library(corrplot)
library(dplyr)
library(psych)
library(gpairs)
library(plyr)

###########
#ładowanie danych
path <- "/Users/mikolajmalec/Desktop/UM_PD1"

loadeddata <- read.csv2( paste0( path, "/heart_disease_weka_dataset.csv"), sep = ",")

"
age	integer	age of patient
sex	integer	1=male; 0=female
cp	integer	chest pain type: 1=typical angina; 2=atypical angine; 3=non-anginal pain; 4=asymptomatic
trestbps	integer	resting blood pressure (mm Hg)
chol	integer	serum cholestrol (mg/dl)
fbs	integer	fasting blood sugar: 1 if > 120 mg/dl; 0 otherwise
restecg	integer	resting electrocardiographic results: 0=normal; 1=having ST-T wave abnormality; 2=showing probable or definite left ventricular hypertrophy
thalach	integer	maximum heart rate achieved
exang	integer	exercise induced angina: 1=yes; 0=no
oldpeak	float	ST depression induced by exercise relative to rest
slope	integer	the slope of the peak exercise ST segment: 1=upsloping; 2=flat; 3=downsloping
ca	integer	number of major vessels (0-3) colored by fluoroscopy
thal	integer	3=normal; 6=fixed defect; 7=reversable defect
num	integer	predicted attribute; 0=HEART DISEASE; 1=NO HEART DISEASE
"
###########
#przekształcanie na dane numeryczą i z faktorami

num_data <- mutate_all(loadeddata, function(x) as.numeric(as.character(x)))

final_data <-num_data
names(final_data) <- c("age", "sex","chest pain","blood pressure","cholesterol","sugar",
                       "electrocardiographic","heart rate","angina","ST depression","slope","fluoroscopy","defect","HEART DISEASE")

final_data$age <- num_data$age
final_data$sex <- revalue( as.character( num_data$sex), c( "0" = "female", "1" = "male"))
final_data$`chest pain` <- revalue( as.character( num_data$cp), c("1"="typical angina", "2"="atypical angine", "3"="non-anginal pain", "4"="asymptomatic"))
final_data$sugar <- revalue( as.character( num_data$fbs), c( "0" = "low", "1" = "high"))
final_data$electrocardiographic <- revalue( as.character( num_data$restecg), c( "0" = "normal", "1" = "abnormality", "2" = "probable or definite"))
final_data$angina <- revalue( as.character( num_data$exang), c( "0" = "no", "1" = "yes"))
final_data$slope <- revalue( as.character( num_data$slope), c( "1" = "upsloping", "2" = "flat", "3" = "downsloping"))
final_data$fluoroscopy <- as.character( num_data$ca)
final_data$defect <- revalue( as.character( num_data$thal), c( "3" = "normal", "6"="fixed defect","7"="reversable defect"))
final_data$`HEART DISEASE` <- revalue( as.character( num_data$num), c( "0" = "yes", "1" = "no"))

names(num_data) <- names(final_data)

###########
#pivot table

#"1=HEART DISEASE; 0=NO HEART DISEASE"
#poniważ piwot table sumuje, potrzeba odwrócenia danych by widzieć odpowiednie kolumny
rev_data <- num_data
rev_data$`HEART DISEASE` <- (num_data$`HEART DISEASE` -1) *(-1)

rpivotTable( rev_data,
             rows = "cp",
             cols = c("sex","HEART DISEASE"),
             rendererName = "Heatmap",
             aggregatorName = "Count")

###########
#matrix korelacji - tylko dla danych numerycznych

#zwiazki miedzy 
corrplot.mixed(cor(num_data), order="hclust", tl.col="black")

#korelacje

gpairs( rev_data, lower.pars=list(scatter="stats"), 
        outer.rot=c(90,0),
        scatter.pars=list(pch=19))

#important |cor| > 0.40
gpairs( rev_data[,c(3,8,9,10,12,13,14)], lower.pars=list(scatter="stats"), 
        outer.rot=c(90,0),
        scatter.pars=list(pch=19))


matplot( num_data, type = 'l', lty = 1)


##########
#wizualizacja kolumn
par( mfrow = c(3,3))
for (i in 1:9){
  barplot( table( final_data[,i]), main = names(final_data)[i])
}
for (i in 10:14){
  barplot( table( final_data[,i]), main = names(final_data)[i])
}

par( mfrow = c(3,2))
for (i in c(1,4,5,8,10)){
  boxplot( final_data[,i], horizontal = TRUE, main = names(final_data)[i])
}

###########
#scaterplot matrix
pairs( num_data)

###########
#data exploler

library(DataExplorer)

create_report( final_data)
