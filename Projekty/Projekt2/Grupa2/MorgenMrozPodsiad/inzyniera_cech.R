source('load&preprocessing.R')

# plec
data3 <- data2 %>% mutate(gender = case_when(
  personal == "rozwiedziony" ~ "mezczyzna",
  personal == "singiel" ~ "mezczyzna",
  personal == "zonaty/wdowiec" ~ "mezczyzna" ,
  personal == "singielka" ~ "kobieta",
  personal == "rozwiedziona/mezatka" ~ "kobieta" ) )

# emeryt
data3 <- data3 %>% mutate(retirement_age = case_when(
  age >= 65 ~ "True",
  age < 65 ~ "False" ) )

# kategoria wieku
data3 <- data3 %>% mutate(age_category = case_when(
  age <= 39 ~ "young",
  age >= 40 & age < 60 ~ "middle-aged",
  age >= 60 ~ "old") )

# bez slubu
data3 <- data3 %>% mutate(never_married = case_when(
  personal == "rozwiedziony" ~ "False",
  personal == "singiel" ~ "True",
  personal == "zonaty/wdowiec" ~ "False" ,
  personal == "singielka" ~ "True",
  personal == "rozwiedziona/mezatka" ~ "False") )

# bez/robotny
data3 <- data3 %>% mutate(employed = case_when(
  present_employment == "bezrobotny" ~ "False",
  present_employment != "bezrobotny" ~ "True") )

# dlugosc kredytu w latach 
data3 <- data3 %>% mutate(duration_years = round(duration/12, 2))

# dlugosc kredytu w latach category
data3 <- data3 %>% mutate(duration_years_cat = case_when(
  duration < 12 ~ "<1",
  duration == 12 ~ "1",
  duration > 12 & duration <24 ~ "1< & <2",
  duration == 24 ~ "2",
  duration > 24 & duration <36 ~ "2< & <3",
  duration == 36 ~ "3",
  duration > 36 ~ "3>" ) )

# przerobka character na factor
data3 <- as.data.frame(unclass(data3))

