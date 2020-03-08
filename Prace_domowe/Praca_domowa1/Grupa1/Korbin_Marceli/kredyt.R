kredyty <- read.csv("german_credit_data_weka_dataset.csv")
library(rpivotTable)
library(ggplot2)
library(dplyr)

rpivotTable(kredyty, cols = "present_employment", rows = "purpose", aggregatorName="Count", vals = "Value",
            rendererName = "Col Heatmap", height=400, subtotals=FALSE)

# bezrobotni nie chcą radia i telewizji

age_dur <- kredyty %>% group_by(age, duration) %>% count()
colnames(age_dur) <- c("age", "duration", "liczbaOsób")

ggplot(age_dur, aes(x=duration, y=age)) + geom_point(aes(colour=liczbaOsób, size=liczbaOsób)) + guides(size=FALSE)

# zazwyczaj kredyt jest brany na rok~2 lata przez ludzi do 40 lat

# credit_history & credit_amount

ggplot(kredyty, aes(x=credit_history, y=credit_amount)) + geom_boxplot() + coord_flip()
