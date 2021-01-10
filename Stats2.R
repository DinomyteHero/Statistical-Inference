library(ggplot2)
library(dplyr)
library(knitr)
#Load the Data
data <- read.csv("ToothGrowth.csv")

data <- rename( data,Number = X, Length = len, Supplement = supp, Dose = dose)
#Due to there being an select type of Doses, change it to a factor
data$Dose <- as.factor(data$Dose)

theplot1 <- ggplot(data = data, aes(x = Dose, y = Length))
theplot1 <- theplot1 + geom_boxplot(aes(fill = Dose)) + labs(title = "Different doses based on tooth length")
theplot1

theplot2 <- ggplot(data = data, aes(x = Supplement, y = Length))
theplot2 + geom_boxplot(aes(fill = Supplement)) + labs(title = "Different tooth length based on supplement")
theplot2

theplot3 <- ggplot(data = data, aes(Dose, Length))
theplot3 <- theplot3 + geom_boxplot(aes(fill = Supplement)) + facet_grid(~ Supplement)
theplot3 <- theplot3 + labs(title = "Different Doses based type of Supplement")
theplot3

sup_test <- t.test(Length~Supplement, data=data)
sup_test
#Since the p-value(0.06) is larger than the level of significance(0.05) then there is no correlation with different supplements and tooth length 
t.test(data$Length[data$Dose == 0.5],data$Length[data$Dose == 2], data=data)
#There is a correlation between tooth length and dose
t.test(data$Length[data$Dose == 0.5],data$Length[data$Dose == 1], data=data)
#There is a correlation between tooth length and dose
t.test(data$Length[data$Dose == 1],data$Length[data$Dose == 2], data=data)
#There is a correlation between tooth length and dose

