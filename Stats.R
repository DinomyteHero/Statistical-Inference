library(ggplot2)
library(dplyr)
library(knitr)

##Part 1

set.seed(13)

n <- 40
sim_num <- 1000
lambda <- 0.2 

sim_sample <- replicate(sim_num, rexp(n,lambda))

mean_sampl <- apply(sim_sample, 2, mean)
theo_mean <- 1/lambda
mean_samp <- round(mean(mean_sampl),2)
print(paste("Theoretical mean is:", theo_mean))
print(paste("Sample Mean is:",round(mean(mean_sampl),2)))
#
theo_var <- (1/lambda)^2 / n
sampl_var <- round(var(mean_sampl),3)

theo_sd <- round(1/(lambda * sqrt(n)),2)
sampl_sd <- round(sd(mean_sampl),2)
print(paste("Theoretical standard deviation is:", theo_sd))
print(paste("Sample standard deviation is:", sampl_sd))

data_sim <- data.frame(mean_sampl)
data_plot <- ggplot(data = data_sim, aes(mean_sampl))

data_plot <- data_plot + geom_histogram(aes(y=..density..),colour="blue", fill = "grey") + geom_vline(xintercept = theo_mean, color = "red", linetype = "dashed") + geom_vline(xintercept = mean_samp, linetype = "dashed")
data_plot <- data_plot + stat_function(fun = dnorm, args = list(mean = mean_samp, sd = sampl_sd), color = "gold1", size = 1.0)
data_plot <- data_plot + stat_function(fun = dnorm, args = list(mean = theo_mean, sd = theo_sd), colour = "red", size = 1.0)
data_plot <- data_plot + labs(title = "Distribution of the Means of the Data", x = "40 Sample Means", y = "Density of Data")
data_plot

#Red is the theoretical mean
#Blue is the sample mean




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

