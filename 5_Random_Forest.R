# 4_Random Forest
# May 13, 2014

setwd("/Users/saravera/Desktop")
data <- read.csv('scale_cluster_good_health_4.csv', head = TRUE)

options(scipen = 999) # Turn off Scientific Notation

library(randomForest)
data[1,]

data$fit_cluster <- data$fit.cluster
data <- within(data, rm("fit.cluster"))

str(data)
summary(data)
dim(data)

as.data.frame(names(data))
data$fit_cluster <- as.factor(data$fit_cluster)

# Subtract the outcome variable from cluster analysis and run randomForest
rf <- randomForest(data[,-17], data$fit_cluster)
importance(rf)
