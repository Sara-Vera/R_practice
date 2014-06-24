k# 3_Data for K means
# May 13, 2014

setwd("/Users/saravera/Desktop")
library(cluster)

data <- read.csv("kmeans_data_good_health.csv", head = TRUE)
data[1,]
str(data)
summary(data)

# Remove all variables not going into the cluster analysis
data <- within(data, rm("X"))
data <- within(data, rm("ga", "days_existed", "Active_users.1", "Contract_value", "Active_users", "Engagement", "Active_days", "plan_name2", "Active.users", "max_users"))


as.data.frame(names(data))
dim(data)


# Within Sum of Squares for Elbow Test
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data,centers=i)$withinss, na.rm = TRUE) 
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# 5 Clusters is best (tried 6 and 4)
fit <- kmeans(data, 3)
data3.summary <- aggregate(data, by = list(fit$cluster), FUN=mean)
data3 <- data.frame(data, fit$cluster) 

clusplot(data3, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
table(data3$fit.cluster)

# Write a csv for Random Forest
write.table(data4, file = 'cluster5.csv', sep = ',')





# ---------------------------------- Scaled Data ---------------------------------------

scale.data <- scale(data)
# standardize variables by converting data to z-scores (http://en.wikipedia.org/wiki/Normalization_(statistics))

str(scale.data)
head(scale.data)
dim(scale.data)


wss <- (nrow(scale.data)-1)*sum(apply(scale.data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scale.data,centers=i)$withinss) 
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

fit<-kmeans(scale.data, 4, nstart = 100)  
scale.data.summary <- aggregate(scale.data,by=list(fit$cluster),FUN=mean)  # get cluster means
scale.data.summary

# Append fit cluster to original data
mydata1 <- data.frame(data, fit$cluster)
str(mydata1)





scale.mydata1 <- data.frame(scale.data, fit$cluster) # append cluster assignment

table(scale.mydata1$fit.cluster)
head(scale.mydata1)
str(scale.mydata1)
summary(scale.mydata1)


#plot the clusters
clusplot(scale.mydata1, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

write.table(scale.mydata1, file = 'scale_cluster_good_health_4.csv', sep = ',')
write.table(mydata1, file = 'cluster_good_health_4.csv', sep = ',') # new data frame with cluster