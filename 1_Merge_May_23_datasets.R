# Merging "All Accts May 23" data
# May 23, 2014

# load data
dataset1 <- read.csv("/Users/saravera/Desktop/SQL_All_Accounts_May_23.csv", head = TRUE)

dataset2 <- read.csv("/Users/saravera/Desktop/Google_Doc_All_Accounts_May_23.csv", head = TRUE)

# Check columns
dataset1[1,]
dataset2[1,]

str(dataset1)
str(dataset2)

as.data.frame(colnames(dataset1))
as.data.frame(colnames(dataset2))


# Merge Datasets 1 and 2
merged_data <- merge(dataset1, dataset2, by = "instance_id")
merged_data[1,]
head(merged_data)
str(merged_data)

setwd("/Users/saravera/Desktop")
write.csv(merged_data, file = 'All_Accts_May_23.csv')


