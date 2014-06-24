# May 23, 2014
# Descriptive Stats

getwd()
setwd('/Users/saravera/Desktop')
data <- read.csv(file = 'Cleaned_All_Accts_May_23.csv', head = TRUE)

str(data)
head(data)
summary(data)
dim(data)

attach(data)

library(ggplot2)
library(lattice)
library(MASS)

options("scipen" = 100, "digits" = 4)


data <- within(data, rm('X'))

# Basic Descriptives
min(data$days_existed)
max(data$days_existed)
range(data$days_existed)
median(data$days_existed)
mean(data$days_existed)
sd(data$days_existed)
quantile(data$days_existed)
sum(data$days_existed)


# Freq tables
table(ga)
table(plan_name)
table(acct_cancel)
summary(days_existed)
summary(days_to_sub)
summary(contract_value)
summary(days_last_act)
summary(contact_create)
summary(contact_per_user)


as.data.frame(table(ga, plan_name))
 

# Histogram
hist(contract_value)
hist(days_last_act)

hist(contact_create)
as.data.frame(table(contact_create))
contact_create_hist <- contact_create[contact_create > 0]
contact_create_hist <- contact_create_hist[contact_create_hist < 51]
as.data.frame(table(contact_create_hist))
hist(contact_create_hist)

# Bar Plots
counts <- table(data$proj_create, data$plan_name2)
barplot(counts, col = c("darkblue", "red", "green"), beside = TRUE)


# Summary by group -- library(psych)
library(psych)
summary_by_group <- describeBy(data, plan_name2)
print(summary_by_group)


# Grid -- look this up again
library(ca)
mytable <- with(data, table(proj_create, opp_create))
prop.table(mytable, 1)
prop.table(mytable, 2)
fit <- ca(mytable)
print(fit)
summary(fit)
plot(fit)
plot(fit, mass = TRUE, contrib = 'absolute', map = 'rowgreen')


# Plot proj_create and opp_create
with(data, plot(proj_create, opp_create), pch = 14, cex= .5)

plot(contract_value ~ days_existed, xlab="Days Since Beginning", ylab="Contract Value")
plot(contract_value ~ days_to_sub, xlab="Days Since Became Paid", ylab="Contract Value")

# Plot Opp and Proj with ggplot2 -- basic and small multiples
qplot(proj_create, opp_create, data=data, xlab="Project Create", ylab="Opportunity Create", main="Power Users Diverge in Their Use of Product")

plot1 <- ggplot(data, aes(y = proj_per_user, x = opp_per_user, group = plan_name)) + geom_point() + facet_wrap( ~ plan_name)
print(plot1)



# To create a Grid -- tried 'type="n"' but it didn't work. This does, though
plot(contract_value ~ max_users, xlab="Paid Licenses", ylab="Contract Value")
grid()


# Correlations 
cor(proj_create, task_create)
cor(engagement, plan_name2)
cor(engagement, license_util)
cor(engagement, active_users)

# Lattice small multiples
xyplot(log10(proj_create) ~ log10(opp_create) | plan_name)


# Basic Regression
linear <- lm(contract_value ~ engagement + days_last_act + actions_per_user + opp_create + contact_create + task_create + proj_create + pipe_assign +link_add + tag_update + evernote + report_opp + license_util + ga, data = data)
summary(linear)

logistic <- glm(acct_downgrade ~ contract_value + engagement + activities + days_last_act + days_existed + actions_per_user + opp_create + contact_create + task_create + proj_create + pipe_assign +link_add + tag_update + evernote + mailchimp + report_opp + license_util + usage_freq + ga, data = data, family = binomial())
summary(logistic)

# Subset data by plan level and re-run regressions
subset_standard <- data[which(plan_name == 'Standard' | plan_name == 'StandardAnnual'), ]

linear <- lm(contract_value ~ engagement + days_last_act + actions_per_user + opp_create + contact_create + task_create + proj_create + pipe_assign +link_add + tag_update + evernote + report_opp + license_util + ga, data = subset_standard)
summary(linear)

subset_gratis <- data[which(plan_name == 'Gratis'), ]

linear <- lm(contract_value ~ engagement + days_last_act + actions_per_user + opp_create + contact_create + task_create + proj_create + pipe_assign +link_add + tag_update + evernote + report_opp + license_util + ga, data = subset_gratis)
summary(linear)


subset_legacy <- data[which(plan_name == 'Advanced' | plan_name == 'Starter' | plan_name == 'Pro'), ]

linear <- lm(contract_value ~ engagement + days_last_act + actions_per_user + opp_create + contact_create + task_create + proj_create + pipe_assign +link_add + tag_update + evernote + report_opp + license_util + ga, data = subset_legacy)
summary(linear)
