# May 23, 2014
# Cleaning All Accounts May 23 data

# load merged dataset
data <- read.csv("/Users/saravera/Desktop/All_Accts_May_23.csv", head = TRUE)

data[1,]
str(data)

# Remove some unnecessary columns, either redundant or bad data
data <- within(data, rm("X"))
str(data)

as.data.frame(colnames(data))
colnames(data)[3] <- 'ga' 
colnames(data)[5] <- 'date_created' 
colnames(data)[6] <- 'date_sub' 
colnames(data)[7] <- 'date_cancel' 
colnames(data)[25] <- 'opp_view' 
colnames(data)[26] <- 'opp_create' 
colnames(data)[27] <- 'proj_view' 
colnames(data)[28] <- 'proj_create' 
colnames(data)[33] <- 'settings' 
colnames(data)[34] <- 'pipe_assign'
colnames(data)[41] <- 'report_opp' 


# Find NA's and NULLs
summary(data)


# GA and ROW
summary(data$ga)
data$ga <- as.character(data$ga)
data$ga_dummy <- NA
data$ga_dummy[data$ga == 'NULL'] <- 0 
data$ga_dummy[data$ga != 'NULL'] <- 1
summary(data$ga_dummy)
head(data$ga_dummy)
str(data$ga_dummy)
data$ga <- data$ga_dummy
data <- within(data, rm('ga_dummy'))
data$ga <- as.integer(data$ga)
data$ga <- as.factor(data$ga)
str(data)
 
# Nonprofit as factor
data$is_nonprofit <- as.factor(data$is_nonprofit)


# Plan Name 
summary(data$plan_name)
data$plan_name2 <- factor(data$plan_name, levels=c("Gratis", "Standard", "StandardAnnual", "Starter", "Advanced", "Pro", "Large", "XLarge"), labels = c(0, 1, 2, 3, 4, 5, 6, 7))
summary(data$plan_name2)


# Plan Name Dummy to indicate New versus Legacy plans -- Gratis (9066) Advanced (1711) Pro (313) Large (38) Xlarge (3) Standard (1928) Standard Annual (407)
summary(data$plan_name2)
data$plan_name2 <- as.character(data$plan_name2)
data$plan_dummy <- NA
data$plan_dummy[data$plan_name2 == "3"] <- "0"
data$plan_dummy[data$plan_name2 == "4"] <- "0"
data$plan_dummy[data$plan_name2 == "5"] <- "0"
data$plan_dummy[data$plan_name2 == "6"] <- "0"
data$plan_dummy[data$plan_name2 == "7"] <- "0"
data$plan_dummy[data$plan_name2 == "1"] <- "1"
data$plan_dummy[data$plan_name2 == "2"] <- "1"
data$plan_dummy <- as.integer(data$plan_dummy)
summary(data$plan_dummy)
as.data.frame(table(data$plan_dummy))

# Company Size -> Recoding : 1-10=1, 11-50=2, 51-100=3, 101-500=4, 501+ =5
as.data.frame(table(data$company_size))
as.data.frame(unique(data$company_size))

data$company_size2 <- data$company_size
levels(data$company_size2)[levels(data$company_size2)==""] <- "0"
levels(data$company_size2)[levels(data$company_size2)=="1/10/2014"] <- "1"
levels(data$company_size2)[levels(data$company_size2)=="1/5/2014"] <- "1"
levels(data$company_size2)[levels(data$company_size2)=="6/10/2014"] <- "1"
levels(data$company_size2)[levels(data$company_size2)=="11/20/2014"] <- "2"
levels(data$company_size2)[levels(data$company_size2)=="11-50"] <- "2"
levels(data$company_size2)[levels(data$company_size2)=="21-50"] <- "2"
levels(data$company_size2)[levels(data$company_size2)=="51-100"] <- "3"
levels(data$company_size2)[levels(data$company_size2)=="101-500"] <- "4"
levels(data$company_size2)[levels(data$company_size2)=="501-1000"] <- "5"
levels(data$company_size2)[levels(data$company_size2)=="1000+"] <- "5"
as.data.frame(table(data$company_size2))

data <- within(data, rm("company_size2"))
data$company_size <- data$company_size2


# Actions per Active User
data$actions_per_user <- data$activities / data$active_users
data$actions_per_user <- as.integer(data$actions_per_user)
data$actions_per_user[is.na(data$actions_per_user)] <- 0
str(data$actions_per_user)
data$actions_per_user <- as.integer(data$actions_per_user)

# Project Create per Active User
data$proj_per_user <- data$proj_create / data$active_users
data$proj_per_user <- as.integer(data$proj_per_user)
data$proj_per_user[is.na(data$proj_per_user)] <- 0
str(data$proj_per_user)
data$proj_per_user <- as.integer(data$proj_per_user)

# Opportunity Create per Active User
data$opp_per_user <- data$opp_create / data$active_users
data$opp_per_user <- as.integer(data$opp_per_user)
data$opp_per_user[is.na(data$opp_per_user)] <- 0
str(data$opp_per_user)
data$opp_per_user <- as.integer(data$opp_per_user)

# Contact Create per Active User
data$contact_per_user <- data$contact_create / data$active_users
data$contact_per_user <- as.integer(data$ocontact_per_user)
data$contact_per_user[is.na(data$contact_per_user)] <- 0
str(data$contact_per_user)
data$contact_per_user <- as.integer(data$contact_per_user)



# days to subscribe -> Subracting Dates   ---- 8501 NA's ------
data$days_to_sub <- difftime(as.Date(data$date_sub), as.Date(data$date_created), units = "days")
data$days_to_sub <- as.integer(data$days_to_sub)
str(data)
head(data)
quantile(data$days_to_sub, na.rm = TRUE)
summary(data$days_to_sub)


# Time since opened account
data$days_existed <- difftime(as.Date('2014-5-23'), as.Date(data$date_created), units = "days")
data$days_existed <- as.integer(data$days_existed)
str(data)
head(data)
quantile(data$days_existed, na.rm = TRUE)
summary(data$days_existed)

# Time since last activity
data$days_last_act <- difftime(as.Date('2014-5-23'), as.Date(data$last_activity), units = "days")
data$days_last_act <- as.integer(data$days_last_act)
str(data)
head(data)
quantile(data$days_last_act, na.rm = TRUE)
summary(data$days_last_act)

str(data)

setwd('/Users/saravera/Desktop')
write.csv(data, file = 'Cleaned_All_Accts_May_23.csv')

