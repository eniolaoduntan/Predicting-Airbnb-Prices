
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2) 
install.packages("corrplot") 
library(corrplot) 
install.packages("tidyverse") 
library(tidyverse) 
install.packages("caret") 
library(caret) 
install.packages("lmtest") 
library(lmtest)
library(epiDisplay)
install.packages("Matrix")
library(Matrix)



#set working directory
getwd() 
setwd("/Users/user/Desktop") 

air <- read.csv("advanced dataset-cities- Athens_20_ - Barcelona_ - Berlin_15_ - student 146 .csv")

air <-as.data.frame(air)
attach(air)

#numerical summary
dim(air)
summary(air)
str(air)
sapply(air, class)

head(air)
tail(air)
sum(is.na(air))
summary(air$price) 
summary(air$bedrooms)
summary(air$beds)
summary(air$minimum_nights)

air %>% 
  group_by(room_type) %>% 
  count()


air %>% 
  group_by(host_is_superhost) %>% 
  count()


#visualise city frequency
#rename column values
air$city[air$city == "Barcelona_10_Sep_2022_listings (1).csv"] <-"Barcelona"
air$city[air$city == "Athens_20_Sep_2022_listings (1).csv"] <-"Athens"
air$city[air$city == "Berlin_15_Sep_2022_listings (1).csv"] <-"Berlin"

air$city <- as.factor(air$city)


Freq_table<-tab1(air$city, sort.group = "decreasing", cum.percent = TRUE)
Freq_df<-as.data.frame(Freq_table$output.table)
View(Freq_df)
summary(air$city)


#create dataset A
Top_freq <-c('Barcelona')
data <- air[air$city %in% Top_freq,]

#DATA CLEANING

#renaming column values
#instantly bookable
data$instant_bookable[data$instant_bookable == "True"] <- 1
data$instant_bookable[data$instant_bookable == "False"] <-0


#host is super host
data$host_is_superhost[data$host_is_superhost == "True"] <-1
data$host_is_superhost[data$host_is_superhost == "False"] <-0

#host identity verified
data$host_identity_verified[data$host_identity_verified == "True"] <-1
data$host_identity_verified[data$host_identity_verified == "False"] <-0


#host has profile picture
data$host_has_profile_pic[data$host_has_profile_pic == "True"] <-1
data$host_has_profile_pic[data$host_has_profile_pic == "False"] <-0


#host has availabilty]
data$has_availability[data$has_availability == "True"] <- 1
data$has_availability[data$has_availability == "False"] <- 0


sapply(data, class)

#dropping irrelevant columns and columns with 30% NA and above

data[data == "N/A"] <- NA
colMeans(is.na(data))

#Drop NAs in host has profile pic column
data <- data %>% drop_na(c(host_has_profile_pic))


#Deal with NAs in other columns
#replace nas in bedroom, beds with median
data$bedrooms <- replace(data$bedrooms, is.na(data$bedrooms), 1)
data$beds <- replace(data$beds, is.na(data$beds), 2)


#replace nas w=in other columns with mode
# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_response_time)))
# Replace "NA" with mode
data$host_response_time[is.na(data$host_response_time)] <- mode_val

#put in levels
data$host_response_time <- as.numeric(factor(data$host_response_time,
                                             levels = c("within an hour","within a few hours", "within a day", "a few days or more"), labels = c(1,2,3,4) ,ordered =TRUE))



data %>% 
  group_by(host_response_time) %>% 
  count()


# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_response_rate)))
# Replace "NA" with mode
data$host_response_rate[is.na(data$host_response_rate)] <- mode_val
data$host_response_rate <-extract_numeric(data$host_response_rate)



# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_acceptance_rate)))
# Replace "NA" with mode
data$host_acceptance_rate[is.na(data$host_acceptance_rate)] <- mode_val
data$host_acceptance_rate <-extract_numeric(data$host_acceptance_rate)

# Calculate the mode of the column
mode_val <- names(which.max(table(data$bathrooms_text)))
# Replace "NA" with mode
data$bathrooms_text[is.na(data$bathrooms_text)] <- mode_val


# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_neighbourhood)))
# Replace "NA" with mode
data$host_neighbourhood[is.na(data$host_neighbourhood)] <- mode_val



# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_location)))
# Replace "NA" with mode
data$host_location [is.na(data$host_location)] <- mode_val


# Calculate the mode of the column
mode_val <- names(which.max(table(data$host_is_superhost)))
# Replace "NA" with mode
data$host_is_superhost [is.na(data$host_is_superhost)] <- mode_val


#REPLACE NA WITH MEAN VALUES OF SOME COLUMNS
#review score rating
data$review_scores_rating <- as.numeric(data$review_scores_rating)
data$review_scores_rating[is.na(data$review_scores_rating)] <- mean(data$review_scores_rating, na.rm = TRUE)

#review score cleanliness
data$review_scores_cleanliness <- as.numeric(data$review_scores_cleanliness)
data$review_scores_cleanliness[is.na(data$review_scores_cleanliness)] <- mean(data$review_scores_cleanliness, na.rm = TRUE)

#review score check in
data$review_scores_checkin <- as.numeric(data$review_scores_checkin)
data$review_scores_checkin[is.na(data$review_scores_checkin)] <- mean(data$review_scores_checkin, na.rm = TRUE)

#review score communication
data$review_scores_communication <- as.numeric(data$review_scores_communication)
data$review_scores_communication[is.na(data$review_scores_communication)] <- mean(data$review_scores_communication, na.rm = TRUE)

#review score location
data$review_scores_location <- as.numeric(data$review_scores_location)
data$review_scores_location[is.na(data$review_scores_location)] <- mean(data$review_scores_location, na.rm = TRUE)

#review score value
data$review_scores_value <- as.numeric(data$review_scores_value)
data$review_scores_value[is.na(data$review_scores_value)] <- mean(data$review_scores_value, na.rm = TRUE)

#review score accuracy
data$review_scores_accuracy <- as.numeric(data$review_scores_accuracy)
data$review_scores_accuracy[is.na(data$review_scores_accuracy)] <- mean(data$review_scores_accuracy, na.rm = TRUE)

#review score per month
data$reviews_per_month <- as.numeric(data$reviews_per_month)
data$reviews_per_month[is.na(data$reviews_per_month)] <- mean(data$reviews_per_month, na.rm = TRUE)


#extract year from date time columns
#first review year
data$first_review_year <- format(as.Date(data$first_review, format="%d/%m/%Y"), "%Y")
data$first_review_year <- as.numeric(data$first_review_year)

#replace NAs with mode
# Calculate the mode of the column
mode_val <- names(which.max(table(data$first_review_year)))
# Replace "NA" with mode
data$first_review_year [is.na(data$first_review_year)] <- mode_val


#last review year
data$last_review_year <- format(as.Date(data$last_review, format="%d/%m/%Y"), "%Y")
data$last_review_year <- as.numeric(data$last_review_year)


#replace NAs with mode
# Calculate the mode of the column
mode_val <- names(which.max(table(data$last_review_year)))
# Replace "NA" with mode
data$last_review_year [is.na(data$last_review_year)] <- mode_val



colMeans(is.na(data))
sapply(data, class)




#dropping neighbourhood, bathrooms, license, calendar updated, host id and x
data <- data[, -c(1,3,15,16,22,30,46)]

data <- subset(data, select = -first_review)
data <- subset(data, select = -last_review)

colMeans(is.na(data))


#Convert price to numeric
data$cleanprice<-extract_numeric(data$price)
summary(as.numeric(data$cleanprice))
boxplot(data$cleanprice)
hist(data$cleanprice)

#remove outliers from price
data$cleanprice[data$cleanprice>3000] <- NA  
data<- data[!is.na(data$cleanprice),]
hist(data$cleanprice)

data <- subset(data, select = -price)


#remove outliers from minimum nights
data$minimum_nights[data$minimum_nights>365] <- NA  
data<- data[!is.na(data$minimum_nights),]
hist(data$minimum_nights)

#Convert all characters to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
sapply(data, class)


#Visualisation
#Plotting Bar on Host Response Time count
library(ggplot2)
GBarplot<-ggplot(data, aes(x=reorder(host_response_time, host_response_time, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Host Response Time')
GBarplot + theme(text = element_text(size = 7)) 


ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(has_availability), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Availability", x="Availability", y="price($)")

ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(host_response_time), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Response Time", x="Response Time", y="price($)")


plot(data$cleanprice,data$review_scores_rating)


ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(host_is_superhost), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Availability", x="Availability", y="price($)")


qplot(x= host_acceptance_rate, y= cleanprice, data = data)+ 
  geom_smooth(method = "lm", se=FALSE)+ 
  ggtitle("Relationship between Price and Acceptance Rate")


  
ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(last_review_year), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Last Review Year", x="Last Review Year", y="price($)")



ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(room_type), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Room Type", x="Room Type", y="price($)")



#convert all columns to numeric to make a correlation plot
data <- data %>% 
  mutate_if(~ !is.numeric(.), as.numeric)



#make correlation plot

subdata <- data[,c('last_review_year','first_review_year','host_acceptance_rate','host_response_time', 'bedrooms','has_availability','room_type', 'cleanprice')]
cor <- cor(subdata)
cor_sort <- as.matrix(sort(cor[,'cleanprice'], decreasing = TRUE))
corrplot(cor_sort, method="number")
corrplot(cor, tl.col = "blue", tl.pos = "lt")





#Split data into test and train 
set.seed(40387286)
#80% of data goes in the train set, 20% goes in the test set
index <- createDataPartition(data$cleanprice, times=1, p= 0.8, list = FALSE) 
train <- data[index,]
test <- data[-index,]


#Subset Selection Using Forward Stepwise Regression
install.packages("olsrr")
library(olsrr)

model <- lm(cleanprice ~ ., data = train)
ols_step_forward_p(model)


#select 10 among top 15 features to be included in linear regression model
#Build regression model

#Add cross validation method
ctrl <- trainControl(method = 'cv', number = 10)

#model 1 linear regression
lm_model <- train(cleanprice ~ accommodates + bedrooms + beds + availability_60 + minimum_nights + 
                    room_type + property_type+ availability_30+host_acceptance_rate+
                    review_scores_cleanliness,data = train, method= "lm", trControl = ctrl)

summary(lm_model)


# Make predictions on the testing data
predictions_lm <- predict(lm_model, newdata = test)


# Calculate RMSE, R-squared, and MAE for linear regression model
RMSE <- RMSE(predictions_lm, test$cleanprice)
R_squared <- cor(predictions_lm, test$cleanprice) ^ 2
MAE <- MAE(predictions_lm, test$cleanprice)

# Print the results
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")
cat("MAE:", MAE, "\n")


#model 2 Decision Tree
#select top five features from forward stepwise selection
# Fit the decision tree model using rpart
library(rpart)
library(rpart.plot)


dt_model <- rpart(cleanprice ~ accommodates + bedrooms + beds + availability_60 + minimum_nights,
                  data = train, method = "anova", control=rpart.control(minsplit=5, minbucket=10, maxdepth=5))

# Print the decision tree model
printcp(dt_model)

# Plot the decision tree
plot(dt_model, cex = 0.8)
text(dt_model, cex = 0.6)

# Make predictions on the testing data
predictions_dt <- predict(dt_model, newdata = test)


# Calculate RMSE, R-squared, and MAE for the decision tree model
RMSE <- RMSE(predictions_dt, test$cleanprice)
R_squared <- cor(predictions_dt, test$cleanprice) ^ 2
MAE <- MAE(predictions_dt, test$cleanprice)

# Print the results
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")
cat("MAE:", MAE, "\n")



#Model 3 Support Vector Machine
#Top 15 from forward stepwise selection
install.packages("e1071")
library(e1071)

svm_model <- svm(cleanprice ~ accommodates + bedrooms + beds + availability_60 + minimum_nights + 
                   bathrooms_text + room_type + property_type+ availability_30+host_acceptance_rate+
                   review_scores_cleanliness+ number_of_reviews+amenities+host_response_rate +host_response_time,data = train, trControl = ctrl)


predictions_svm <- predict(svm_model, newdata = test)

# Calculate RMSE, R-squared, and MAE for the support vector model
RMSE <- RMSE(predictions_svm, test$cleanprice)
R_squared <- cor(predictions_svm, test$cleanprice) ^ 2
MAE <- MAE(predictions_svm, test$cleanprice)

# Print the results
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")
cat("MAE:", MAE, "\n")




#CREATE DATASET B
Top_freq <-c('Athens','Berlin')
dataB <- air[air$city %in% Top_freq,]

dim(dataB)
summary(dataB)
str(dataB)
sapply(dataB, class)

head(dataB)
tail(dataB)
sum(is.na(dataB))


data[data == "N/A"] <- NA

colMeans(is.na(dataB))

#Data Cleaning
#remove columns with more than 30% NA
dataB <- dataB[, -c(1,3,10,15,16,22,30,46)]

#Deal with NAs in other columns
#Host Location
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$host_location)))
# Replace "NA" with mode
dataB$host_location [is.na(dataB$host_location)] <- mode_val


#Bathrooms Text
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$bathrooms_text)))
# Replace "NA" with mode
dataB$bathrooms_text[is.na(dataB$bathrooms_text)] <- mode_val


#Bedrooms
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$bedrooms)))
# Replace "NA" with mode
dataB$bedrooms [is.na(dataB$bedrooms)] <- mode_val


#Beds
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$beds)))
# Replace "NA" with mode
dataB$beds [is.na(dataB$beds)] <- mode_val

#REPLACE NA WITH MEAN VALUES OF SOME COLUMNS
#review score rating
dataB$review_scores_rating <- as.numeric(dataB$review_scores_rating)
dataB$review_scores_rating[is.na(dataB$review_scores_rating)] <- mean(dataB$review_scores_rating, na.rm = TRUE)

#review score cleanliness
dataB$review_scores_cleanliness <- as.numeric(dataB$review_scores_cleanliness)
dataB$review_scores_cleanliness[is.na(dataB$review_scores_cleanliness)] <- mean(dataB$review_scores_cleanliness, na.rm = TRUE)

#review score check in
dataB$review_scores_checkin <- as.numeric(dataB$review_scores_checkin)
dataB$review_scores_checkin[is.na(dataB$review_scores_checkin)] <- mean(dataB$review_scores_checkin, na.rm = TRUE)

#review score communication
dataB$review_scores_communication <- as.numeric(dataB$review_scores_communication)
dataB$review_scores_communication[is.na(dataB$review_scores_communication)] <- mean(dataB$review_scores_communication, na.rm = TRUE)

#review score location
dataB$review_scores_location <- as.numeric(dataB$review_scores_location)
dataB$review_scores_location[is.na(dataB$review_scores_location)] <- mean(dataB$review_scores_location, na.rm = TRUE)

#review score value
dataB$review_scores_value <- as.numeric(dataB$review_scores_value)
dataB$review_scores_value[is.na(dataB$review_scores_value)] <- mean(dataB$review_scores_value, na.rm = TRUE)

#review score accuracy
dataB$review_scores_accuracy <- as.numeric(dataB$review_scores_accuracy)
dataB$review_scores_accuracy[is.na(dataB$review_scores_accuracy)] <- mean(dataB$review_scores_accuracy, na.rm = TRUE)

#review score per month
dataB$reviews_per_month <- as.numeric(dataB$reviews_per_month)
dataB$reviews_per_month[is.na(dataB$reviews_per_month)] <- mean(dataB$reviews_per_month, na.rm = TRUE)



#extract year from date time columns
#first review year
dataB$first_review_year <- format(as.Date(dataB$first_review, format="%d/%m/%Y"), "%Y")
dataB$first_review_year <- as.numeric(dataB$first_review_year)

#replace NAs with mode
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$first_review_year)))
# Replace "NA" with mode
dataB$first_review_year [is.na(dataB$first_review_year)] <- mode_val


#last review year
dataB$last_review_year <- format(as.Date(dataB$last_review, format="%d/%m/%Y"), "%Y")
dataB$last_review_year <- as.numeric(dataB$last_review_year)


#replace NAs with mode
# Calculate the mode of the column
mode_val <- names(which.max(table(dataB$last_review_year)))
# Replace "NA" with mode
dataB$last_review_year [is.na(dataB$last_review_year)] <- mode_val

dataB <- subset(dataB, select = -first_review)
dataB <- subset(dataB, select = -last_review)


colMeans(is.na(dataB))
sapply(dataB, class)


#renaming column values
#instantly bookable
dataB$instant_bookable[dataB$instant_bookable == "True"] <- 1
dataB$instant_bookable[dataB$instant_bookable == "False"] <-0


#host is super host
dataB$host_is_superhost[dataB$host_is_superhost == "True"] <-1
dataB$host_is_superhost[dataB$host_is_superhost == "False"] <-0

#host identity verified
dataB$host_identity_verified[dataB$host_identity_verified == "True"] <-1
dataB$host_identity_verified[dataB$host_identity_verified == "False"] <-0


#host has profile picture
dataB$host_has_profile_pic[dataB$host_has_profile_pic == "True"] <-1
dataB$host_has_profile_pic[dataB$host_has_profile_pic == "False"] <-0


#host has availabilty]
dataB$has_availability[dataB$has_availability == "True"] <- 1
dataB$has_availability[dataB$has_availability == "False"] <- 0



#Convert price to numeric
dataB$cleanprice<-extract_numeric(dataB$price)
summary(as.numeric(dataB$cleanprice))
boxplot(dataB$cleanprice)
hist(dataB$cleanprice)

#remove outliers from price
dataB$cleanprice[dataB$cleanprice>100] <- NA  
dataB<- dataB[!is.na(dataB$cleanprice),]
hist(dataB$cleanprice)

dataB <- subset(dataB, select = -price)


#Convert all characters to factors
dataB[sapply(dataB, is.character)] <- lapply(dataB[sapply(dataB, is.character)], as.factor)
sapply(dataB, class)


#VISUALISATIONS

ggplot(data=dataB)+
  geom_bar(mapping = aes(x=as.factor(accommodates), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Accomodation Capacity", x="Accommodates", y="price($)")

ggplot(data=dataB)+
  geom_bar(mapping = aes(x=as.factor(bedrooms), y=cleanprice), stat="summary")+ 
  labs(title = "Price by Number of Bedrooms", x="Bedrooms", y="price($)")


plot(dataB$cleanprice,dataB$review_scores_rating)


#convert all columns to numeric
data <- data %>% 
  mutate_if(~ !is.numeric(.), as.numeric)


#BUILDING THE MODEL

#Split data into test and train 
set.seed(40387286)
#80% of data goes in the train set, 20% goes in the test set
index <- createDataPartition(dataB$cleanprice, times=1, p= 0.8, list = FALSE) 
trainB <- dataB[index,]
testB <- dataB[-index,]


model <- lm(cleanprice ~ ., data = trainB)
ols_step_forward_p(model)


#select top 10 features to be included in linear regression model
#Build regression model

#Add cross validation method
ctrl <- trainControl(method = 'cv', number = 10)

#model 1 linear regression
lm_model <- train(cleanprice ~ room_type + reviews_per_month + host_listings_count + accommodates + availability_60+
                    minimum_nights+ latitude+ beds+ host_location+ host_has_profile_pic,
                    data = trainB, method= "lm", trControl = ctrl)

summary(lm_model)


# Make predictions on the testing data
predictions_lm <- predict(lm_model, newdata = testB)


# Calculate RMSE, R-squared, and MAE for linear regression model
RMSE <- RMSE(predictions_lm, testB$cleanprice)
R_squared <- cor(predictions_lm, testB$cleanprice) ^ 2
MAE <- MAE(predictions_lm, testB$cleanprice)

# Print the results
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")
cat("MAE:", MAE, "\n")
