##### Regression Analysis #####

# use library or require command to use the library
library("tidyverse")
library("corrplot")
set.seed(100)

# Import the data set
my_data <- read.csv("k2046544.csv")
view(my_data)

# summary of data
summary(my_data)

# converting covid attribute from integer to a factor data type
my_data$covid <- as.factor(my_data$covid)
summary(my_data)

# sample model - regression analysis of spend and deprivation



cor(my_data$spend, my_data$deprivation)

model <- lm(spend ~ deprivation, data = my_data)
model

plot(spend ~ deprivation+covid, data = my_data)
abline(model)

summary(model)

# Predicting spend using deprivation and covid variables by Multiple Linear Regression

plot(my_data$covid, my_data$deprivation, xlab = "covid", ylab = "deprivation")

mlr_model <- lm(spend ~ deprivation + covid, data = my_data)

mlr_model

# summary of mlr model output
summary(mlr_model)

# plotting mlr model
plot(mlr_model)

plot(mlr_model, which = 1)

# predicting spend using developed model
pred_spend = predict(mlr_model)
# plotting predicted spend how values are distributed
# Independence of observations
plot(pred_spend)

# residuals and its hist plot 
# residuals should follow normal distribution (NORMALITY)
residspend = residuals(mlr_model)
hist(residspend)

# summary actual spend
summary(my_data$spend)

# summary of predicted spend
summary(pred_spend)

