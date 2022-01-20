# use library or require command to use the library
library("tidyverse")
set.seed(100)

##### Data Analysis #####

# Import the data set
my_data <- read.csv("k2046544.csv")

# names or columns of my data
names(my_data)

length(my_data)

#view my data
head(my_data)
tail(my_data)
view(my_data)

# dimension of the data
dim(my_data)

# structure of the data 
str(my_data)

glimpse(my_data)

# standard deviation
sd(my_data$deprivation)
sd(my_data$spend)
sd(my_data$logspend)

# converting covid attribute from integer to a factor data type
my_data$covid <- as.factor(my_data$covid)
str(my_data)

#summary of data
summary(my_data)

# Using $ to take particular column from the data set
my_data$deprivation

typeof(my_data)

attributes(my_data)

class(my_data$covid)

length(my_data)

unique(my_data$deprivation)
length(unique(my_data$deprivation))

unique(my_data$covid)
length(unique(my_data$covid))

unique(my_data$spend)
length(unique(my_data$spend))

unique(my_data$logspend)
length(unique(my_data$logspend))

table(my_data$covid)

# slicing data
my_data[1,3]

my_data[ ,3]

# finding missing values or none values
view(my_data[is.na(my_data$spend), ])

# view and sorting data
View(sort(table(my_data$spend), decreasing = TRUE))

# plotting sorted data
barplot((sort(table(my_data$spend), decreasing = TRUE)))

barplot((sort(table(my_data$covid), decreasing = TRUE)))

View((sort(table(my_data$deprivation), decreasing = TRUE)))

barplot((sort(table(my_data$deprivation), decreasing = TRUE)))

# Basic plotting to understand how data is distributed
plot(my_data$deprivation)
plot(my_data$spend)
plot(my_data$covid)
plot(my_data$logspend)

plot(my_data$deprivation, my_data$spend)

plot(my_data$covid, my_data$spend)

plot(my_data$covid, my_data$logspend)

# data analysis ctrl + shift +m for pipe operator
# pipe operator means (and then)
my_data %>%
  select(covid, spend) %>% 
  filter(spend > 8 & covid == 0) %>% 
  arrange(spend) %>% 
  na.omit()







