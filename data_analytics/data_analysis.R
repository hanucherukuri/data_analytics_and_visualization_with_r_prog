# use library or require command to use the library
library("tidyverse")
library("corrplot")
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

# plotting data
plot(my_data$deprivation)
plot(my_data$spend)
plot(my_data$covid)
plot(my_data$logspend)
hist(my_data$spend)
boxplot(my_data$spend)

# data analysis ctrl + shift +m for pipe operator
# pipe operator means (and then)
my_data %>%
  select(covid, spend) %>% 
  filter(spend > 8 & covid == 0) %>% 
  arrange(spend) %>% 
  na.omit()

##### correlation #####

# correlation between variables)
df1 <- my_data %>% 
  select(deprivation, spend, logspend)

x = cor(df1)
x

# correlation plot
corrplot(x, method = 'number', order = 'alphabet')

# correlation test
cor.test(my_data$deprivation, my_data$spend, method = "pearson")

cor.test(my_data$deprivation, my_data$logspend, method = "pearson")

#####statistical analysis #####

#average spend during covid and pre-covid
my_data %>% 
  select(covid, spend) %>% 
  filter(covid == 0 | covid == 1) %>% 
  group_by(covid) %>% 
  summarise(Average_spend = mean(spend))

df2 <- my_data %>% 
  select(covid, spend)

#Two sample t-test no directionality is applied.

# performing t-test
t.test(spend ~ covid, data = df2, var.equal = TRUE, paired = FALSE)

##### Regression Analysis #####

# pred spend

plot(my_data$covid, my_data$deprivation, xlab = "covid", ylab = "deprivation")

lm_model <- lm(spend ~ deprivation + covid, data = my_data)

lm_model

summary(lm_model)

plot(lm_model)

plot(lm_model, which = 1)


library(car)
avPlots(lm_model)

coef(lm_model)



pred_spend = predict(lm_model)
plot(pred_spend)

summary(pred_spend)

residspend = residuals(lm_model)
hist(residspend)

##### Data Visualisation #####
# method 1
ggplot(data = my_data,
       mapping = aes(x = covid,
                     y = spend))+
  geom_point(size = 3, alpha = 0.8)

#method 2
my_data %>%
  ggplot(aes(covid, spend))+
  geom_boxplot(size = 3, alpha = 0.8)+
  ylim(5,20)+
  geom_smooth(method = lm, se = F)

# Jitter plot
my_data %>%
  ggplot(aes(covid, spend,
             colour = covid))+
  geom_jitter(size = 3, alpha = 0.8)+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~covid)+
  theme_bw()

#Plotting spend using ggplot
my_data %>%  
  ggplot(aes(spend))+
  geom_bar(size = 5)

# visualising spend vs deprivation using ggplot
my_data %>% 
  ggplot(aes(deprivation,spend,
             colour = deprivation))+
  geom_point(size = 3)+
  geom_smooth(method = lm, se = F)+
  labs(title = "visualising spend vs derpivation",
       x = "deprivation",
       y = "spend")+
  theme_bw()

# visualising logspend vs deprivation using ggplot
my_data %>% 
  ggplot(aes(deprivation,logspend,
             colour = deprivation))+
  geom_point(size = 3)+
  geom_smooth(method = lm, se = F)+
  labs(title = "visualising spend vs derpivation",
       x = "deprivation",
       y = "logspend")+
  theme_bw()

