##### Statistical Analysis####

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

#####Two sample T-test #####

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


# Two sample T-test between log spend and covid
t.test(logspend ~ covid, data = my_data, var.equal = TRUE, paired = FALSE)
