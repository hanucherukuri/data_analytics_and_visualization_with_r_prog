##### Data Visualisation #####

##### Regression Analysis #####

# use library or require command to use the library
library("tidyverse")
library("corrplot")
library(ggpubr)
set.seed(100)

# Import the data set
my_data <- read.csv("k2046544.csv")
view(my_data)

# summary of data
summary(my_data)

# converting covid attribute from integer to a factor data type
my_data$covid <- as.factor(my_data$covid)
summary(my_data)

#Plotting spend using ggplot
my_data %>%  
  ggplot(aes(spend))+
  geom_bar(size = 5)

# Jitter plot
my_data %>%
  ggplot(aes(covid, spend,
             colour = covid))+
  geom_jitter(size = 3, alpha = 0.8)+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~covid)+
  theme_bw()

# Visualising covid vs spend
my_data %>%
  ggplot(aes(covid, spend))+
  geom_boxplot(size = 3, alpha = 0.8)+
  geom_smooth(method = lm, se = F)+
  labs(title = "visualisation of spend during pre-covid and lockdown ")+
  scale_y_continuous(name = "spend",
                     limits = c(8,13),
                     breaks = c(seq(8,13,0.5)))+
  theme(text = element_text(size=16), plot.title = element_text(hjust = 0.5))

# visualising spend vs deprivation using ggplot
my_data$covid <- as.numeric(my_data$covid)

summary(my_data$covid)

plot1 <- my_data %>% 
  ggplot(aes(deprivation,spend,
             colour = deprivation))+
  geom_point(size = 3)+
  ylim(cbind(5,35))+
  stat_smooth(method = lm, se = F, col = "red")+
  labs(title = "spend vs deprivation")+
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5))

plot1


plot2 <- my_data %>% 
  ggplot(aes(covid, spend,  
             colour = spend))+
  geom_point()+
  stat_smooth(method = lm, se = F, col = "red")+
  labs(title = "spend vs covid")+
  scale_x_continuous(name = "covid",
                     limits = c(1, 2),
                     breaks = c(seq(1,2,1)))
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5))

plot2

ggarrange(plot1, plot2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          heights = c(1,1))




