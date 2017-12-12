##Setting Working Directory
setwd("C:/Users/rashm/Desktop/Exploratory Analysis")

##Importing Data and Viewing It
library(readr)
train <- read_csv("Train.csv")
test <- read.csv("Test.csv")
View(data)

##Aggregating Binge Drinking By State in Train Data 
library(tidyverse)
library(stringr)
library(dplyr)
library(COUNT)
df_binge_state_train <- count_(train, c("state","rfbing5"))
df_bingeByState_train<- subset(df_binge_state_train, rfbing5!="9.0")
View(df_bingeByState_train)

##Aggregating Binge Drinking By State in Test Data
df_binge_state_test <- count_(test, c("state","rfbing5"))
df_bingeByState_test<- subset(df_binge_state_test, rfbing5!="9.0")
View(df_bingeByState_test)

##PLotting Binge Drinkers By State
library(ggplot2)
g<- ggplot(df_bingeByState, aes(state,n))
g + geom_point() + facet_grid(. ~ rfbing5) +  xlab("State Name") + ylab("Count of Binge Drinkers") + 
  ggtitle("Binge Drinkers By State")

##Heat Map of Binge Drinkers By State
library(ggplot2)
library(maps)
library(mapproj)

bingedrink <- read_csv("Exploratory Analysis/CDC Plot Data/bingedrink.csv")
view(bingedrink)
head(bingedrink)

bingedrink$region <- tolower(bingedrink$State)
states <- map_data("state")
map.df <- merge(states,bingedrink, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=pbinge))+
  geom_path()+ 
  scale_color_gradient(low = "white", high = "red",na.value="grey90")+
  coord_map() + labs(title = "Binge Drinkers By State", x = "Longitude", y = "Lattitude") + scale_fill_continuous(name = "% of Binge Drinkers")

##Relation BetWeen Smoking and Binge Drinking
df_binge_smoker_train <- count_(train, c("smoker3","rfbing5"))
df_bingeBySmoker_train <- subset(df_binge_smoker_train, rfbing5 == "2")
df_bingeBySmoker_train <- subset(df_binge_smoker_train, smoker3!="9")
View(df_bingeBySmoker_train)

##Cleaning and Importing Smoker3 for Binge Drinkers 
smokerbing <- read_csv("Exploratory Analysis/CDC Plot Data/smokerbing.csv")
View(smokerbing)

##Plotting the Smoking Trends of Binge Drinkers and Non Binge Drinkers 
ggplot(smokerbing, aes(x = smoker3, y=n, fill = rfbing)) + geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Smoking Trends of Binge Drinkers and Non Binge Drinkers", y = "Count", x = "Type of Smoker")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(smokerbing, aes(x=smoker3, y = n)) + geom_bar(stat = "identity", width = 0.5, fill = cbPalette)

##Relation Between Fruit Intake and Binge Drinking 
library(ggplot2)
df_frutsum <- cbind(frutsum = ceiling(train$frutsum), binge = train$rfbing5)
View(df_frutsum)
a <- aggregate(frutsum~binge, df_frutsum, FUN=mean)
frutsumbing <- read_csv("Exploratory Analysis/CDC Plot Data/frutsumbinge.csv")
View(frutsumbing)
ceiling(frutsumbing)
palette <- c("#E69F00", "#56B4E9")
ggplot(frutsumbing, aes(x=binge, y=frutsum)) + geom_bar(stat = "identity", width = 0.25 , fill = palette) +
  labs(title = "Fruit Intake of Binge Drinkers and Non Binge Drinkers", x = "Type of Drinker" ,y = "Grams of Fruit Consumed Per Day")

##Relation Between Age80 and Binge Drinkers 
df_age80Binge <- cbind(age80 = train$age80, binge = train$rfbing5)
View(df_age80Binge)
b <- aggregate(age80~binge, df_age80Binge, FUN = mean)
age80bing <- read_csv("Exploratory Analysis/CDC Plot Data/age80bing.csv")
View(age80bing)
palette2 <- c("#0072B2", "#D55E00")
ggplot(age80bing, aes(x=bing, y=age80)) + geom_bar(stat = "identity", width = 0.25, fill = palette2) +
  labs(title = "Average Age of Binge Drinkers and Non Binge Drinkers", x = "Type of Drinker" ,y = "Age of The Individual")

##Relation Between bmi5 and binge drinkers
df_bmi5bing <- cbind(bmi = ceiling(train$bmi5), bing = train$rfbing5)
View(df_bmi5bing)

data_bmibing <- subset(as.data.frame(df_bmi5bing), bing == 2)
data_bminotbing <- subset(as.data.frame(df_bmi5bing), bing == 1)

a <- ggplot(data=data_bmibing, aes(data_bmibing$bmi)) + geom_histogram() + stat_bin(binwidth = 50)
b <- ggplot(data=data_bminotbing, aes(data_bminotbing$bmi)) + geom_histogram() + stat_bin(binwidth = 50)

ggplot(as.data.frame(df_bmi5bing), aes(x=bmi, fill=bing)) +
  geom_histogram(binwidth=100)+
  facet_grid(bing~.) + facet_wrap(~bing, scales = "free_y") + 
  labs(title = "Relationship Between BMI and Binge Drinking", x = "BMI", y = "Count of People" )

##Relation Between  Vegetable Consumption and Binge Drinking
df_vegesumBing <- cbind(vegesum = train$vegesum, bing = train$rfbing5)
View(df_vegesumBing)
c <- aggregate(vegesum~bing, df_vegesumBing, FUN = mean)
avg_vegesum_bing <- ceiling(c)
vegesumBing <- read_csv("Exploratory Analysis/CDC Plot Data/vegesumBing.csv")
View(vegesumBing)
palette3 <- c("#0072B2", "#D55E00")
ggplot(vegesumBing, aes(x=bing, y=vegesum))+geom_bar(stat = 'identity', width = 0.25, fill = palette3) + 
  labs(title = "Average Vegetable Consumption of Binge Drinkers and Non Binge Drinkers", x = "Type of Drinker", y = "Grams of Vegetables Consumed Per Day")

##Physical Activity and Binge Drinking
df_minacBing <- cbind(minac = ceiling(train$minac11), bing = train$rfbing5)
View(df_minacBing)
ggplot(as.data.frame(df_minacBing), aes(x=minac, fill=bing)) +
  geom_histogram(binwidth=500)+
  facet_grid(bing~.) + facet_wrap(~bing, scales = "free_y") + 
  labs(title = "Relationship Between Physical Activity and Binge Drinking", x = "Physical Activity", y = "Count of People")

