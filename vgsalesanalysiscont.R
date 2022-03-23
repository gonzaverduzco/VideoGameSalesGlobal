# Machine Learning Project (Daniel Chow, Brian Lucaci, Juan Gonzalez)

# We want to make the assumption that global sales of video games can be accurately predicted by critic score, user rating.

library(tidyverse)

vgsales <- read.csv("vgsales.csv")
attach (vgsales)
View(vgsales)

# We noticed that User_Score was out of 10 but Critic_Score was out of 100, so we simply:

vgsales=mutate(vgsales,User_Score=User_Score*10)

# This way when comparing the two it is more fair

# What we then wanted to do was run some explanatory data analysis to make sure our data was normally distributed

vgsales %>% ggplot(aes(Global_Sales))+geom_histogram()

# This would yield a somewhat left-skewed distribution, so:

vgsales %>% ggplot(aes(Global_Sales))+geom_histogram()+scale_x_log10()

# This made the skew better
# We also plotted the dependent variable (the one we want to predict(Global Sales) against the two independent vars)

vgsales %>% ggplot(aes(User_Score,Global_Sales))+geom_histogram()
vgsales %>% ggplot(aes(Critic_Score,Global_Sales))+geom_histogram()

# Linear analysis was then done on both independent variables

lm1=lm(Global_Sales~User_Score)
summary(lm1)

lm2=lm(Global_Sales~Critic_Score)
summary(lm2)

# Multiple Linear Regression Model

lm3=lm(Global_Sales~User_Score+Critic_Score)
summary(lm3)

library(caret)

vgsales= vgsales %>% 
  select(GlobalSales=Global_Sales,CriticScore=Critic_Score,UserScore=User_Score) %>% 
  mutate(ID=c(1:nrow(vgsales)))

set.seed(123)
SampleTest=sample(1:nrow(vgsales),round(nrow(vgsales)*0.10))
vgsalesTest=vgsales[SampleTest,] %>% mutate(Prediction=NA)
vgsalesTrain=vgsales[-SampleTest,]

vgsales %>% ggplot(aes(User_Score,Global_Sales))+geom_histogram(stat="identity")
vgsales %>% ggplot(aes(Critic_Score,Global_Sales))+geom_histogram(stat="identity")


training=vgsalesTrain[,1:3]
trctrl<- trainControl(method="repeatedcv",number=10, repeats=3)
set.seed(3333)
knn_fit <-train(GlobalSales~UserScore+CriticScore,data=training,method="knn",trcontrol=trctrl,tuneLength=10)
knn_fit


