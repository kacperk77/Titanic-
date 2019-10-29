#Loading the packages
library(tidyverse)
library(plotrix)

#Loading data

train <- read.csv('train.csv')

#Variable Name | Description
#--------------|-------------
# Survived      | Survived (1) or died (0)
#Name          | Passenger's name
#Sex           | Passenger's sex
#Age           | Passenger's age
#SibSp         | Number of siblings/spouses aboard
#Parch         | Number of parents/children aboard
#Ticket        | Ticket number
#Fare          | Fare
#Cabin         | Cabin
#Embarked      | Port of embarkation

dim(train)
glimpse(train)
summary(train)

sum(is.na(train$Embarked))

#Training data set contains 12 variables and 891 rows.
#Unfortunately we have 177 NA values in column Age and 2 NA values in column Embarked.

#These 2 values may be easy to predict. 

which(is.na(train$Embarked))

train[62,]
train[830,]

#These two women travelled in the first class and paid 80$ for the ship. 

ggplot(train, aes(x = Embarked, y = Fare, fill = as.factor(Pclass))) + geom_boxplot() + 
  geom_hline(aes(yintercept = 80), linetype = 'dashed', colour = 'red')

#As we can see median fare for the first class for peoplewho got shipped in Cherbourg ammounts to 80 such asfare for our 2 data points.
#We can safely replace 2 NAs with 'C'.

train$Embarked[c(62, 830)]  <- "C"

#At this moment I will not predict Age in data points where there is NA. 



#Analysis of the training data set

#Firstly I will look into qualitative variables

table(train$Survived)
#% of people who died
549/(342+549)

ggplot(data = train) + geom_bar(mapping =aes(x = as.factor(Survived), fill = as.factor(Survived)))+
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))+
  labs(x = "Survived", y = "Count")

#There were more people who died in the disaster than people who survived. 62% died and 48% survived.  



table(train$Survived, train$Pclass)
#survived in the first class 
136/(80+136)
#survived in the third class
119/(372+119)
ggplot(data = train)+
  geom_bar(mapping = aes(x = Pclass, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))


#More than 60% of people in first class survived in the distaster.
#Contrarily only almost 25% of the people who travelled in third class survived in the titanic sank. 

df <- data.frame("Sex" = c('female 37%', 'male 63%'), prop = c((64+197)/(64+197+360+93), (360+93)/(360+94+64+197)))
pie3D(df$prop, labels = df$Sex, explode=0.1, main = 'Piechart of gender')

table(train$Survived, train$Sex)
#women survived 
233/(81+233)
#men survived
109/(468+109)
ggplot(data = train)+
  geom_bar(mapping = aes(x = Sex, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))

#74% of women managed to survive in the disaster.  On the other hand less than 20% of men escape alive from titanic.
#It means that gender drastically influence the probability of survival. 


table(train$Survived, train$Embarked)

ggplot(data = train)+
  geom_bar(mapping = aes(x = Embarked, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))

#As we can see place where someone got on the ship also highly influence the probability of survival.
#People who got shipped on Cherbourg more likely escape alive from titanic than people who got shipped on Queenstown and Southampton. 


table(train$Embarked, train$Pclass)
ggplot(data = train)+
  geom_bar(mapping = aes(x = Embarked, fill = as.factor(Pclass)))

#Majority of people who got shipped in Cherbourg were travelling in the frist class.
#Contrarily majority people who got shipped in Southampton were travelling in the third class.
#Partly it explains the results in the previous table.


#Now I will look into quantitative variables. 

ggplot(train, aes(x = as.factor(Survived),y = Age, fill = as.factor(Survived)))+ 
  geom_boxplot()+ 
  theme(legend.position = "none")+ 
  labs(x="Survived")

#People who surivived from the distaster were a little bit younger.
#It makes sense, because younger people had priority to be on the lifeboat. 


ggplot(train, aes(x = as.factor(Survived), y = SibSp, fill = as.factor(Survived))) +
  geom_boxplot()  +
  theme(legend.position = "none") + 
  labs(x="Survived") 

#There is no visible difference. 


ggplot(train, aes(x = as.factor(Survived), y = Parch, fill = as.factor(Survived))) + geom_boxplot()  + theme(legend.position = "none") + 
  labs(x="Survived") 

#People who survived had more children/parents aboard, but the difference is not very big. 


ggplot(train, aes(x = as.factor(Survived), y = Fare, fill = as.factor(Survived))) + 
  geom_boxplot()  + 
  theme(legend.position = "none") + 
  labs(x="Survived") 

#People who survived had more expensive tickets.



#Logistic regression

#Now I will prepare variables and make logistic regression. 

train$male <- ifelse(train$Sex == 'male', 1, 0)
train$Cherbourg <- ifelse(train$Embarked == "C", 1, 0)
train$Southampton <- ifelse(train$Embarked == "S", 1, 0)

lr1 <- glm(Survived ~ Pclass + Age + SibSp + Parch + Fare + male + Cherbourg + Southampton, data = train, family = binomial)
summary(lr1)

#Now I will exclude, one by one  unsignificant estimations. 

lr2 <- glm(Survived ~ Pclass + Age + SibSp  + Fare + male + Cherbourg + Southampton, data = train, family = binomial)
summary(lr2)

lr3 <- glm(Survived ~ Pclass + Age + SibSp + male + Cherbourg + Southampton, data = train, family = binomial)
summary(lr3)

#Final logistic regression


lr4 <- glm(Survived ~ Pclass + Age + SibSp + male + Cherbourg, data = train, family = binomial)
summary(lr4)


train <- na.omit(train)
train$probabilities  <-  predict(lr4, data = train,  type = "response")
train$prediction  <- rep(0, length(train$PassengerId))
train$prediction[train$probabilities > 0.5] <- 1

results1 <-  table(predicted = train$prediction, actual = train$Survived)
results1


mean(train$prediction == train$Survived)

#Logistic regression corectly predicted the result 80,25% of time. 


#Linear discriminant analysis

library(MASS)
lda  <- lda(Survived~Pclass+Age+male+Cherbourg+Fare+SibSp+Parch, data = train)
lda
lda.prediction <- predict(lda, data = train)
names(lda.prediction)
lda.class  <- lda.prediction$class

table(prediction = lda.class, actual = train$Survived)

mean(lda.class == train$Survived)

#Linear discriminant analysis corectly predicted the result 79,55% of time. 



#Quadratic discriminant analysis

qda <- qda(Survived~Pclass+Age+male+Cherbourg+Fare+SibSp+Parch, data = train)
qda.class <- predict(qda, data = train)$class
table(prediction = qda.class, actual = train$Survived)

mean(qda.class == train$Survived)

#Quadratic discriminant analysis corectly predicted the result 80,81% of time. 

#Summary

#Quadratic discriminant analysis would be the best to predict the survival in the titanic.
#However the discrepancies between corectness of predictions are low and I tested alghoritms on the train data set, which was used to create the models.
#Unfortunately at that time I did not manage to get the information about survivals in the train data set.
#If I did I would have tested alghoritms on test data set. I also did not fill the NA values in the Age column. 
#If I did the predictions would be probably  better. 

