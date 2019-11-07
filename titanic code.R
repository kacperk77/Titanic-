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

test$Survived <- NA
all <- rbind(train,test)
sapply(all, function(x){sum(is.na(x))})

sum(is.na(train$Embarked))

#Adding new variable and looking into NA values
all$Title <- gsub('(.*, )|(\\..*)', '', all$Name)
count(all, Title)

VIP <- c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major",
         "Mlle", "Mme","Rev","Sir","the Countess")
all$Title[all$Title %in% VIP] <- "VIP"
all$Title <- as.factor(all$Title)
count(all, Title)

sapply(all, function(x){sum(is.na(x))})

#Data set contains 12 variables and 1309 rows. Unfortunately we have 263 NA values in column Age,
#2 NA values in column Embarked and 1 NA value in Fare. NA values in cabin does not interest us. 


#These 2 values of Embarked may be easy to predict. 

which(is.na(train$Embarked))

train[62,]
train[830,]

#These two women travelled in the first class and paid 80$ for the ship. 

ggplot(train, aes(x = Embarked, y = Fare, fill = as.factor(Pclass))) + geom_boxplot() + 
  geom_hline(aes(yintercept = 80), linetype = 'dashed', colour = 'red')

#As we can see median fare for the first class for peoplewho got shipped in Cherbourg ammounts to 80 such asfare for our 2 data points.
#We can safely replace 2 NAs with 'C'.

train$Embarked[c(62, 830)]  <- "C"

#Dealing with NA in Fare column

which(is.na(all$Fare))
all$Fare[1044] <- median(all$Fare, na.rm = TRUE)

#Dealing with NA's in Age column

AgeLM <- lm(Age ~ Pclass + Sex + SibSp  + Embarked+Title,
            data=all[!is.na(all$Age),])
summary(AgeLM)
all$AgeLM <- predict(AgeLM, all)
indexMissingAge <- which(is.na(all$Age))
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]

#Splitting the all data set

train2 <- all[1:891,]
test2 <- all[892:1309,]


#Exploring training data set

#Firstly I will look into qualitative variables

table(train2$Survived)
#% of people who died
549/(342+549)

ggplot(data = train2) + geom_bar(mapping =aes(x = as.factor(Survived), fill = as.factor(Survived)))+
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))+
  labs(x = "Survived", y = "Count")

#There were more people who died in the disaster than people who survived. 62% died and 48% survived.  



table(train2$Survived, train2$Pclass)
#survived in the first class 
136/(80+136)
#survived in the third class
119/(372+119)
ggplot(data = train2)+
  geom_bar(mapping = aes(x = Pclass, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))


#More than 60% of people in first class survived in the distaster.
#Contrarily only almost 25% of the people who travelled in third class survived in the titanic sank. 

df <- data.frame("Sex" = c('female 37%', 'male 63%'), prop = c((64+197)/(64+197+360+93), (360+93)/(360+94+64+197)))
pie3D(df$prop, labels = df$Sex, explode=0.1, main = 'Piechart of gender')

table(train2$Survived, train2$Sex)
#women survived 
233/(81+233)
#men survived
109/(468+109)
ggplot(data = train2)+
  geom_bar(mapping = aes(x = Sex, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))

#74% of women managed to survive in the disaster.  On the other hand less than 20% of men escape alive from titanic.
#It means that gender drastically influence the probability of survival. 

ggplot(data = train2)+
geom_bar(mapping = aes(x = Title, fill = as.factor(Survived)))+ 
scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))

#There we can see the more complex division. Of course our conclusion do not change.

table(train2$Survived, train2$Embarked)

ggplot(data = train2)+
  geom_bar(mapping = aes(x = Embarked, fill = as.factor(Survived)))+ 
  scale_fill_discrete(name = "Legend", labels = c("Died", "Survived"))

#As we can see place where someone got on the ship also highly influence the probability of survival.
#People who got shipped on Cherbourg more likely escape alive from titanic than people who got shipped on Queenstown and Southampton. 


table(train2$Embarked, train2$Pclass)
ggplot(data = train2)+
  geom_bar(mapping = aes(x = Embarked, fill = as.factor(Pclass)))

#Majority of people who got shipped in Cherbourg were travelling in the frist class.
#Contrarily majority people who got shipped in Southampton were travelling in the third class.
#Partly it explains the results in the previous table.


#Now I will look into quantitative variables. 

ggplot(train2, aes(x = as.factor(Survived),y = Age, fill = as.factor(Survived)))+ 
  geom_boxplot()+ 
  theme(legend.position = "none")+ 
  labs(x="Survived")

#People who surivived from the distaster were a little bit younger.
#It makes sense, because younger people had priority to be on the lifeboat. 


ggplot(train2, aes(x = as.factor(Survived), y = SibSp, fill = as.factor(Survived))) +
  geom_boxplot()  +
  theme(legend.position = "none") + 
  labs(x="Survived") 

#There is no visible difference. 


ggplot(train2, aes(x = as.factor(Survived), y = Parch, fill = as.factor(Survived))) + geom_boxplot()  + theme(legend.position = "none") + 
  labs(x="Survived") 

#People who survived had more children/parents aboard, but the difference is not very big. 


ggplot(train2, aes(x = as.factor(Survived), y = Fare, fill = as.factor(Survived))) + 
  geom_boxplot()  + 
  theme(legend.position = "none") + 
  labs(x="Survived") 

#People who survived had more expensive tickets.



#Logistic regression

#Now I will prepare variables and make logistic regression. 

train2$male <- ifelse(train2$Sex == 'male', 1, 0)
train2$Cherbourg <- ifelse(train2$Embarked == "C", 1, 0)
train2$Southampton <- ifelse(train2$Embarked == "S", 1, 0)


#Final logistic regression


lr4 <- glm(Survived ~ Pclass + Age + SibSp + Cherbourg+Title, data = train2, family = binomial)
summary(lr4)


train2$probabilities  <-  predict(lr4, data = train2,  type = "response")
train2$prediction  <- rep(0, length(train2$PassengerId))
train2$prediction[train2$probabilities > 0.5] <- 1

results1 <-  table(predicted = train2$prediction, actual = train2$Survived)
results1


mean(train2$prediction == train2$Survived)

#Logistic regression corectly predicted the result 82,38% of time. 


#Linear discriminant analysis

library(MASS)
lda  <- lda(Survived~Pclass + Age + SibSp + Cherbourg+Title, data = train2)
lda
lda.prediction <- predict(lda, data = train2)
names(lda.prediction)
lda.class  <- lda.prediction$class

table(prediction = lda.class, actual = train2$Survived)

mean(lda.class == train2$Survived)

#Linear discriminant analysis corectly predicted the result 82,15% of time. 



#Summary

#Logistic regression would be the best to predict the survival in the titanic. 
#However the discrepancies between corectness of predictions are low
#and I tested alghoritms on the train data set,
#which was used to create the models.
#It means the accuracy is biased. 
#I also tested my alghoritms on test data set and i scored 0.77990 accuracy. 

