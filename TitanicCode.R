library(dplyr)
library(ggplot2)
library(stringr)
library(VIM)
library(randomForest)
set.seed(123)

train <- read.csv("train.csv")

test <- read.csv("test.csv")

titanic_data <- bind_rows(train, test)
#some EDA to your data 
str(titanic_data)

summary(titanic_data)

head(titanic_data)

table(titanic_data$Survived)

table(titanic_data$Pclass)

table(titanic_data$SibSp)

table(titanic_data$Embarked)

table(titanic_data$Parch)
#turning some variables into factors
titanic_data$Survived <- as.factor(titanic_data$Survived)
train$Survived <- as.factor(train$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)
#visualizing important variables to understand our data more 
ggplot(train , aes(x = Sex , fill = Survived))+
  geom_bar() + 
  facet_wrap(.~ Pclass)

median(titanic_data$Age , na.rm = TRUE)

ggplot(train , aes(x = Age , fill = Survived))+
 geom_histogram()
#Ading new variable contains the surname 
titanic_data <- titanic_data %>%
  mutate(Surname= str_split(titanic_data$Name , boundary("word"), simplify = T)[,2]) 

table(titanic_data$Sex , titanic_data$Surname)

#dividing them to 4 factors only and putting the rare values in one value named it other 
q <- c('Miss','Master','Mrs','Mr')
titanic_data$Surname[!(titanic_data$Surname %in% q)] <- 'other'
#making new variable contains the values of the family size 
titanic_data <- titanic_data %>%
  mutate(FamilySize = SibSp + Parch)

str(titanic_data)
titanic_data$Surname <- as.factor(titanic_data$Surname)
titanic_data$SibSp <- as.factor(titanic_data$SibSp)
titanic_data$Parch <- as.factor(titanic_data$Parch)
titanic_data$FamilySize <- as.factor(titanic_data$FamilySize)
#imputing the missing values 

table(is.na(titanic_data$Age))
table(is.na(titanic_data$Fare))
table(is.na(titanic_data$Cabin))

titanic_data$Name[is.na(titanic_data$Fare)]
grep("Storey, Mr. Thomas",titanic_data$Name)
titanic_data[1044,]
titanic_imputed <-kNN(titanic_data , variable = c("Fare","Age","Cabin"), k=5)
str(titanic_imputed)
titanic_imputed <- subset(titanic_imputed , select = PassengerId:FamilySize )
str(titanic_imputed)
#bulding our model 
#first split the data again 
train <- titanic_imputed[1:891,]
test <- titanic_imputed[892:1309,]

rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Surname + FamilySize , data = train)

predictions <- predict(rf_model,newdata = test)
table(predictions)
solution <- data.frame(PassengerID = test$PassengerId, Survived = predictions)
write.csv(solution, file = 'titanic.csv', row.names = F)