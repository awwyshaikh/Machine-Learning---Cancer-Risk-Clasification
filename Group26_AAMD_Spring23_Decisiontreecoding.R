install.packages(rpart)
library(rpart)
library(rpart.plot)

#Reading data

mydata <- read.csv("cancer.csv")
str(mydata)
mydata$Level <- as.factor(mydata$Level)

#Datapartition
set.seed(123)
ind <- sample(2, nrow(mydata),
              replace = TRUE,
              prob = c(0.7,0.3))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

#Train a decision tree model
model1 <- rpart(formula = Level~., data = train)
model1
summary(model1)
rpart.plot(model1)

?rpart
#Training a model with minbucket = 30
model2 <- rpart(formula = Level~., data = train,
                control = rpart.control(minbucket =30))
model2
rpart.plot(model2)
summary(model2)

#Predictions on data of model with min bucket=25
# Predict on test data
testpred <- predict(model2, newdata = test, type = "class")

# Predict on training data
trainpred<- predict(model2, newdata = train, type = "class")

t <- table(prediction = trainpred, actual = train$Level)
t

sum(diag(t))/sum(t)


t2 <- table(prediction = testpred, actual = test$Level)

t2
sum(diag(t2))/sum(t2)
