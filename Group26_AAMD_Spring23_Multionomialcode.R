  #Reading data
  
  mydata <- read.csv("cancer.csv")
  str(mydata)
  mydata$Level <- as.factor(mydata$Level)
  
  #Datapartition
  set.seed(123)
  ind <- sample(2, nrow(mydata),
                replace = TRUE,
                prob = c(0.7,0.3))
  training <- mydata[ind==1,]
  testing <- mydata[ind==2,]

  #multionomial logistic regression
  
  library(nnet)
  
  training$Level <- relevel(training$Level, ref="1")
  mymodel <- multinom(Level~.,
                      data = training)
  
  summary(mymodel)
  
  #calculating p-values
  
  #calculating significance level
  MASS::dropterm(mymodel, trace=FALSE, test="Chisq")
  
  #Step model
  stepmodel <- step(mymodel)
  summary(stepmodel)
  
  #calculating significance level
  MASS::dropterm(stepmodel, trace=FALSE, test="Chisq")
  
  #Confusion matrix & Mis classification error - Training Data
  trainingpred <- predict(stepmodel, training)
  
  Tab <- table(trainingpred,training$Level)
  Tab
  #Accuracy in classification of training data
  
  sum(diag(Tab))/sum(Tab)
  
  #Confusion matrix & Misclassification error - Testing Data
  testingpred <- predict(stepmodel, testing)
  Tab1 <- table(testingpred, testing$Level)
  Tab1
  
  #Accuracy in classification of testing data
  sum(diag(Tab1))/sum(Tab1)
  
  #predict and model assessment
  n <- table(training$Level)
  n/sum(n)
  
  Tab/colSums(Tab)
  
  #Tranferring predictions to excel
  
  allpreds <- c(testingpred,trainingpred)
  
  trainprob <- predict(stepmodel, training, type="probs")
  testprob <- predict(stepmodel, testing, type="probs")
  
 training$prob <- trainprob
 training$class <- trainingpred
 testing$prob <- testprob
 testing$class <- testingpred

write.csv (training, file = "Outcome prediction for train.csv")
write.csv (testing, file = "Outcome prediction for testing.csv")
  