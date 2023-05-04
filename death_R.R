getwd()
setwd('C:\\Users\\Kalyan Rohith T G\\Downloads')
death = read.csv('DeathPenalty.csv')


table(death$Death)
59/(303-59)


#train and test data
library(caTools)
set.seed(88)

split <- sample.split(death$Death, SplitRatio = 0.75)
split
train <- subset(death, split == TRUE)
test <- subset(death, split == FALSE)


#checks

prop.table(table(death$Death))*100
prop.table(table(train$Death))*100
prop.table(table(test$Death))*100


#Fitting a model

logit1 = glm(Death ~ ., data = death, family = binomial)

summary(logit1)



#calculation of Odds ratio
#1) Agg
exp(1.5397)

#2) VRace
exp(1.8106)



#probabilitic prediction on test data


pred = predict(logit1,newdata = test, type = 'response')
pred[1:5]


library(InformationValue)


sensitivity(test$Death, pred)


specificity(test$Death, pred)


precision(test$Death, pred)



youdensIndex(test$Death, pred)



misClassError(test$Death, pred)




library(ROCR)
ROCpred <- prediction(pred, test$Death)
ROCperf <- performance(ROCpred,'tpr','fpr')
plot(ROCperf)
plot(ROCperf, colorize = T,
     print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))


# AUROC
auroc = AUROC(test$Death, pred)
auroc  #Closer the AUROC curve value to 1, better is the model























