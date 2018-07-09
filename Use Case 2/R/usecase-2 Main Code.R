# The main point in this is to predict weather a customer willdeposit in tnat term or not.

'''
# bank client data:
#1 - age (numeric)
#2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
#3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
#4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
#5 - default: has credit in default? (categorical: 'no','yes','unknown')
#6 - housing: has housing loan? (categorical: 'no','yes','unknown')
#7 - loan: has personal loan? (categorical: 'no','yes','unknown')
## related with the last contact of the current campaign:
#8 - contact: contact communication type (categorical: 'cellular','telephone') 
#9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
#10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
#11 - duration: last contact duration, in seconds (numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
# other attributes:
#12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
#14 - previous: number of contacts performed before this campaign and for this client (numeric)
#15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
#16 - balance: balance in account

#Output variable (desired target):
#17 - y - has the client subscribed a term deposit? (binary: 'yes','no')


'''

# We will be suing Classification algorithm to predict it.

rm(list=ls(all=T))

# Importing the dataset and store that in variable named dataset
dataset = read.csv('usecase-2.csv')
str(dataset)
summary(dataset)

################################################################################3
############################### Analysis Of The Data #################################

# Copying the dataset to another variable in order to not to ,mess up with the original data.
data_remove=dataset
str(data_remove)
summary(data_remove)
head(data_remove)

############################ Data Preprocessing #################################
data_remove
summary(data_remove)

# Check the number of NA values in the data
sum(is.na(data_remove))

####################################################################################
#################### Plotting the Data for univariate analysis #################################
#[1] "age"       "job"       "marital"   "education" "default"  
#[6] "balance"   "housing"   "loan"      "contact"   "day"      
#[11] "month"     "duration"  "campaign"  "pdays"     "previous" 
#[16] "poutcome"  "output"  


dev.off()
library(ggplot2)

## Numeric Variable with traget variable and distribution

ggplot(data_remove, aes(x=age,color="blue")) +geom_histogram(binwidth = 5)+ggtitle("Age")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=age,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Age vs Output ")+theme(plot.title = element_text(hjust = 0.5))
#The average age for subscribed and non-subscribed clients is quite similar (41 and 40 years old, respectably).
#There is no significant difference in the age of clients by groups, indicating little association between age and subscription.

ggplot(data_remove, aes(x=balance,color="blue")) +geom_histogram(binwidth = 10000)+ggtitle("Balance")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=balance,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Balance vs Output ")+theme(plot.title = element_text(hjust = 0.5))

ggplot(data_remove, aes(x=duration,color="blue")) +geom_histogram(binwidth = 100)+ggtitle("Duration")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=duration,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Duration vs Output ")+theme(plot.title = element_text(hjust = 0.5))

ggplot(data_remove, aes(x=pdays,color="blue")) +geom_histogram(binwidth = 100)+ggtitle("days")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=pdays,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Pdays vs Output ")+theme(plot.title = element_text(hjust = 0.5))

ggplot(data_remove, aes(x=campaign,color="blue")) +geom_histogram(binwidth = 5)+ggtitle("campaign")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=campaign,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Campaign vs Output ")+theme(plot.title = element_text(hjust = 0.5))

ggplot(data_remove, aes(x=previous,color="blue")) +geom_histogram(binwidth = 10)+ggtitle("previous")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data_remove, aes(x=output, y=previous,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle(" Previous vs Output ")+theme(plot.title = element_text(hjust = 0.5))


ggplot(data_remove, aes(factor(output), pdays)) + geom_boxplot(aes(fill = factor(output)))
ggplot(data_remove, aes(factor(output), age)) + geom_boxplot(aes(fill = factor(output)))
ggplot(data_remove, aes(factor(output), balance)) + geom_boxplot(aes(fill = factor(output)))
ggplot(data_remove, aes(factor(output), duration)) + geom_boxplot(aes(fill = factor(output)))
ggplot(data_remove, aes(factor(output), previous)) + geom_boxplot(aes(fill = factor(output)))
ggplot(data_remove, aes(factor(output), campaign)) + geom_boxplot(aes(fill = factor(output)))



## Categorical Data
ggplot(data_remove,aes(x=marital,fill=output))+geom_bar()
ggplot(data_remove,aes(x=marital,fill=output))+geom_bar(position = "fill")
# More married person are seeking for the loan but the person single is .

ggplot(data_remove,aes(x=job,fill=output))+geom_bar()
ggplot(data_remove,aes(x=job,fill=output))+geom_bar(position = "fill")
# Peoples from management,blue-collar,and technician are contacted most but conversion rate is high in case of students and retired

ggplot(data_remove,aes(x=education,fill=output))+geom_bar()
ggplot(data_remove,aes(x=education,fill=output))+geom_bar(position = "fill")
# More secondary are applying for the loan and the conversion rate is similar for both.

ggplot(data_remove,aes(x=default,fill=output))+geom_bar()
ggplot(data_remove,aes(x=default,fill=output))+geom_bar(position = "fill")
# A person with default no has good conversion rate

ggplot(data_remove,aes(x=housing,fill=output))+geom_bar()
ggplot(data_remove,aes(x=housing,fill=output))+geom_bar(position = "fill")
# Thers is no much effect by housing as there distribution is same

ggplot(data_remove,aes(x=loan,fill=output))+geom_bar()
ggplot(data_remove,aes(x=loan,fill=output))+geom_bar(position = "fill")
# 

ggplot(data_remove,aes(x=contact,fill=output))+geom_bar()
ggplot(data_remove,aes(x=contact,fill=output))+geom_bar(position = "fill")

ggplot(data_remove,aes(x=month,fill=output))+geom_bar()
ggplot(data_remove,aes(x=month,fill=output))+geom_bar(position = "fill")

ggplot(data_remove,aes(x=poutcome,fill=output))+geom_bar()
ggplot(data_remove,aes(x=poutcome,fill=output))+geom_bar(position = "fill")


###########################################################################################
############################ BiVariate Analysis with loan status as color###################


library(ggplot2)

ggplot(data_remove,aes(x=job,y=age,color=output))+geom_boxplot()+
  facet_wrap(~output,scale="free_y")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=job,y=duration,color=output))+geom_boxplot()+
  facet_wrap(~output,scale="free_y")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=marital,fill=loan))+geom_bar()+
  facet_wrap(~output)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=job,fill=output))+geom_bar()+
  facet_wrap(~marital)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=job,fill=output))+geom_bar()+
  facet_wrap(~housing)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=job,fill=output))+geom_bar()+
  facet_wrap(~month)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove,aes(x=output))+geom_bar()+
  facet_wrap(~month)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data_remove, aes(education, fill = output)) + geom_density(alpha = 0.3) +
  xlab("Call duration - density") +
  ylab("Bank account balance")

#By thsi graphs the data is totally right skewed. While spliting the take normally distributed data
#and even the outliers are there in each data better to clear the outliers and then proceed with the data.

######
library(polycor)
corr=hetcor(data)
finalcor=data.frame(corr$correlations)

## default,housing,loan,day,contact,poutcome are having negative correlation and duration is directly related to target and able to say
## 43% of variance.

### Chisquare test

# Categorical
chisq.test(table(data$job,data$output)) # Ok
chisq.test(data$marital,data$output) #ok
chisq.test(data$education,data$output) #ok
chisq.test(table(data$output,data$default),correct = F)#ok
chisq.test(data$housing,data$output,correct = F) #ok
chisq.test(data$loan,data$output) #ok
chisq.test(data$contact,data$output) #ok
chisq.test(data$month,data$output) #ok
chisq.test(data$poutcome,data$output) #ok

wilcox.test(age ~ output, data = data,exact = FALSE) #not ok
wilcox.test(balance ~ output, data = data,exact = FALSE) # ok
wilcox.test(day ~ output, data = data,exact = FALSE) # ok
wilcox.test(duration ~ output, data = data,exact = FALSE) #ook
wilcox.test(pdays ~ output, data = data,exact = FALSE) #ok
wilcox.test(previous ~ output, data = data,exact = FALSE)

# As this test is not giving any signification varaible for further model prepartaion we
# will use aic

################################### Binning the numeric Vriables and making up new variables #############################################
# Binning age
data_remove$nage =cut(data_remove$age,breaks =c(17,35,60,100) , include.lowest=T,labels =c("adult","middle","old") )

#binning campaign
data_remove$ncampaign =cut(data_remove$campaign,breaks =c(0,30,60,Inf) , include.lowest=T,labels =c(1,2,3) )

# Binning duration
data_remove$nduration=(data_remove$duration)/60
data_remove$nduration=cut(data_remove$duration,breaks = c(0,5,10,Inf),include.lowest = T,labels = c('less','medium','high'))


# Binning Pdays
data_remove$npdays=data_remove$pdays/30
data_remove$npdays =cut(data_remove$pdays,breaks =c(-Inf,0,2,6,Inf) , include.lowest=T,labels =c("notcontacted","0_2 Months","2_6 Months","Above 6 Months") )

# Binning Prevous
data_remove$nprevious =cut(data_remove$previous,breaks =c(-Inf,0,100,Inf) , include.lowest=T,labels =c("notcontacted","Contacted Few days","Contacted Long Back") )

#Bining balance
data_remove$nbalance =cut(data_remove$balance,breaks =c(-Inf,0,25000,Inf) , include.lowest=T,labels =c("negative","low","high") )

#Bining Days
data_remove$nday=cut(data_remove$day,breaks = c(0,7,14,28,32),include.lowest = T,labels = c("first","second","third","fourth"))
#############
################################################
########### Model Buiding ######################
data=data_remove
## Splitting the data into train and test

## 80% of the sample size
smp_size = floor(0.8 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(data)), size = smp_size)

train = data[train_ind, ]
test = data[-train_ind, ]

rm(data_remove,dataset)
####################### Predict taking all variables##############################################
#########################################################
#Using logistics regression to remove the variables by step aic method
lr1 <- glm(output ~ . , data = train, family = "binomial")
summary(lr1)

#Running the stepaic
library(MASS)
step=stepAIC(object = lr1,direction = "both")

## After running stepaic we are removing the insignificant variables
'''{r}
Step:  AIC=17067.6
output ~ job + marital + education + housing + loan + contact + 
day + month + duration + campaign + previous + poutcome + 
nage + nduration + nprevious + nbalance + nday
'''
# New dataset from step aic
data_train_aic=subset(train,select=-c(balance,ncampaign,npdays,default,pdays,age))
data_test_aic=subset(test,select=-c(balance,ncampaign,npdays,default,pdays,age))


### GLM Model
glmmodel = glm(output~.,data = data_train_aic,famil="binomial")
summary(glmmodel)

## Getting AUC

library(ROCR)

Classifierglm = predict(glmmodel, type = 'response')
predglm <- prediction(as.numeric(Classifierglm), as.numeric(data_train_aic$output))
perfglm <- performance (predglm, measure = "tpr", x.measure = "fpr")
plot(perfglm, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(predglm, measure="auc")
aucglm <- perf_auc@y.values[[1]]
print(aucglm)

# Getting Confusion matrix
train_pred <- predict(glmmodel)
test_pred <- predict(glmmodel, subset(data_test_aic,select = -c(output)))

library(caret) 
preds_test=ifelse(test_pred>0.15,"yes","no")
preds_train=ifelse(train_pred>0.15,"yes","no")

train_stats1=confusionMatrix(preds_train, data_train_aic$output)
train_stats1[["byClass"]][7]
train_stats1[["byClass"]][6]
train_stats1[["byClass"]][5]
test_stats1=confusionMatrix(preds_test,data_test_aic$output)
test_stats1[["byClass"]][7]
test_stats1[["byClass"]][6]
test_stats1[["byClass"]][5]

################################################################3
##### naive Model

### Whole Model
library(e1071)
nvmodel = naiveBayes(x = subset(data_train_aic,select=-c(output)),
                     y = train$output)
nvmodel

## Getting AUC
'''
train$output
       no       yes 
0.8826864 0.1173136 

Data Set is imbalance and here the no is predominating over yes and there is a
tradeoff between training the data as it will be trained more on No rather than yes.

Generally a customer will subscribe a term deposit if, 

1) His/her job type is management or technician or blue-color or admin but management has the high prior- ity. 

2) If he/she is married. 

3) Education is secondary or tertiary. 

4) Has no credit in default. 

5) Average yearly balance is around ???1751.3002. 

6) Has no housing loan. 

7) Has no personal loan. 

8) Contact communication type is cellular. 

9) Last contact month is (priority basis) May or Au- gust or July or April. 

10) Averge contact duration is around 539.5608 second. and if contact duration is above 448 seconds there is 
huge conversion rate.

11) Number of contact to him/her is around 2.1545 times. 

12) Around 69.2306 days passed after the last contact. 

13) Number of contact performed is around 1.2003. 

14) A person who contacted long back cant term deposit

15) A person with low balance is having high chances of term deposition
'''


# Predict results on train data
train_pred <- predict(nvmodel,subset(data_train_aic,select=-c(output)))

# Predict results on test data
test_pred <- predict(nvmodel, subset(data_test_aic,select=-c(output)))

library(ROCR)
ClassifierPred <- predict(nvmodel, subset(data_train_aic,select = -c(output)), type = 'class')

pred <- prediction(as.numeric(ClassifierPred), as.numeric(data_train_aic$output))

perf <- performance (pred, measure = "tpr", x.measure = "fpr")

plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

library(caret) 

train_stats2=confusionMatrix(train_pred, data_train_aic$output, positive = "no")
train_stats2[["byClass"]][7]
train_stats2[["byClass"]][6]
train_stats2[["byClass"]][5]
test_stats2=confusionMatrix(test_pred,data_test_aic$output,positive = "no")
test_stats2[["byClass"]][7]
test_stats2[["byClass"]][6]
test_stats2[["byClass"]][5]
#################################################################
library(randomForest)

model_rf <- randomForest(output ~ . , data_train_aic,ntree = 200,mtry = 4)

# We can also look at variable importance from the built model using the importance() function and visualise it using the varImpPlot() funcion
varImpPlot(model_rf)

# From this the more important variables are: duration, month,balance,age

# Store predictions from the model
preds_rf <- predict(model_rf, data_test_aic)
preds_rf_prob <- predict(model_rf, data_test_aic, type = "prob")

confusionMatrix(preds_rf, data_test_aic$output)

# Predict on the train data
preds_train_rf <- predict(model_rf)
preds_train_rf_prob <- predict(model_rf, type = "prob")

confusionMatrix(preds_train_rf, data_train_aic$output)

x=getTree(randomForest(data_train_aic[,-13], data_train_aic[,13], ntree=10), 3, labelVar=TRUE)
########################################################
library(rpart)

model_dt <- rpart(output ~ . , data_train_aic)

# * The predictions here too are probabilities for each of the two classes in the target variable

preds_dt = predict(model_dt, data_test_aic, type = "class")
preds_dt_prob = predict(model_dt, data_test_aic)

preds_train_dt_prob <- predict(model_dt)
preds_train_dt= predict(model_dt, type = "class")

confusionMatrix(preds_dt, data_test_aic$output)
confusionMatrix(preds_train_dt, data_train_aic$output)

###################################################################

### Using balancing techniques

library(ROSE)
library(rpart)
library (DMwR)
library(caret)
# Upsampling the data
data_balanced_over <- ovun.sample(output ~ ., data = data_train_aic, method = "over",N=50000, seed = 1)$data
# Smoting
data_balanced_smote <- SMOTE(output ~ ., data_train_aic, perc.over = 300, perc.under=100)
# Rose Smoting
data_balanced_rose <- ROSE(output ~ ., data_train_aic, seed = 1,N=40000)$data
# Under
data_under <- ovun.sample(output~. ,data_train_aic,method="under")$data

######### Logistic Regression #####################3
ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE,verboseIter = T)

# Using smote
mod_fit_smote <- train(output~.,data=data_train_aic, method="glm", family="binomial",
                       trControl = ctrl, tuneLength = 2)
summary(mod_fit_smote)
mod_fit_smote

# Using over
mod_fit_over <- train(output~.,data=data_train_aic, method="glm", family="binomial",
                      trControl = ctrl, tuneLength = 2)
summary(mod_fit_over)
mod_fit_over

# Using rose
mod_fit_rose <- train(output~.,data=data_train_aic, method="glm", family="binomial",
                      trControl = ctrl, tuneLength = 2)
summary(mod_fit_rose)
mod_fit_rose


# Using under
mod_fit_under <- train(output~.,data=data_train_aic, method="glm", family="binomial",
                       trControl = ctrl, tuneLength = 2)
summary(mod_fit_under)
mod_fit_under

## Metric for verification
pred_under = predict(mod_fit_under, newdata=data_test_aic)
confusionMatrix(data=pred_under, data_test_aic$output)

pred_smote = predict(mod_fit_smote, newdata=data_test_aic)
confusionMatrix(data=pred_smote, data_test_aic$output)

pred_over = predict(mod_fit_over, newdata=data_test_aic)
confusionMatrix(data=pred_over, data_test_aic$output)

pred_rose = predict(mod_fit_rose, newdata=data_test_aic)
confusionMatrix(data=pred_rose, data_test_aic$output)


varImp(mod_fit_over)

#######################################################3
############################ Decision Tree ##################
# Build a Classification model using rpart 
library(rpart)
library(rattle)
library(rpart.plot)
require(maptree)
dev.off()
############# Over ###################3
DT_over <- rpart(output~., data=data_balanced_over,cp=0.014) # Default cp = 0.01
plotcp(DT_over)
rpart.plot(DT_over)

# Predict on Train and Test data
predCartTest_dt <- predict(DT_over, newdata=data_test_aic, type="class")
confusionMatrix(data_test_aic$output,predCartTest_dt)

################ smote ######################3
DT_smote <- rpart(output~., data=data_balanced_smote,cp=0.017) # Default cp = 0.01
plotcp(DT_smote)
draw.tree(DT_smote,cex = 0.8)
rpart.plot(DT_smote)

# Predict on Train and Test data
predCartTest_dt_smote <- predict(DT_smote, newdata=data_test_aic, type="class")
confusionMatrix(data_test_aic$output,predCartTest_dt_smote)

########### Rose ############3333
DT_rose <- rpart(output~., data=data_balanced_rose,cp=0.0017) # Default cp = 0.01
plotcp(DT_rose)
rpart.plot(DT_rose,faclen = 2,tweak = 1.2)
plot(printcp(DT_rose), type='b')

# Predict on Train and Test data
predCartTest_dt_rose <- predict(DT_rose, newdata=data_test_aic, type="class")
confusionMatrix(data_test_aic$output,predCartTest_dt_rose)

###################3 Under sampling ################
DT_under <- rpart(output~., data=data_under,cp=0.0012) # Default cp = 0.01
print(DT_under)
plotcp(DT_under)
rpart.plot(DT_under,digits = 0,tweak = 1.2)

# Predict on Train and Test data
predCartTest_dt_under <- predict(DT_under, newdata=data_test_aic, type="class")
confusionMatrix(data_test_aic$output,predCartTest_dt_under)

