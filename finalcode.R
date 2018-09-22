cred<-read.csv("C:/Users/Anusha/Downloads/crx_data (1).csv")
View(cred)
cred$Approved<-ifelse(cred$Approved=="+",1,0)
cred
credit<-cred[-c(249,328,347,375,454,490,521,599,642,674,207,271,331,457,480,540,602,593,623),]
credit$Age[is.na(credit$Age)]<-round(mean(credit$Age,na.rm = T))
credit$age1<-ifelse(credit$Age>50,0,1)
table(credit$age1,credit$Approved)
credit1=na.omit(credit)
model<-glm(Approved~.,data=credit1)
library(MASS)
stepAIC(model)
library(popbio)
logi.hist.plot(credit1$Age,credit1$Approved)

#split the data set into train and test
set.seed(10)
index<-sample(1:nrow(credit1),round(nrow(credit1)*0.7),replace=F)
traindata1=credit1[index,]
testdata1=credit1[-index,]
View(traindata1)
#bulid the model 
m<-glm(Approved ~ Gender+age1+Debit.Amount+YearsEmployed+PriorDefault+Employed+
         Income,data=traindata1)

#s=glm(formula = Approved ~ MaritalStatus + EducationLevel + YearsEmployed + 
#PriorDefault + Employed + CreditScore + ZipCode + Income, 
#data = traindata1)

summary(m)
traindata1$pre<-predict(m,type="response")

#roc of auc
library(ROCR)
predt <- prediction(traindata1$pre,traindata1$Approved)
roc.perf = performance(predt, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=T)
#area under curve
auc = performance(predt, measure = "auc")
auc@y.values[[1]]  #0.93
#creating the thershold value
traindata1$predtn<-ifelse(traindata1$pre>0.5,1,0)
table(traindata1$predtn,traindata1$Approved)
#confusion matrix
library(caret)
library(e1071)
confusionMatrix(factor(traindata1$predtn),factor(traindata1$Approved))#0.87
#plot the prediction vs target
library(popbio)
logi.hist.plot(traindata1$pre,traindata1$Approved)
#somrsD
library(InformationValue)
somersD(traindata1$Approved,traindata1$pre) #0.87
#hoslem.test
library(ResourceSelection)
hoslem.test(traindata1$Approved,traindata1$predt) #p-value-0.12

# test data
testdata1
n<-glm(Approved ~ Gender+age1+Debit.Amount+YearsEmployed+PriorDefault+Employed+
         Income,data=testdata1)
summary(n)

#t=glm(formula = Approved ~ MaritalStatus + EducationLevel + YearsEmployed + 
#PriorDefault + Employed + CreditScore + ZipCode + Income, 
#data = testdata1)

#predictions
testdata1$predts<-predict(n,type="response")
# thershold
testdata1$predts<-ifelse(testdata1$predts>0.5,1,0)
table(testdata1$predts,testdata1$Approved)
