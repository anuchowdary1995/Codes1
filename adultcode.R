adult<-read.csv("D:/kaggle/Adult/Adult.csv")
View(adult)
adult1=adult[,-1]
name <- c("Age", "Workclass", "Fnlwgt", "Education", "Education_num", "Marital_status", "Occupation",
          "Relationship", "Race", "Sex", "Capital_gain", "Capital_loss", "Hours_per_week", "Native_Country",
          "Class")

# replace the ? into other on workclass varible
adult1$Workclass <- as.factor(ifelse(adult1$Workclass == "?", "Other", as.character(adult1$Workclass)))

# replace the ? into other on occupation

adult1$Occupation <- as.factor(ifelse(adult1$Occupation == "?", "Other", as.character(adult1$Occupation)))

# this is dependent varible change into 0's and 1's
adult1$Class <- as.factor(ifelse(adult1$Class == '<=50K', 0, 1))

summary(adult1)

#cleaning the data set
# workclass
adult1$Workclass = gsub("Federal-gov","Federal-Govt",adult1$Workclass)
adult1$Workclass = gsub("Local-gov","Other-Govt",adult1$Workclass)
adult1$Workclass = gsub("State-gov","Other-Govt",adult1$Workclass)
adult1$Workclass = gsub("Self-emp-inc","Self-Employed",adult1$Workclass)
adult1$Workclass = gsub("Self-emp-not-inc","Self-Employed",adult1$Workclass)
adult1$Workclass = gsub("Without-pay","Not-Working",adult1$Workclass)
adult1$Workclass = gsub("Never-worked","Not-Working",adult1$Workclass)

#occupation
adult1$Occupation = gsub("Adm-clerical","Admin",adult1$Occupation)
adult1$Occupation = gsub("Armed-Forces","Military",adult1$Occupation)
adult1$Occupation = gsub("Craft-repair","Blue-Collar",adult1$Occupation)
adult1$Occupation = gsub("Exec-managerial","White-Collar",adult1$Occupation)
adult1$Occupation = gsub("Farming-fishing","Blue-Collar",adult1$Occupation)
adult1$Occupation = gsub("Handlers-cleaners","Blue-Collar",adult1$Occupation)
adult1$Occupation = gsub("Machine-op-inspct","Blue-Collar",adult1$Occupation)
adult1$Occupation = gsub("Other-service","Service",adult1$Occupation)
adult1$Occupation = gsub("Priv-house-serv","Service",adult1$Occupation)
adult1$Occupation = gsub("Prof-specialty","Professional",adult1$Occupation)
adult1$Occupation = gsub("Protective-serv","Other-Occupations",adult1$Occupation)
adult1$Occupation = gsub("Tech-support","Other-Occupations",adult1$Occupation)
adult1$Occupation = gsub("Transport-moving","Blue-Collar",adult1$Occupation)

#education

adult1$Education = gsub("10th","Dropout",adult1$Education)
adult1$Education = gsub("11th","Dropout",adult1$Education)
adult1$Education = gsub("12th","Dropout",adult1$Education)
adult1$Education = gsub("1st-4th","Dropout",adult1$Education)
adult1$Education = gsub("5th-6th","Dropout",adult1$Education)
adult1$Education = gsub("7th-8th","Dropout",adult1$Education)
adult1$Education = gsub("9th","Dropout",adult1$Education)
adult1$Education = gsub("Assoc-acdm","Associates",adult1$Education)
adult1$Education = gsub("Assoc-voc","Associates",adult1$Education)
adult1$Education = gsub("Preschool","Dropout",adult1$Education)
adult1$Education = gsub("Some-college","HS-grad",adult1$Education)


View(adult1)


class(adult1$Capital.gain)

# convert into integer to factor and indicating the levels in captial_gain&captial_gain

adult1$Capital.gain <- cut(adult1$Capital.gain,c(-Inf, 0, 
                                median(adult1$Capital.gain[adult1$Capital.gain >0]), 
                                    Inf),labels = c("None", "Low", "High"))


adult1$Capital.loss <- cut(adult1$Capital.loss,c(-Inf, 0, 
                                                 median(adult1$Capital.loss[adult1$Capital.loss >0]), 
                                                 Inf),labels = c("None", "Low", "High"))



adult1$Age <- cut(adult1$Age,seq(16,96,10),right = FALSE)

adult1$Hours.per.week <- cut(adult1$Hours.per.week,seq(0,100,10),right = FALSE)

names(adult1)
View(adultl)
adult1$Workclass <- as.factor(adult1$Workclass)
adult1$Education <- as.factor(adult1$Education)
adult1$Occupation <- as.factor(adult1$Occupation)
str(adult1)
sapply(adult1, sd)

#split the data into train and test
set.seed(10)
index<-sample(1:nrow(adult1),nrow(adult1)*0.7,replace=F)
train=adult1[index,]
test=adult1[-index,]

#build the model using logistic
Adult_Model <- glm(Class ~ Age+Workclass+Fnlwgt+Education+Education.num
                   + Marital.status + Occupation + Relationship + 
                     Race + Sex + Capital.gain + Capital.loss+Hours.per.week
                   ,family = 'binomial', data = train)
summary(Adult_Model)
library(MASS)
stepAIC(Adult_Model)
#predictions
train$predit=predict(Adult_Model,type="response")
library(popbio)
logi.hist.plot(train$predit,train$Class)

library(ROCR)
pred<-prediction(train$predit,train$Class)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=T)
# creating the thershold value
train$predit<-ifelse(train$predit>0.3,1,0)
View(train)
#calculate the accuracy
table(train$predit,train$Class)

somersD(train$predit,train$Class)

# test data set
Adult_Model1 <- glm(Class ~ Age+Workclass+Fnlwgt+Education+Education.num
                   + Marital.status + Occupation + Relationship + 
                     Race + Sex + Capital.gain + Capital.loss+Hours.per.week
                   ,family = 'binomial', data = test)
summary(Adult_Model1)
test$pred<-predict(Adult_Model1)
test$pred<-ifelse(test$pred>0.5,1,0)
table(test$Class,test$pred)
