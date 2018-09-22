studentm<-read.csv("C:/Users/Anusha/Downloads/student (1)/student-mat.csv",sep=";",header=TRUE)
sum(is.na(studentm))
head(studentm)

#decision tree
studentm$fG<-factor(ifelse(studentm$G3 >=10,1,0),label=c("fail","pass"))
boxplot(studentm$G3 ~ studentm$Fjob)
m1<-studentm[,-33]
#split the data into train and test
set.seed(10)
index<-sample(1:nrow(m1),round(nrow(m1)*0.7),replace=F)
traindata1=m1[index,]
testdata1=m1[-index,]
View(traindata1)
#train data
modeltrain<-rpart(fG~.,data=traindata1,method="class")
rpart.plot(modeltrain)
traindata1$pred1<-predict(modeltrain,type="class")
traindata1$pred1
traindata1$fG
table(traindata1$fG,traindata1$pred1)
#testdata set
View(testdata1)
modeltest<-rpart(fG~.,data=testdata1,method="class")
rpart.plot(modeltest)
testdata1$pred1<-predict(modeltest,type="class")

table(testdata1$fG,testdata1$pred1)
#entire data
model2<-rpart(fG~.,data=m1,method="class")
library(rpart.plot)
rpart.plot(model2)
rpart.plot(model2, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)
studentm$pred<-predict(model2,type="class")
table(studentm$fG,studentm$pred)

studentp<-read.csv("C:/Users/Anusha/Downloads/student (1)/student-por.csv",sep=";",header=TRUE)
head(studentp)
View(studentp)
studentp$fG<-factor(ifelse(studentp$G3 >=10,1,0),label=c("fail","pass"))
m1<-studentp[,-33]
model3<-rpart(fG~.,data=m1,method="class")
library(rpart.plot)
rpart.plot(model3)
rpart.plot(model3, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)
studentp$pred<-predict(model3,type="class")
table(studentp$fG,studentp$pred)



stud=merge(studentm,studentp,by=c("school","sex","age","address","famsize","Pstatus","Medu",
                                  "Fedu","Mjob","Fjob","reason","nursery","internet"))
View(stud)
stud$f<-factor(ifelse(stud$G3.x>=10,1,0))
stud$p<-factor(ifelse(stud$G3.y>=10,1,0))
names(stud)
stud1<-stud[,-c(33,53)]
stud1<-rpart(f~.,data=stud1,method="class")
rpart.plot(stud1,type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)
