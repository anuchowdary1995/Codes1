library(clusterSim)
x=data.Normalization (Data_ori,type="n0",normalization="X_id")
View(x)
Data_ori$X_Id
library(IntClust)
library(BBmisc)
z=normalize(Data_ori, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
View(z)

a=lm(Age~Debt,data=Data_ori)
summary(a)
anova(a)
plot(a)

length(which(is.na(Data_ori)))
#### multicolinearity
library(car)
vif(fit1)
# every null value replace to 0
Data_ori[is.na(Data_ori)] = 0

Data_ori$X_Id[is.na(Data_ori$X_Id)]=mean(Data_ori$X_Id,na.rm = T)

Data_ori$X_Id.1[is.na((Data_ori$X_Id.1))]=round(mean(Data_ori$X_Id.1,na.rm=T))
fit<-glm(T30~.,data=Data_ori)

############ today################

linear<-read.csv("D:/analytics/Analytics_Base/base-analytics-master/data/house_prices.csv")
View(linear)

fit<-lm(Price ~.,data=linear)
summary(fit)

linear$prd<-predict(fit)
linear$err<-residuals(fit)
plot(linear$Price,linear$prd)
plot(linear$Price,linear$err)

library(MASS)
stepAIC(fit)
fit<-lm(formula = Price ~ SqFt + Bedrooms + Bathrooms + Offers + Brick + 
          Neighborhood, data = linear)
summary(fit)

hist(linear$err)
cor(linear$Price,linear$prd)

##### split the data ####

index<-sample(1:128,80,replace=F)
traindata<-linear[index,]
test<-linear[-index,]

### logistic #####

log<-read.csv("D:/analytics/Analytics_Base/base-analytics-master/data/florence.csv")
library(popbio)
logi.hist.plot(log$M ,log$Florence)
logi.hist.plot(log$M ,log$Florence,boxp = F)
logi.hist.plot(log$M ,log$Florence,box=F,type = "hist")
fit2<-glm(Florence ~.,data=log,family = binomial())
summary(fit2)
stepAIC(fit2)
fit2<-  glm(formula = Florence ~ Gender + Recency + F + ChildBks + YouthBks + 
              CookBks + DoltYBks + ArtBks + GeogBks + ItalCook, family = binomial(), 
            data = log)


log$prd<-predict(fit2,type = "response")

 ## roc of auc#
library(ROCR)
pred<-prediction(log$prd,log$Florence)
roc<-performance(pred,measure = "tpr",x.measure = "fpr")
plot(roc,colorize=T)

auc<-performance(pred,measure = "auc")
auc@y.values[[1]]

log$prd<-ifelse(log$prd>0.08,1,0)
table(log$Florence,log$prd)

library(ResourceSelection)
hoslem.test(log$Florence,log$prd)
library(InformationValue)
somersD(log$Florence,log$prd)


##### decision tree#####
library(rpart)
library(rpart.plot)

fit3<-rpart(Florence ~ Gender + Recency + F + ChildBks + YouthBks + 
              CookBks + DoltYBks + ArtBks + GeogBks + ItalCook,method="class",data=log)
summary(fit3)
rpart.plot(fit3)
log$pred<-predict(fit3)
printcp(fit3)
plotcp(fit3)

pfit<- prune(fit3, cp=   fit3$cptable[which.min(fit3$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, uniform=TRUE,cex = 4)
####random forest#####
library(randomForest)
rf<-read.csv("D:/analytics/Analytics_Base/base-analytics-master/data/florence.csv")

fit<-randomForest(Florence ~.,data=rf,ntree=100,nodesize=50)
rf$pred<-predict(fit)
table(rf$pred,rf$Florence)

##### cluster####
clus1<-scale(rf)
View(clus1)
clus<-kmeans(clus1,4)
clus1$clus<-clus$cluster
View(clus1)
plot(clus$cluster)
####  hcluster###
hcl<-hclust(dist(clus1))
plot(hcl)
### svm######
library(caret)
anyNA(rf)
set.seed(10)
svm_Linear <- train(Florence ~., data = rf, method = "svmLinear",
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
rf$prd<-predict(svm_Linear)
View(rf)
plot(svm_Linear, rf, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(4),
     color.palette = terrain.colors)
str(rf)
rf$Florence<-as.factor(rf$Florence)
