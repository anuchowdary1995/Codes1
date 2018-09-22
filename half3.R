house_price<-read.csv("C:/Users/Anusha/Desktop/houseprices/train.csv")
summary(house_price)
#imput lotfrontage with mean

house_price$LotFrontage[is.na(house_price$LotFrontage)]<-round(mean(house_price$LotFrontage,na.rm=T))
names(house_price)
View(house_price)
for(i in 1:80) {
    if(is.factor(house_price[,i])){
      house_price[,i]=as.integer(house_price[,i])
    }
}
house_price[,i]=as.integer(house_price[,i])

model=lm(SalePrice~ MSSubClass+LotFrontage+LotArea+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+LowQualFinSF+GrLivArea+GarageArea+WoodDeckSF+OpenPorchSF+EnclosedPorch+ScreenPorch+ScreenPorch+MiscVal+MoSold,data=house_price)
summary(model)
StepAIC(model)
View(model)
house_price$pred=predict(model)
View(predit)
resd=residuals(model)
View(resd)
hist(model)
cook<-cooks.distance(model)
cook
train_out<-house_price[cook < 4/(80-2-1),]
plot(train_out)
