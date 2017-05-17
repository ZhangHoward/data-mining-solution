traindata = read.csv("SYS6018-FinalQ2_train.csv",header=T,na.strings="?",sep = ",")
attach(traindata)
dim(traindata)
testdata = read.csv("SYS6018-FinalQ2_test.csv",header=T,na.strings="?",sep = ",")

library(glmnet)
grid=10^seq(8,0, length =1000)
train_x= model.matrix(y~., traindata)[,2:7]
train_y = traindata$y
test_x= model.matrix(y~., testdata)[,2:7]
test_y= testdata$y
ridge.mod =glmnet(train_x,train_y, alpha=0, lambda=grid)
dim(coef(ridge.mod))


par(mfrow=c(1,1))
plot(ridge.mod$lambda[1:400],coef(ridge.mod)[1,1:400],col="blue",cex=0.5)
legend ("topright", legend =c("intercept") ,col =c("blue") ,lty =1, lwd =2, cex =.8)
title ("intercept versus lambda")

plot(ridge.mod$lambda[1:400],coef(ridge.mod)[2,1:400],col="yellow",cex=0.5)
points(ridge.mod$lambda[1:400],coef(ridge.mod)[3,1:400],col="red",cex=0.5)
points(ridge.mod$lambda[1:400],coef(ridge.mod)[4,1:400],col="grey",cex=0.5)
points(ridge.mod$lambda[1:400],coef(ridge.mod)[5,1:400],col="purple",cex=0.5)
points(ridge.mod$lambda[1:400],coef(ridge.mod)[6,1:400],col="green",cex=0.5)
points(ridge.mod$lambda[1:400],coef(ridge.mod)[7,1:400],col="blue",cex=0.5)
legend ("topright", legend =c("x1","x2","x3","x4","x5","x6") ,col =c("yellow","red","grey","purple","green","blue") ,lty =1, lwd =2, cex =.8)
title("coefficients versus lambda")

#ridge.pred= predict(ridge.mod, s=ridge.mod$lambda[990],newx=test_x)
#MSE=mean(( ridge.pred -test_y)^2)

#evaluate on the test set
MSE=rep(c(0),length(grid))
for (i in 1:length(grid)){
  ridge.pred= predict(ridge.mod, s=ridge.mod$lambda[i],newx=test_x)
  MSE[i]=mean(( ridge.pred -test_y)^2)
}
coef(ridge.mod)[5,1:400]

plot(ridge.mod$lambda[1:400], MSE[1:400], col="blue")
title("MSE versus lambda")
#plot(ridge.mod$lambda, MSE, col="blue")

#choose the best lamda
cv.out =cv.glmnet(train_x, train_y, alpha =0)
par(mfrow=c(1,1))
plot(cv.out,main="MSE versus LOG(Lambda)")
bestlam =cv.out$lambda.min
bestlam
ridge.pred= predict(ridge.mod ,s=bestlam , newx=test_x)
mean((ridge.pred -test_y)^2)

#find the parameters for the best lamda
ridge.mod.best =glmnet(train_x,train_y, alpha=0, lambda=bestlam)
coef(ridge.mod.best)
