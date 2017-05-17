library( splines )
traindata = read.csv("SYS6018-FinalQ4_train.csv",header=T,na.strings="?",sep = ",")
attach(traindata)
dim(traindata)
testdata = read.csv("SYS6018-FinalQ4_test.csv",header=T,na.strings="?",sep = ",")
dim(testdata)
train_x=traindata$x
train_y=traindata$y
test_x = testdata$x
test_y = testdata$y

str(traindata)
summary(traindata)
plot(traindata$x,traindata$y)
title ("training set y versus x")



xlims = range(x)
x.grid=seq(from= xlims[1], to= xlims[2],0.1)
xlims_test = range(testdata$x)
x.grid.test=seq(from= xlims_test[1], to= xlims_test[2],0.05)

MSE=rep(c(0),100)
for (i in 3:100){
  fit =lm(train_y~bs(train_x ,df =i) ,data=traindata)
  pred=predict(fit, newdata =list(train_x=test_x),se=T)
  plot(test_x ,test_y ,col ="gray")
  lines(sort(test_x),pred$fit[order(test_x)],lw=2)
  lines(sort(test_x) , pred$fit[order(test_x)] +2*pred$se ,lty ="dashed")
  lines(sort(test_x) , pred$fit[order(test_x)] -2*pred$se ,lty ="dashed")
  MSE[i]=mean(( pred$fit-test_y)^2)
}
MSE = MSE[3:100]
plot(c(3:100),MSE,xlab="number of knots")
bestdf = match(min(MSE),MSE)+2
title("MSE versus knots")
xlab("number of knots")
attr(bs(testdata$x ,df =bestdf) ,"knots")

fit.best =lm(train_y~bs(train_x ,df =bestdf) ,data=traindata)
pred.best=predict(fit.best, newdata =list(train_x=test_x),se=T)
plot(test_x ,test_y ,col ="gray")
lines(sort(test_x),pred.best$fit[order(test_x)],lw=2)
lines(sort(test_x) , pred.best$fit[order(test_x)] +2*pred$se ,lty ="dashed")
lines(sort(test_x) , pred.best$fit[order(test_x)] -2*pred$se ,lty ="dashed")
title("the best-fit on test data")

#Degree 5 polynomial
fit.polynomial =lm(train_y~poly(train_x ,5) ,data= traindata)
coef(fit.polynomial)
pred.polynomial=predict(fit.polynomial, newdata =list(train_x=test_x),se=T)
coef(summary(fit.polynomial))

#Degree 6 smooth spline
fit.smooth = smooth.spline(train_x,train_y,df=6)
summary(fit.smooth)

#Local regression with a span of 0.2
fit.local=loess(train_y~train_x ,span =0.2, data= traindata)
pred.local = predict(fit.local, data.frame(train_x= test_x))
MSE.local=mean(( pred.local-test_y)^2)

#Plot the graphs
plot(test_x ,test_y ,col ="gray")
lines(sort(test_x), pred.polynomial$fit[order(test_x)],lwd =2, col ="pink")
lines(fit.smooth ,col ="red",lwd =2)
lines(x.grid.test, predict(fit.local, data.frame(train_x= x.grid.test)),col ="yellow",lwd =2)
lines(sort(test_x),pred.best$fit[order(test_x)],lw=2,col = "blue")
legend ("topleft", legend =c("polynomial df5","smooth spine","local regression s0.2","cubic spline") ,col =c("pink","red","yellow","blue") ,lty =1, lwd =2, cex =.8)
title("comparison of 4 models")

#ANOVA
#anova(fit.polynomial ,fit.smooth,fit.local,fit.best ,test="F")
anova(fit.polynomial,fit.best)

#comparison
pred.best.train=predict(fit.best, newdata =list(train_x=train_x),se=T)
MSE.cubic.train=mean(( pred.best.train$fit-train_y)^2)
