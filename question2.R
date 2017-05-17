traindata = read.csv("SYS6018-FinalQ2_train.csv", header = T, sep = ",")
testdata = read.csv("SYS6018-FinalQ2_test.csv", header = T, sep = ",")

#visualization and basic characteristics
str(traindata)
summary(traindata)
pairs(traindata)
plot(traindata$x1,traindata$x6)
title("x1 versus x6")
plot(traindata$x3,traindata$x5)
title("x3 versus x5")

fit.original = lm(y ~ x1 + x2 + x3 +x4, data = traindata)
summary(fit.original)
fit.final = lm(y ~ x3 , data = traindata)
summary(fit.final)

#MSE and R square
pred.train = predict(fit.final, traindata)
pred.test = predict(fit.final, testdata)

#MSE
MSE.train = mean((pred.train - traindata$y)^2)
MSE.test = mean((pred.test - testdata$y)^2)
cor(pred.test,testdata$y)^2


#the whole model
fit.full = lm(y ~. , data = traindata)
summary(fit.full)
fit.final2 = lm(y ~x5 , data = traindata)
summary(fit.final2)


#MSE again
pred.train2 = predict(fit.final2, traindata)
pred.test2 = predict(fit.final2, testdata)
MSE.train2 = mean((pred.train2 - traindata$y)^2)
MSE.test2 = mean((pred.test2 - testdata$y)^2)
MSE.train2
MSE.test2
cor(pred.test2,testdata$y)^2

#interaction  x2 and x6
fit.final3 = lm(y ~x5+x2*x6 , data = traindata)
summary(fit.final3)
pred.train3 = predict(fit.final3, traindata)
pred.test3 = predict(fit.final3, testdata)
MSE.train3 = mean((pred.train3 - traindata$y)^2)
MSE.test3 = mean((pred.test3 - testdata$y)^2)
MSE.train3
MSE.test3




