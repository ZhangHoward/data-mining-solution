library(tree)
traindata = read.csv("SYS6018-FinalOEA_train.csv",header=T,na.strings="?",sep = ",")
testdata = read.csv("SYS6018-FinalOEA_test.csv",header=T,na.strings="?",sep = ",")
attach(traindata)


dim(traindata)
names(traindata)
train_x = traindata[,2:12]
train_x2 = traindata[,2:12]
train_y = traindata[,13]
test_x = testdata[2:12]
dim(test_x)
  
fit.lm = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = traindata)
summary(fit.lm)

fit.lm.final = lm(quality ~ fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol, data = traindata)
summary(fit.lm.final)



low= ifelse(quality <=5 ,1,0)
high = ifelse(quality>=8, 1, 0)



train_x.add= data.frame(train_x2 , low)
glm.fit.classi =glm(train_x.add$low~., data=train_x.add ,family =binomial)
glm.probs = predict(glm.fit.classi , test_x , type ="response")
glm.pred.low=rep ("No" ,898)
glm.pred.low[glm.probs >.5]="YES"
test.low= data.frame(test_x , glm.pred.low)



