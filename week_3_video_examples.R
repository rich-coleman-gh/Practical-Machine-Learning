### Predicting with trees
data(iris)
library(ggplot2)
names(iris)

table(iris$Species)

library(caret)

inTrain <- createDataPartition(y=iris$Species,p=.7,list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]

dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width,color=Species,data=training)

model <- train(Species ~ .,method="rpart",data=training)

print(model$finalModel)

plot(model$finalModel,uniform=TRUE,main="Classification Tree")
text(model$finalModel,use.n = TRUE, all=TRUE,cex=.8)

library(rattle)
fancyRpartPlot(model$finalModel)

predict(model,newdata = testing)

### Bagging
library(ElemStatLearn)

data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <-matrix(NA,nrow=10,ncol = 155)
for (i in 1:10) {
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone , data=ozone,span=.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors,temperature,B = 10,bagControl = bagControl(fit=ctreeBag$fit,predict = ctreeBag$pred,aggregate = ctreeBag$agg))

### Random Forests
data(iris)
library(ggplot2)
library(caret)

inTrain <- createDataPartition(y = iris$Species,p=.7,list=FALSE) 

training <- iris[inTrain,]
testing <- iris[-inTrain,]

model <- train(Species ~ .,method="rf",prox=TRUE,data=training)
print(model)

irisP <- classCenter(training[,c(3,4)],training$Species,model$finalModel$proximity)
irisP <- as.data.frame(irisP)
iris$Species <- rownames(irisP)

p <- ggplot(data = training, aes(training$Petal.Width,training$Petal.Length,color=training$Species))
p + geom_point(size=5,shape=4,data=irisP)


pred <- predict(model,testing)

