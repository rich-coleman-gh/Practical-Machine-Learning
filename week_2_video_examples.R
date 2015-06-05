library(caret);
library(kernlab);
data(spam);
#############################################Video 2################################################
inTrain <- createDataPartition(y=spam$type,p=.6,list=FALSE)

training <- spam[inTrain,]
test <- spam[-inTrain,]
#########################Folds
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain = TRUE)

sapply(folds,length)


##########################Resample
folds <- createResample(y=spam$type,times=10,list=TRUE)

#########################time slices
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow = 20,horizon = 10)

########################################Video 3###################################################
set.seed(1254)

model1 <- train(type ~.,data=training,method="glm")

#######################################Video 4########################################################
library(ISLR);
library(ggplot2);
library(caret);

data(Wage)

inTrain <- createDataPartition(y=Wage$wage,p=.6,list=FALSE)

training <- Wage[inTrain,]
test <- Wage[-inTrain,]

featurePlot(x=training,y=training$wage,plot="pairs")
#####################################Video 5########################################################
library(kernlab);
library(Hmisc);

inTrain <- createDataPartition(y=spam$type,p=.6,list=FALSE)

training <- spam[inTrain,]
test <- spam[-inTrain,]

describe(training)

###########################Standardize

trainStd <- preProcess(training[,-'type'],method=c("center","scale"))

describe(trainCapAveS)

##########################Splines
library(splines)
bsBasis <- bs(training$age,df=3)
bsBasis

##################################Preprocessing with PCA
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type,p=.6,list=FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > .8, arr.ind=T)

####################investigate
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

#################PCA on SPAM
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col = typeColor,xlab = "PC1",ylab="PC2")

###############PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

trainPC <- predict()

##########################linear regression
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,p=.6,list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x=training,y=training$wage,plot="pairs")

modFit <- train(wage ~ age + jobclass + education,method="lm",data=training)

finMod <- modFit$finalModel

plot(finMod,1)
qplot(data=training,finMod$fitted,finMod$residuals,color=race)
#redsiduals by index
plot(finMod$residuals)
