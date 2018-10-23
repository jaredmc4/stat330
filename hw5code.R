fat <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/4%20-%20BodyFat/Data/BodyFat.txt", header=TRUE, sep="")
fat
library(GGally)
ggpairs(fat)
pairs(fat)
cor(fat)
cov(fat)
plot(data=fat, brozek~chest)
fatlm <- lm(data=fat, brozek~age+weight+height+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist)
summary(fatlm)
library(MASS)
fatres <-stdres(fatlm)
hist(fatres)
bptest(fatlm)
ks.test(fatres, "pnorm")
qqnorm(fatres)
qqline(fatres)
library(car)
avPlots(fatlm)
ks.test
summary(fat)

n.cv <- 250
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid <- rep(NA,n.cv)
n.test <- 25

for(i in 1:n.cv){
  #split into test and training set
  test.obs <- sample(1:nrow(fat),n.test)
  test.set <- fat[test.obs,]
  train.set <- fat[-test.obs,]
  
  # fit a lm using training data only
  train.lm <- lm(brozek~age+weight+height+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data=train.set)
  
  # Prediction and prediction intervals
  test.pred <- predict.lm(train.lm,newdata = test.set,interval="prediction",level = 0.95)  
  
  # calculate results
  bias[i] <- mean(test.pred[,1] - test.set$brozek)
  rpmse[i] <- sqrt(mean((test.pred[,1] - test.set$brozek)^2))
  cvg[i] <- mean(test.pred[,2] < test.set$brozek& test.pred[,3] > test.set$brozek)
  wid[i] <- mean(test.pred[,3]-test.pred[,2])  
}
mean(bias)
mean(rpmse)
mean(cvg)
mean(wid)

summary(fatlm)

fat.df <- data.frame(age=50, weight=203, height=67, neck=40.2, chest=114.8, abdom=108.1, hip=102.5, thigh=61.3, knee=41.1, ankle=24.7, biceps=34.1, forearm=31, wrist=18.3)
predfat <- predict.lm(fatlm, newdata=fat.df, interval="prediction", level=.95)
predfat
summary(fat)
