gdp <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/5%20-%20LifeExpectancy/Data/LifeExp.txt",head=TRUE,sep="")
gdp
plot(gdp$LifeExp~gdp$PPGDP)
boxplot(gdp$LifeExp~gdp$Group)
plot(gdp$LifeExp~log(gdp$PPGDP))
gdp$Group <- factor(gdp$Group, levels=c("other","oecd","africa"))
ggplot(gdp, aes(y=LifeExp, x=log(PPGDP), color=Group))+geom_point()+geom_smooth(method="lm",se=FALSE)
gdplm <- lm(gdp$LifeExp~gdp$PPGDP*gdp$Group)
summary(gdplm)
ngdplm <- lm(data=gdp, LifeExp~log(PPGDP)*Group)
noint <- lm(data=gdp, LifeExp~log(PPGDP)+Group)
summary(ngdplm)

n.cv <- 250
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid <- rep(NA,n.cv)
n.test <- 10

for(i in 1:n.cv){
  #split into test and training set
  test.obs <- sample(1:nrow(gdp),n.test)
  test.set <- gdp[test.obs,]
  train.set <- gdp[-test.obs,]
  
  # fit a lm using training data only
  train.lm <- lm(LifeExp~PPGDP*Group, data=train.set)
  
  # Prediction and prediction intervals
  test.pred <- predict.lm(train.lm,newdata = test.set,interval="prediction",level = 0.95)  
  
  # calculate results
  bias[i] <- mean(test.pred[,1] - test.set$LifeExp)
  rpmse[i] <- sqrt(mean((test.pred[,1] - test.set$LifeExp)^2))
  cvg[i] <- mean(test.pred[,2] < test.set$LifeExp & test.pred[,3] > test.set$LifeExp)
  wid[i] <- mean(test.pred[,3]-test.pred[,2])  
}
mean(bias)
mean(rpmse)
mean(cvg)
mean(wid)

anova(ngdplm,noint)
