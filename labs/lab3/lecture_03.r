###################################
#### MAR 580 POPULATION MODELING
# Fall 2015
# Lecture / Lab 3
# 15 Sep 2015
# Instructor: Gavin Fay, gfay@umassd.edu
###################################

### STREAMS EXAMPLE ###
setwd("C:/Users/pganz/Documents/UAF/advpopmod/labs/lab3")
library(nlme)
Streams <- scan('streams.dat',what=list(Stream=0,Density=0),n=3*18,skip=5)
streams.lm1 <- lm(Density~1,data=Streams)
streams.lm2 <- lm(Density~factor(Stream)-1,data=Streams)
streams.lme1 <- lme(Density~1,random=~1|Stream,data=Streams,meth="ML")
#help function for lme()
?(lme)
#summary of results
summary(streams.lme1)

#compare residuals among models
par(las=0)
boxplot(split(residuals(streams.lm1)/summary(streams.lm1)$sigma,Streams$Stream))
mtext(side=3,"lm, intercept only",line=2)
mtext(side=2,"Residuals",line=2)
abline(h=0)
boxplot(split(residuals(streams.lm2)/summary(streams.lm2)$sigma,Streams$Stream))
mtext(side=3,"lm, stream effect",line=2)
mtext(side=1,"Stream",line=2)
abline(h=0)
boxplot(split(residuals(streams.lme1)/summary(streams.lme1)$sigma,Streams$Stream))
mtext(side=3,"lme, random stream effect",line=2)
abline(h=0) 

# plot the fitted values
library(lattice)
attach(Streams)
plot(streams.lme1,Density~fitted(.)|Stream, abline = c(0,1)) 
detach(Streams)

# diagnostic plots of residuals, random effects
par(mfrow=c(2,3))
# QQ plot
qqnorm(streams.lme1$residuals/streams.lme1$sigma,ylab="Quantiles of Residuals")
qqline(streams.lme1$residuals/streams.lme1$sigma)

plot(residuals(streams.lme1)/streams.lme1$sigma,ylab="Standardized Residuals")
hist(residuals(streams.lme1)/streams.lme1$sigma,xlab="Standardized Residuals",main="")

#homogeneity of within group variance
boxplot(split(residuals(streams.lme1)/streams.lme1$sigma,Streams$Stream),ylab="Standardized Residual",xlab="Stream",csi=0.2)
abline(0,0,lwd=3)
#normality of the between-group residuals
re.sigma <- as.numeric(VarCorr(streams.lme1)[1,2])
print(streams.lme1$coefficients$random$Stream)
re<-streams.lme1$coefficients$random$Stream/re.sigma  ##/0.6184
qqnorm(re,ylab="Quantiles of random effects")
qqline(re)
hist(streams.lme1$coefficients$random$Stream/re.sigma,xlab="random effects",main="")



###### Weight-Length relationships ######
WtLen <- scan('wtlen.txt',what=list(Subject=0,Length=0,Weight=0),n=3*100,skip=1)
WtLen$LogLen <- log(WtLen$Length)
WtLen$LogWt <- log(WtLen$Weight)
# linear model, no individual parameters
wtlen.lm1 <- lm(LogWt~LogLen,data=WtLen)
# linear model, individual ln_a parameters
wtlen.lm2 <- lm(LogWt~factor(Subject)+LogLen-1,data=WtLen)
## linear mixed effects models, lab exercise.
library(nlme)
# ln_a as random effect by subject
wtlen.lme1 <- lme(LogWt~LogLen,random=~1|Subject,data=WtLen,meth="ML")
# b as random effect by subject
wtlen.lme2 <- lme(LogWt~LogLen,random=~-1+LogLen|Subject,data=WtLen,meth="ML")
# compare models using AIC
AIC(wtlen.lm1,wtlen.lm2,wtlen.lme1,wtlen.lme2)

# based on AIC, wtlen.lm2 is the "best" of all 4 models
summary.lm(wtlen.lm2)

par(mfrow=c(2,2))
# QQ plot
qqnorm(wtlen.lm2$residuals/summary.lm(wtlen.lm2)$sigma,ylab="Quantiles of Residuals")
qqline(wtlen.lm2$residuals/summary.lm(wtlen.lm2)$sigma)

plot(residuals(wtlen.lm2)/summary.lm(wtlen.lm2)$sigma,ylab="Standardized Residuals")
hist(residuals(wtlen.lm2)/summary.lm(wtlen.lm2)$sigma,xlab="Standardized Residuals",main="")

#homogeneity of within group variance
boxplot(split(residuals(wtlen.lm2)/summary.lm(wtlen.lm2)$sigma,Streams$Stream),ylab="Standardized Residual",xlab="Stream",csi=0.2)
abline(0,0,lwd=3)

# however, if we want an overall mean and between-subject variance, 
# wtlen.lme1 is the best random effects model based on AIC
summary(wtlen.lme1)
coef(wtlen.lme1)

par(mfrow=c(2,3))
# QQ plot
qqnorm(wtlen.lme1$residuals/wtlen.lme1$sigma,ylab="Quantiles of Residuals")
qqline(wtlen.lme1$residuals/wtlen.lme1$sigma)

plot(residuals(wtlen.lme1)/wtlen.lme1$sigma,ylab="Standardized Residuals")
hist(residuals(wtlen.lme1)/wtlen.lme1$sigma,xlab="Standardized Residuals",main="")

#homogeneity of within group variance
boxplot(split(residuals(wtlen.lme1)/wtlen.lme1$sigma,WtLen$Subject),ylab="Standardized Residual",xlab="Stream",csi=0.2)
abline(0,0,lwd=3)
#normality of the between-group residuals
re.sigma <- as.numeric(VarCorr(wtlen.lme1)[1,2])
print(wtlen.lme1$coefficients$random$Subject)
re<-wtlen.lme1$coefficients$random$Subject/re.sigma  ##/0.6184
qqnorm(re,ylab="Quantiles of random effects")
qqline(re)
hist(wtlen.lme1$coefficients$random$Subject/re.sigma,xlab="random effects",main="")