setwd("")
mydata=read.csv("customersurvey.csv",header=TRUE)
#install.packages("nFactors")
library(nFactors)
mydata
names(mydata)
summary(mydata)
mydata[,2]
mynewdata=mydata[,2:12]
pairs(mynewdata)
myfulldata=mydata[,2:13]
cormatbig=cor(myfulldata)
cormatbig
cormatsmall=cor(mynewdata)
myeigenvalues=eigen(cormatsmall)
myeigenvalues$values
myeigenvalues$vectors
factorids=c(1:11)
screeplot=plot(data.frame(factorids,myeigenvalues$values),col="Blue")
lines(data.frame(factorids,myeigenvalues$values),col="Blue")
unrotatematrix=principal(mynewdata,nfactors = 4,rotate="none")
unrotatematrix
unrotateplot=plot(unrotatematrix,row.names(unrotatematrix$loadings))
rotatematrix=principal(mynewdata,nfactors = 4,rotate = "varimax")
rotatematrix
rotateplot=plot(rotatematrix,row.names(rotatematrix$loadings),cex=1.0)
moredata=rotatematrix$scores
moredata
attach(moredata)
moredata=data.frame(moredata,mydata$Satisfaction)
mymodel=lm(moredata$mydata.Satisfaction~moredata$RC1+moredata$RC2+moredata$RC3+moredata$RC4,data=moredata)
moredata
summary(mymodel)
anova(mymodel)
#myfulldata$ComPricing=10-myfulldata$ComPricing
# myfulldata$ComPricing
#mynewdata$ComPricing=10-mynewdata$ComPricing
# rotatematrix=principal(mynewdata,nfactors = 4,rotate = "varimax")
# rotatematrix
plot(mymodel)
predictedval=predict(mymodel)
actual=moredata$mydata.Satisfaction
backtrack=data.frame(actual,predictedval)
plot(actual,col="Red")
lines(actual,col="Red")
plot(predictedval,col="Blue")
lines(predictedval,col="Blue")
confint(mymodel)
