#HousePrice Problem week 4
HousePrice_df = read.csv("HousePrices.csv")
dim(HousePrice_df)
names(HousePrice_df)
attach(HousePrice_df)
model1 = lm(Price~SqFt)
summary(model1)
model2 = lm(Price~SqFt+Bedrooms+Bathrooms+Offers+Brick+Neighborhood)
summary(model2)

##double check the data for consistency
dim(data);
names(data);

##running three different regression models
reg1 <- lm(Price~SqFt); summary(reg1);

reg2 <- lm(Price~SqFt+Bedrooms+Bathrooms); summary(reg2); 

reg3 <- lm(Price~SqFt+Bedrooms+Bathrooms+Offers+Brick+Neighborhood); summary(reg3); 

############################################################################################
##checking outliers and model assumptions; tools for that are contained in the library "car"
############################################################################################
library(car);

##Outliers and Influential points: Influence Plot
influencePlot(reg1, id.method="identify");


##Checking Model Assumption Diagnostics


##Normality - Histogram and QQ-Plot
par(mfrow=c(1,2))
hist(reg1$res,freq=F,main="Histogram of Residuals", xlab="");
##overlay nonparametric kernel density
lines(density(reg1$res),col="red",lwd=2,lty=2);
##overlay true normal distribution
xfit<-seq(min(reg1$res),max(reg1$res),length=50)
yfit<-dnorm(xfit,mean=mean(reg1$res),sd=sd(reg1$res))
lines(xfit, yfit, col="blue", lwd=2)
##now add QQ Plot
qq.plot(reg1$res,main="QQ Plot of Residuals",
        ylab="Residuals",xlab="Normal Quantiles");



##Check constant Variance with smooth trend line
par(mfrow=c(1,1));
plot(reg1$res~reg1$fit,ylab="Residuals",xlab="Fitted");
lines(smooth.spline(reg1$res~reg1$fit,df=6),col=2);


##Check correlated Errors
par(mfrow=c(2,1))
plot(reg1$res~SqFt,type="p",ylab="Residuals");
acf(reg1$res)


png("plot.png")
dev.off()






