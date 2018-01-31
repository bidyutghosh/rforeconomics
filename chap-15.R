# To import the dataset (blaisdell.txt) and save it as an object "blaisdell"
blaisdell<-read.table(file.choose(),header = TRUE)
# To view the dataset
View(blaisdell)
# To attach the dataset into R-session
attach(blaisdell)
# To fit the initial model to explain company sales with industry sales
fit1<-lm(comsales~indsales)
summary(fit1)

# To save the residuals(uhat1) of the model 
blaisdell$uhat1<-resid(fit1)
attach(blaisdell)

# To plot the residuals against time
plot(uhat1, type = "l")
library(ggplot2)
ggplot(blaisdell, aes(time, uhat1))+geom_line()

# To conduct the Durbin-Watson test
library(car)
durbinWatsonTest(fit1)

# To adopt the Cochrane-Orcutt Procedure and save it as "fit2"
library(orcutt)
fit2<-cochrane.orcutt(fit1)
summary(fit2)

# To run the regressioin of the first-difference transformation model with no intercept term
fit3<-lm(diff(comsales,1)~0+diff(indsales,1))
summary(fit3)

# To apply Durbin-Watson test on the first-difference model
durbinWatsonTest(fit3)


## Advanced Autocorrelation Problem using the dataset "us_consumption.xls"
# Importing the dataset using "readxl" package
library(readxl)
us_consumption<-read_excel(file.choose(), sheet = "Sheet1", col_names = TRUE,col_types = NULL, skip = 0)
# To view the dataset
View(us_consumption)
# To attach the dataset into Rsession
attach(us_consumption)

# Now we run the model to explain logarithm of consumption using logarithms of X variables in R and the results are shown below:
fit4<- lm(lnconsump~lndpi+lnwealth+interest)
summary(fit4)


# To save the residuals of the model fit
us_consumption$uhat1<-ts(resid(fit4))
attach(us_consumption)

# To plot the residuals of the model againt time
plot(uhat1, type="l")
library(ggplot2)
ggplot(us_consumption, aes(time, uhat1))+geom_line()

# To create the one-period lag of the saved residuals
us_consumption$luhat1<-lag(uhat1, -1)

# To plot the Current vs Lagged Residuals using basic graphics
attach(us_consumption)
plot(uhat1[1:54], luhat1[2:55], pch="*", main = "Current vs. Lagged Residuals")
abline(lm(luhat1[2:55]~uhat1[1:54]), col="red")

# To plot the Current vs Lagged Residuals using advanced graphics
ggplot(us_consumption, aes(luhat1[2:55], uhat1[1:54]))+geom_point()+geom_smooth(method = "lm", se=FALSE)

# To conduct the Durbin-Watson test
library(car)
durbinWatsonTest(fit4)

# To perform the Breusch–Godfrey (BG) test of autocorrelation.
library(lmtest)
bgtest(fit4, order = 1, type = "Chisq")

# Remedial Measures for Autocorrelation: Using the first difference method
fit5<- lm(diff(lnconsump,1)~0+diff(lndpi,1)+diff(lnwealth,1)+diff(interest,1))
summary(fit5)


# Breusch–Godfrey (BG) test of autocorrelation for the first difference model
bgtest(fit5,order = 1, type = "Chisq")

# Remedial Measures for Autocorrelation: Generalized Transformation
# To save the residuals of the initial model (fit4)
uhat4<-resid(fit4)

# To generate the one-period lag of the residuals.
library(dplyr)
us_consumption$luhat4<-lag(uhat4, n=1L)

# For estimating the rho
fit6<-lm(uhat4~0+luhat4)
summary(fit6)
# Thus, rho hat is found to be 0.3247. Further, we can recheck it approximately using D-W statistic
durbinWatsonTest(fit4)

# Complete transformation
us_consumption$Cstar<- lnconsump-0.3247*lag(lnconsump,n=1L)
us_consumption$Ystar<-lndpi-0.3247*lag(lndpi,n=1L)
us_consumption$Wstar<-lnwealth-0.3247*lag(lnwealth,n=1L)
us_consumption$rstar<-interest-0.3247*lag(interest,n=1L)
attach(us_consumption)
# To estimate the transformed model  
fit7<-lm(Cstar~Ystar+Wstar+rstar)
summary(fit7)
