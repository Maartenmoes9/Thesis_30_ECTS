library("readxl")
library("forecast")
library("FinTS")
library("lmtest")
library("sandwich")
library("rugarch")
library("urca")
library("tseries")
library("xts")
library("fGarch")
library("PerformanceAnalytics")
library(ggplot2)
df <- read.csv("C:/Users/maart/OneDrive/Skrivebord/CloneCloneClone/Thesis_30_ECTS/SP_SC.csv")
#Now first creating the log returns
log_returns <- diff(log(df$Adj.Close))

#Plotting the log returns 
plot(log_returns, type = "l", xlab = "Date", ylab = "Daily Returns",
     main = "Daily Returns Over Time", col = "blue")
#Creating the basic dummy
df$dummy <- as.integer(df$reg_output < 0)

#Squaring the scores
df$reg_output_squared <- df$reg_output^2

#Multiplication Dummies 
df$Multiplication_dummy <- df$reg_output_squared * as.integer(df$reg_output < 0)
df <- df[complete.cases(df), ]


#Stationary 
##Augmented Dicky Fuller test 
ADF_Returns = ur.df(log_returns, type = "drift",selectlags = "AIC" )
#summary of the test is that the absolute value of test statistics is higher than the three
# values. So the returns of Tesla is stationary 
summary(ADF_Returns)
##Time Series Plot 

#Check For Presence Of Volatility 
# plot returns with squared and absolute returns
dataToPlot = cbind(log_returns, log_returns^2, abs(log_returns))
colnames(dataToPlot) = c("Returns", "Returns^2", "abs(Returns)")
plot.zoo(dataToPlot, main="Tesla Daily Returns", col="blue")
#Check For Normality 
#histogram with normal density curve
options(repr.plot.width=21, repr.plot.height=11)#to set the figure size
hist(log_returns,prob=T,breaks=50,xlab="Daily Returns",main = "Tesla returns and the Normal Distribution",
     ylab="Probabillty Distribution",col="cornflowerblue", cex.lab=1.5, cex.axis=1.7,cex.main=2.3) 
mu<-mean(log_returns)  
sigma<-sd(log_returns)
x<-seq(min(log_returns),max(log_returns),length=80) 
y<-dnorm(x,mu,sigma) 
lines(x,y,lwd=2,col="red")  
##QQ-plot
qqnorm(log_returns, main = "Tesla Daily Returns -QQ Plot", col = "blue")
qqline(log_returns)
#Check For Arch Effect 
# plot autocorrelations of returns, returns^2 and abs(returns)
options(repr.plot.width=15, repr.plot.height=5)
par(mfrow=c(1,3))
acf(log_returns, main="Tesla Returns",cex.main=10)
acf(log_returns^2, main="Tesla Returns^2",cex.main=10)
acf(abs(log_returns), main="Tesla abs(Returns)",cex.main=10)
par(mfrow=c(1,1))
#The acf shows some forms of autocorrelation
# use Ljung Box.test from stats package to check auto correlation in sqrt returns
Box.test(coredata(log_returns^2), type="Ljung-Box", lag = 12)
#H0 is rejected which tells us the data is not independent, so there is 
#autocorrelation present
#ARCH LM Test
ArchTest(log_returns)
#H0 is rejected which tells us Tesla returns exhibit ARCH effects 
#Specify the GARCH(1,1) student T model with the exogenous variable
garch_std.exo.spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                        external.regressors = as.matrix(df[, "reg_output_squared"])),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)
# Fit the model
garch_std.exo.fit <- ugarchfit(garch_std.exo.spec, data = log_returns)

# Print the results
print(garch_std.exo.fit)


# Specify the GJR-GARCH(1,1,1) model with the exogenous variables
spec.gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1, 1), external.regressors = as.matrix(df[, c("reg_output_squared", "Multiplication_dummy")])),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)


# Fit the model
gjr.fit <- ugarchfit(spec.gjr, data = log_returns)

# Print the results
print(gjr.fit)


# Specify the GARCH(1,1) model without exogenous variables
garch_normal.spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)

# Fit the model
garch_normal.fit <- ugarchfit(garch_normal.spec, data = log_returns)

# Print the results
print(garch_normal.fit)



# Specify the GARCH(1,1) model with the exogenous variable "reg_output_squared"
garch.spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = as.matrix(df[, "reg_output_squared"])),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)


# Fit the model
garch.fit <- ugarchfit(garch.spec, data = log_returns)

# Print the results
print(garch.fit)

# Specify the EGARCH(1,1) model with the exogenous variables
egarch.spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1), external.regressors = as.matrix(df[, c("reg_output_squared", "Multiplication_dummy")])),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)


# Fit the model
egarch.fit <- ugarchfit(egarch.spec, data = log_returns)

# Print the results
print(egarch.fit)

# Specify the APARCH(1,1) model with the exogenous variables
aparch.spec <- ugarchspec(
  variance.model = list(model = "apARCH", garchOrder = c(1, 1), external.regressors = as.matrix(df[, c("reg_output_squared", "Multiplication_dummy")])),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)


# Fit the model
aparch.fit <- ugarchfit(aparch.spec, data = log_returns)

# Print the results
print(aparch.fit)




# Model selection using information criterion
model.list <- list("egarch(1,1)" = egarch.fit,
                   "garch(1,1)" = garch.fit,
                   "gjr(1,1)" = gjr.fit,
                   "aparch(1,1)"=aparch.fit,
                   "garch_normal (1,1)" = garch_normal.fit,
                  "garch_studentT (1,1)" =garch_std.exo.fit)
info.mat <- sapply(model.list, infocriteria)
rownames(info.mat) <- rownames(infocriteria(garch.fit))
info.mat


# Create a data frame with log returns and fitted values from each model
df <- data.frame(log_returns = log_returns,
                 egarch = fitted(egarch.fit),
                 garch = fitted(garch.fit),
                 gjr = fitted(gjr.fit),
                 aparch = fitted(aparch.fit),
                 garch_normal = fitted(garch_normal.fit),
                 garch_std_exo = fitted(garch_std.exo.fit))

# Plot log returns and fitted values from each model
ggplot(df, aes(x = 1:length(log_returns))) +
  geom_line(aes(y = log_returns, color = "Log Returns")) +
  geom_line(aes(y = egarch, color = "EGARCH(1,1)")) +
  geom_line(aes(y = garch, color = "GARCH(1,1)")) +
  geom_line(aes(y = gjr, color = "GJR-GARCH(1,1)")) +
  geom_line(aes(y = aparch, color = "APARCH(1,1)")) +
  geom_line(aes(y = garch_normal, color = "GARCH-Normal(1,1)")) +
  geom_line(aes(y = garch_std_exo, color = "GARCH-Std(1,1)")) +
  labs(x = "Time", y = "Returns") +
  scale_color_manual(name = "Models",
                     values = c("Log Returns" = "black",
                                "EGARCH(1,1)" = "red",
                                "GARCH(1,1)" = "blue",
                                "GJR-GARCH(1,1)" = "green",
                                "APARCH(1,1)" = "purple",
                                "GARCH-Normal(1,1)" = "orange",
                                "GARCH-Student-T(1,1)" = "brown"))

#various plots for fitted values
#options(repr.plot.width=15, repr.plot.height=15)
#plot(gjr.fit, which= "all")

