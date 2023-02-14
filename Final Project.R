library(readxl)    
library(corrplot)
library(TSstudio)
library(tseries)
library(texreg)
library(dynamac)
library(astsa)
library(forecast)
library(ggplot2)




mydata <- read_excel("/Users/mohibullahfaqeerzai/Desktop/Data.xlsx", sheet = "Spread" )
head(mydata)
attach(mydata)


mydata$Date <- NULL              # remove the date column
df.ts <- ts(mydata, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)    
summary(df.ts)


############################## correlation of variables #####################
cor(df.ts, method = "pearson")

corr_dataset <- data.frame(ShortTerm, MedTerm, LongTerm, T10Y2Y, T10Y3M, INDPRO)
corr_dataset <- ts(corr_dataset, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
corrplot.mixed(cor(corr_dataset),
               lower = "number",
               upper = "circle",
               tl.col = "black")


# plotting all the variables to observe stationary and non stationary variables
plot(df.ts)
plot(corr_dataset)



INDPRO.ts <- ts(mydata$INDPRO, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
T10Y3M.ts <- ts(mydata$T10Y3M, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
T10Y2Y.ts <- ts(mydata$T10Y2Y, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
ShortTerm.ts <- ts(mydata$ShortTerm, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
MedTerm.ts <- ts(mydata$MedTerm, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
LongTerm.ts <- ts(mydata$LongTerm, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)
EBP.ts <- ts(mydata$EBP, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)



###################################### Plots ##############################
govtbonds <- cbind(T10Y3M.ts, T10Y2Y.ts)
ts_plot(govtbonds, 
        title = "Yield Spread Government Bonds",
        Xtitle = "Maturity",
        Ytitle = "Yield"
        )

gov.cor <- cbind(ShortTerm.ts, MedTerm.ts, LongTerm.ts)
ts_plot(gov.cor,
        title = "Spread between Government and Corporate Bonds",
        Xtitle = "Maturity",
        Ytitle = "Yield"
        )

indspr <- cbind(T10Y3M.ts, INDPRO_PC)
ts_plot(indspr, 
        title = "Industrial Production and 10Y3M Spread",
        Xtitle = "Period",
        Ytitle = "Yield"
)



###################################################################
##################### Recession Plot #############################

ts.plot(indspr, gpars= list(col=rainbow(8)), abline = 0, ylab = "Yield")

par(mar=c(5, 5, 6, 3), xpd=TRUE) 
legend("top", inset = c(2, -0.30), legend = c("10Y3M Spread", "INDPRO"), cex = 0.5, col=rainbow(9), lty = c(1,1))

min=min(T10Y3M.ts, INDPRO_PC)
max=max(T10Y3M.ts, INDPRO_PC)

shade_height1=c(min,max)

polygon(x=c(2001.25,2001.25, 2001.75,2001.75),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)
polygon(x=c(2008,2008, 2009.25,2009.25),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)
polygon(x=c(2020.25,2020.25, 2020.50,2020.50),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)

##############################################################################
dataaaa <- data.frame(MedTerm.ts, INDPRO_PC)
dataaaa <- ts(dataaaa, start = c(1999,1,1), end = c(2021,3,1), frequency = 12)

ts.plot(dataaaa, gpars= list(col=rainbow(8)), abline = 0, ylab = "Yield")

par(mar=c(5, 5, 6, 3), xpd=TRUE) 
legend("top", inset = c(2, -0.30), legend = c("MedTerm", "INDPRO"), cex = 0.5, col=rainbow(9), lty = c(1,1))

min=min(MedTerm.ts, INDPRO_PC)
max=max(MedTerm.ts, INDPRO_PC)

shade_height1=c(min,max)

polygon(x=c(2001.25,2001.25, 2001.75,2001.75),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)
polygon(x=c(2008,2008, 2009.25,2009.25),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)
polygon(x=c(2020.25,2020.25, 2020.50,2020.50),
        y=c(shade_height1, rev(shade_height1)),
        density=NA, col=rgb(0, 0, 0,0.25), border=NA)



######################################################

# ADF test to check stationary and non stationary
adf.test(INDPRO.ts)
adf.test(ShortTerm.ts)
adf.test(MedTerm.ts)
adf.test(LongTerm.ts)
adf.test(T10Y3M.ts)
adf.test(T10Y2Y.ts)
adf.test(EBP.ts)




# taking log difference for INDPRO and differences for other variables to make it stationary 
DINDPRO <- 100 * diff(log(INDPRO))
DSterm <- diff(ShortTerm)
DMterm <- diff(MedTerm)
DLterm <- diff(LongTerm)
DT10Y3M <- diff(T10Y3M)
DT10Y2Y <- diff(T10Y2Y)



# creating a data frame for variables after making it stationary

df2 <- data.frame(ShortTerm[-1], MedTerm[-1], LongTerm[-1], DINDPRO, DT10Y3M, DT10Y2Y)

timeseries.df <- as.ts(df2)

# To check the variables if stationary
plot(timeseries.df)


# ADF test
adf.test(DINDPRO)
adf.test(DSterm)
adf.test(DMterm)
adf.test(DLterm)
adf.test(DT10Y3M)
adf.test(DT10Y2Y)


############################ Regression (1) ########################

# Taking lag of variables
lag_DINDPRO <- lag(DINDPRO, k = 1)
lag_DSterm <- lag(DSterm, k = 1)
lag_DMterm <- lag(DMterm, k = 1)
lag_DLterm <- lag(DLterm, k = 1)
lag_DT10Y3M <- lag(DT10Y3M, k = 1)
lag_DT10Y2Y <- lag(DT10Y2Y, k = 1)
lag_EBP <- lag(EBP, k = 1)

lag_Sterm <- lag(ShortTerm, k = 1)
lag_Mterm <- lag(MedTerm, k = 1)
lag_Lterm <- lag(LongTerm, k = 1)
lag_T10Y2Y <- lag(T10Y2Y, k = 1)
lag_T10Y3M <- lag(T10Y3M, k = 1)




# regression

reg1 <- lm(DINDPRO ~ lag_DT10Y3M)
reg2 <- lm(DINDPRO ~ lag_DT10Y2Y)
reg3 <- lm(DINDPRO ~ lag_Sterm[-1])
reg4 <- lm(DINDPRO ~ lag_Mterm[-1])
reg5 <- lm(DINDPRO ~ lag_Lterm[-1])
reg6 <- lm(DINDPRO ~ lag_T10Y3M[-1])
reg7 <- lm(DINDPRO ~ lag_T10Y2Y[-1])


screenreg(list(Regression1 = reg1, Regression2 = reg2, 
               Regression3 = reg3, Regression4 = reg4, Regression5 = reg5,
               Regression6 = reg6, Regression7 = reg7),
          digits = 3, omit.coef = "Intercept", include.loglik = FALSE, 
          include.nobs = FALSE)


reg8 <- lm(DINDPRO ~  lag_Sterm[-1] + lag_DT10Y3M + lag_DT10Y2Y)
reg9 <- lm(DINDPRO ~ lag_DT10Y3M + lag_DT10Y2Y)

screenreg(list(Regression8 = reg8, Regression9 = reg9),
          digits = 3, omit.coef = "Intercept", include.loglik = FALSE, 
          include.nobs = FALSE)




# from the results we can see that in simple regression the current value and one lag of the 
# explanatory variables cannot explain the variation in our dependent variable (INDPRO) 
# so it might take effect in longer than one lag so we need to use ARDL which will look at 
# many lags impact on dependent variable.

################################### ARDL Model (2) ########################
# 

# In its basic form, an ARDL regression model looks like this:
  
#  yt = β0 + β1yt-1 + .......+ βpyt-p + α0xt + α1xt-1 + α2xt-2 + ......... + αqxt-q + εt

# where εt is a random "disturbance" term.


modelARDL <- dynardl(DINDPRO ~ DT10Y2Y + ShortTerm,
                  lags = list("DINDPRO" = 1, "DT10Y2Y" = c(1,3,6,9), "ShortTerm" = c(1,3,6,9)), # want to see all lags
                  ec = FALSE) # if dependent variable is non stationary and want it in difference then it should be TRUE but here difference is already taken so we need it in levels


summary(modelARDL)
# the summary show that the conventional predictor (DT10Y2Y) and its historical values (lags) 
# cannot explain the variation in economic growth so our new explanatory variable is much significant
# and is much better than conventional one so it can replace the old one. 

# lag 1 of DINDPRO is sig
# lag 1 of DMterm is sig
# lag 6 of DMterm is sig which can indicate some seasonality
# all others are not significant



# The summary show that one lag is optimal so we can use one lag in SARIMA






######################## Forecast (3) #######################
acf(DINDPRO)
pacf(DINDPRO)
# ACF and PACF
acf2(DINDPRO)
acf2(INDPRO)


# in SARIMA model we use AR(1) and add exogeneous variables i.e. xreg = 
modelSARIMA1 <- sarima(DINDPRO, p = 2, d = 0, q = 0, P = 0, D = 0, Q = 0, S = 0,
                       xreg = cbind(lag_DT10Y2Y, lag_DT10Y3M))
summary(modelSARIMA1)

# results
modelSARIMA1$ttable

modelSARIMA2 <- sarima(DINDPRO, p = 2, d = 0, q = 0, P = 0, D = 0, Q = 0, S = 0, 
                       xreg = cbind(lag_Sterm[-1], lag_DT10Y2Y, lag_DT10Y3M))
summary(modelSARIMA2)
# let's check out just the parameters using the different estimation methods

# results
modelSARIMA2$ttable


Forecast1 <- sarima.for(DINDPRO, p = 2, d = 0, q = 0, n.ahead = 12, 
            newxreg = cbind(lag_DT10Y2Y, lag_DT10Y3M), plot.all = TRUE)
title(main = "Forecast1")


Forecast2 <- sarima.for(DINDPRO, p = 2, d = 0, q = 0, n.ahead = 12, 
                        newxreg = cbind(lag_Sterm[-1], lag_DT10Y2Y, lag_DT10Y3M),
                        plot.all = TRUE)
title(main = "Forecast2")





########################

demo_model <- auto.arima(DINDPRO)
demo_model


plot.ts(demo_model$residuals)

demo_forecast <- forecast(demo_model, level = c(95), h = 10)
plot(demo_forecast)








