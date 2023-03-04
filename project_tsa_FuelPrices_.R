#Stori Matteo: matteo.stori01@icatt.it
#Esposito Francesco: francesco.esposito06@icatt.it
#Lanubile Fabrizio: fabrizio.lanubile01@icatt.it
#Marchesi Serena: serena.marchesi02@icatt.it

#The dataset contains the weekly fuel prices in Italy from March 2005 to August 2021, but we are looking only for the prices from 2013 till now, in order to carry on an appropriate analysis of this time series.
#Prices are ordered according to time and are multiplied by a factor of 1000 to have a more precise accuracy on the decimal numbers.
#All the indicated prices are referring to self service prices, and for each week, we have also the references to the IVA tax.
#All the prices are available on the italian government site "Ministero dell'ambiente e della sicurezza energetica".

library(readxl)

# Loading the raw dataset
#df <- read_excel("C:/Users/kecco/Documents/CATTOLICA/TIME SERIES/weekly_fuel_prices_all_data_from_2005_to_20210823.xlsx")
#df <- read_excel("C:/Users/matte/OneDrive/Desktop/TSA and Spatial/project/weekly_fuel_prices_all_data_from_2005_to_20210823.xlsx")
df <- read_excel("C:/Users/seren/OneDrive/Desktop/weekly_fuel_prices_all_data_from_2005_to_20210823.xlsx")

# Checking the correctness of the dataframe structure
str(df)

# Converting the "PRICE" column as numeric 
df$PRICE <- as.numeric(as.character(df$PRICE), na.rm = TRUE)

# Subsetting the whole dataframe by "PRODUCT_NAME" column
df_EuroSuper95 <- df[df$PRODUCT_NAME == "Euro-Super 95", ]
df_Automotive_gasoil <- df[df$PRODUCT_NAME == "Automotive gas oil", ]
df_Heating_gasoil <- df[df$PRODUCT_NAME == "Heating gas oil", ]
df_LPG <- df[df$PRODUCT_NAME == "LPG", ]
df_Residual_fueloil <- df[df$PRODUCT_NAME == "Residual fuel oil", ]
df_Heavy_fueloil <- df[df$PRODUCT_NAME == "Heavy fuel oil", ]

#Now we have 6 different dataframes

#EXPLORATORY ANALYSIS

#Comparing all the different types of fuel by their distribution

#x11();
par(mfrow = c(2,3))

# EURO95 --- Histogram of the "PRICE" column
hist(df_EuroSuper95$PRICE, main = "Euro-Super 95",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_EuroSuper95$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_EuroSuper95$PRICE)
sigma <- sd(df_EuroSuper95$PRICE)
x <- seq(min(df_EuroSuper95$PRICE), max(df_EuroSuper95$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

# AUTOMOTIVE_GASOIL -- Histogram of the "PRICE" column
hist(df_Automotive_gasoil$PRICE, main = "Automative Gasoil",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_Automotive_gasoil$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_Automotive_gasoil$PRICE)
sigma <- sd(df_Automotive_gasoil$PRICE)
x <- seq(min(df_Automotive_gasoil$PRICE), max(df_Automotive_gasoil$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

# HEATING GASOIL --- Histogram of the "PRICE" column
hist(df_Heating_gasoil$PRICE, main = "Heating Gasoil",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_Heating_gasoil$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_Heating_gasoil$PRICE)
sigma <- sd(df_Heating_gasoil$PRICE)
x <- seq(min(df_Heating_gasoil$PRICE), max(df_Heating_gasoil$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

# HEAVY FUEL OIL --- Histogram of the "PRICE" column
hist(df_Heavy_fueloil$PRICE, main = "Heavy Fue Oil",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_Heavy_fueloil$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_Heavy_fueloil$PRICE)
sigma <- sd(df_Heavy_fueloil$PRICE)
x <- seq(min(df_Heavy_fueloil$PRICE), max(df_Heavy_fueloil$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

# LPG --- Histogram of the "PRICE" column
hist(df_LPG$PRICE, main = "LPG",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_LPG$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_LPG$PRICE)
sigma <- sd(df_LPG$PRICE)
x <- seq(min(df_LPG$PRICE), max(df_LPG$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

# RESIDUAL FUEL OIL --- Histogram of the "PRICE" column
hist(df_Residual_fueloil$PRICE, main = "Residual Fuel Oil",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(df_Residual_fueloil$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(df_Residual_fueloil$PRICE)
sigma <- sd(df_Residual_fueloil$PRICE)
x <- seq(min(df_Residual_fueloil$PRICE), max(df_Residual_fueloil$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

#Since EURO-SUPER 95 is the most similar distribution to the Gaussian one, we selected it for our further analysis
#We want to look at only at data from 2013 since in the 2008-2012 there was a big recession that could potentially make more complicate our analysis.

#CREATION OF THE DATASET WITH date >= 2013 and product_ID = 1

df$SURVEY_DATE <- as.Date(df$SURVEY_DATE, format = "%d/%m/%Y")

euro95_cut <- subset(df, format(df$SURVEY_DATE, "%Y") >= "2013")
euro95_cut <- subset(euro95_cut, euro95_cut$PRODUCT_ID == '1')

#View(euro95_cut)


# EURO95 --- Histogram of the "PRICE" column
#x11(); 
par(mfrow = c(2,2))
hist(euro95_cut$PRICE, main = "Histogram of Euro-Super 95 Prices",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(euro95_cut$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(euro95_cut$PRICE)
sigma <- sd(euro95_cut$PRICE)
x <- seq(min(euro95_cut$PRICE), max(euro95_cut$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)


boxplot(euro95_cut$PRICE, main = "Box plot of Euro-Super 95 Prices")

plot(euro95_cut$SURVEY_DATE, euro95_cut$PRICE, main = "Scatter plot of Euro-Super 95 Prices",
     xlab = "Date")

plot(euro95_cut$SURVEY_DATE, euro95_cut$PRICE, type = "l", main = "Line plot of Euro-Super 95 Prices",
     xlab = "Date")

#Take the logs of the y's trying to make data more normal

# EURO95 --- Histogram of the "PRICE" column
euro95_cut$PRICE = log(euro95_cut$PRICE)

#x11(); 
par(mfrow = c(2,2))
hist(euro95_cut$PRICE, main = "Histogram of Euro-Super 95 Prices",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(euro95_cut$PRICE)
lines(d, col = "red", lwd = 2)
mu <- mean(euro95_cut$PRICE)
sigma <- sd(euro95_cut$PRICE)
x <- seq(min(euro95_cut$PRICE), max(euro95_cut$PRICE), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)


boxplot(euro95_cut$PRICE, main = "Box plot of Euro-Super 95 Prices")

plot(euro95_cut$SURVEY_DATE, euro95_cut$PRICE, main = "Scatter plot of Euro-Super 95 Prices",
     xlab = "Date")
plot(euro95_cut$SURVEY_DATE, euro95_cut$PRICE, type = "l", main = "Line plot of Euro-Super 95 Prices",
     xlab = "Date")

#Centralization of the distribution

adj_price=euro95_cut$PRICE
mean = mean(euro95_cut$PRICE)
adj_price = (euro95_cut$PRICE) - mean
range(adj_price)


val_x=c()
val_y=c()
plot(adj_price,main="Adjusted prices",xlab="date",ylab="adjusted prices",xlim=c(0,430),ylim=c(-0.14,0.14),
     xaxt="n",yaxt="n",type="l",lwd=3)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(rep(mean(adj_price,na.rm=TRUE),times=430),col="red",lwd=2)

#Let's check the Normality of the distribution

#HISTOGRAM
#x11(); 
par(mfrow = c(1, 3))
hist(adj_price, main = "Histogram of adjusted prices",
     xlab = "Price", col = "lightblue", border = "black", probability = TRUE)
d <- density(adj_price)
lines(d, col = "red", lwd = 2)
mu <- mean(adj_price)
sigma <- sd(adj_price)
x <- seq(min(adj_price), max(adj_price), length.out = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2)

#BOXPLOT
boxplot(adj_price,main = "Box plot of adjusted prices") 

#QQ-PLOT
qqnorm(adj_price)
qqline(adj_price)

#In the histogram is evident that the red distribution (representing the trend of histograms and thus of our distribution) is quite far from being approximated to a normal one.
#The boxplot there is a good symmetry centred in zero, due to the centralization operation done above.
#The QQplot is useful to determine if a set of data come from some theoreical distribution. Seeing the tails, it is quite clear that the data are not normal.

#let's assume is normal and analyze by normal model and see if the results are good or not

shapiro.test(adj_price) #Shapiro-Wilk Normality - independent data!

#Jarque-Bera Normality - independent data!
library (tseries)
jarque.bera.test(adj_price)

#In the shapiro test the data are Normal if the p-value is above 0.05, in this case it is smaller than the threshold, so this test suggests that the data are not Normal. 
#In the jacques brera test the null hp is that the data are normal. Due to the fact that the p-value<0.05, the test provides results that our data are not normal. 
#So, as we see from the graphs and tests, it would be rash to say that our data are normally distributed despite the application of the 'log' function and centralization. Graphically there is some (faint) clue that may lead us to this assumption, but considering the test results we cannot conclude that our data are normal. This means that we will probably not have normally distributed residuals in our analysis.
#To develop an analysis on this data, we will assume Normality. 

#A first step to begin to understand which type of models our data best fit is to use the Autocorrelation function and Partial Autocorrelation function. 
#Respectively we will use: 
#  - ACF to identify the fit of the Moving Average model. Since the latter is a model with exponentially decreasing autocorrelation as lag increases, ACF is fit (not adjusted for the influence of intermediate lags). 
#- PACF to identify the fit of the Autoregressive Model. In this case, the autocorrelation of the model does not decay exponentially, so distant lags may have influential statistical significance, so PACF is used (adjusts the result for the influence of intermediate values). 

#ACF vs PACF
#x11(); 
par(mfrow = c(1,2))
acf(adj_price,lag.max=100,main="")
pacf(adj_price,lag.max=50,main="")

#From the results suggested by the previous functions, we can then begin to eliminate the MA model from the possible models applicable to our time series. 
#What PACF suggests instead is quite different. It seems that the AR model fits. It is also possible to point out that there are 3 possible significant lagged values, identifiable by the three spikes beyond the horizontal limit, and so the maximum order that can be considered in our models that we are going to test should be 3.  
#Recall that that horizontal limit is an approximation of 0, usually computed as 2/sqrt(n). 

#We can implement now the step of the Model Specification. 
#Assuming that our data are stationary, then we can start comparing different models. 
#It is reasonable to exclude comparisons with MA models of different order based on what has been pointed out by the ACF. Instead, it will be useful to compare different orders of AR models with a more complex model such as the ARMA model to see whether it is worth using higher complexity or not.  

#Moreover, for each model we will compute AIC and BIC. They are two fundamental tools used to chose the best model. 
#Both of them are based on the LogLikelihood function, which will be also useful for the next step, when we will estimate the parameters of the chosen model using the MLE method. 
#We will chose the model that provides us the lower AIC/BIC. 

n = length(adj_price)
AIC=rep(0,times=8)
BIC=rep(0,times=8)
AIC.tsa=rep(0,times=8)

## AR(1) ##

# Z_{t} = phi_{1} * Z_{t-1} + a_{t} #

dim=2
AR1=arima(adj_price,order=c(1,0,0),method='ML')

est.phi1=AR1$coef[1]
est.sigma2=AR1$sigma2

LogLik=-0.5*(n-1)*log(2*pi*est.sigma2)
for (t in 2:n) {
  LogLik=LogLik-0.5*((adj_price[t]-est.phi1*adj_price[t-1])^2)/est.sigma2
}

Deviance=-2*LogLik

AIC[1]=Deviance+2*dim
BIC[1]=Deviance+dim*log(n)
AIC.tsa[1]=AR1$aic

## AR(2) ##

# Z_{t} = phi_{1} * Z_{t-1} + phi_{2} * Z_{t-2} + a_{t} #

dim=3
AR2=arima(adj_price,order=c(2,0,0),method='ML')

est.phi1=AR2$coef[1]
est.phi2=AR2$coef[2]
est.sigma2=AR2$sigma2

LogLik=-0.5*(n-2)*log(2*pi*est.sigma2)
for (t in 3:n) {
  LogLik=LogLik-0.5*((adj_price[t]-est.phi1*adj_price[t-1]-est.phi2*adj_price[t-2])^2)/est.sigma2
}

Deviance=-2*LogLik

AIC[2]=Deviance+2*dim
BIC[2]=Deviance+dim*log(n)
AIC.tsa[2]=AR2$aic


## AR(3) ##

# Z_{t} = phi_{1} * Z_{t-1} + phi_{2} * Z_{t-2} + phi_{3} * Z_{t-3} + a_{t} #

dim=4

AR3=arima(adj_price,order=c(3,0,0),method='ML')
est.phi1=AR3$coef[1]
est.phi2=AR3$coef[2]
est.phi3=AR3$coef[3]
est.sigma2=AR2$sigma2

LogLik=-0.5*(n-3)*log(2*pi*est.sigma2)
for (t in 4:n) {
  LogLik=LogLik-0.5*((adj_price[t]-est.phi1*adj_price[t-1]-est.phi2*adj_price[t-2]
                      -est.phi3*adj_price[t-3])^2)/est.sigma2
}

Deviance=-2*LogLik

AIC[3]=Deviance+2*dim
BIC[3]=Deviance+dim*log(n)
AIC.tsa[3]=AR3$aic

## AR(4) ##

# Z_{t} = phi_{1} * Z_{t-1} + phi_{2} * Z_{t-2} 

#        + phi_{3} * Z_{t-3} + phi_{4} * Z_{t-4} + a_{t} #

dim=5

AR4=arima(adj_price,order=c(4,0,0),method='ML')

est.phi1=AR4$coef[1]
est.phi2=AR4$coef[2]
est.phi3=AR4$coef[3]
est.phi4=AR4$coef[4]
est.sigma2=AR4$sigma2

LogLik=-0.5*(n-4)*log(2*pi*est.sigma2)
for (t in 5:n) {
  LogLik=LogLik-0.5*((adj_price[t]-est.phi1*adj_price[t-1]-est.phi2*adj_price[t-2]
                      -est.phi3*adj_price[t-3]-est.phi4*adj_price[t-4])^2)/est.sigma2
}

Deviance=-2*LogLik

AIC[4]=Deviance+2*dim
BIC[4]=Deviance+dim*log(n)
AIC.tsa[4]=AR4$aic



# ARMA(1,1) #

# Z_{t} = phi1 * Z_{t-1} + a_{t} - theta1 * a_{t-1} #

dim=3
ARMA11=arima(adj_price,order=c(1,0,1),method='ML')

est.phi1=ARMA11$coef[1]
est.theta1=ARMA11$coef[2]
est.sigma2=ARMA11$sigma2

a.obs=rep(0,times=n)

LogLik=-0.5*(n-1)*log(2*pi*est.sigma2)
for (t in 2:n) {
  a.obs[t]=adj_price[t]-est.phi1*adj_price[t-1]-est.theta1*a.obs[t-1]
  LogLik=LogLik-0.5*(a.obs[t]^2)/est.sigma2
}

Deviance=-2*LogLik

AIC[5]=Deviance+2*dim
BIC[5]=Deviance+dim*log(n)
AIC.tsa[5]=ARMA11$aic

# ARMA(2,1) #

# Z_{t} = phi1 * Z_{t-1} + phi2 * Z_{t-2} + a_{t} + theta1 * a_{t-1} #

dim=4
ARMA21=arima(adj_price,order=c(2,0,1),method='ML')

est.phi1=ARMA21$coef[1]
est.phi2=ARMA21$coef[2]
est.theta1=ARMA21$coef[3]
est.sigma2=ARMA21$sigma2

a.obs=rep(0,times=n)

LogLik=-0.5*(n-2)*log(2*pi*est.sigma2)
for (t in 3:n) {
  a.obs[t]=adj_price[t]-est.phi1*adj_price[t-1]-est.phi2*adj_price[t-2]-est.theta1*a.obs[t-1]
  LogLik=LogLik-0.5*(a.obs[t]^2)/est.sigma2
}
Deviance=-2*LogLik

AIC[6]=Deviance+2*dim
BIC[6]=Deviance+dim*log(n)
AIC.tsa[6]=ARMA21$aic

# ARMA(1,2) #

# Z_{t} = phi1 * Z_{t-1} + a_{t} + theta1 * a_{t-1} + theta2 * a_{t-2} #

dim=4
ARMA12=arima(adj_price,order=c(1,0,2),method='ML')

est.phi1=ARMA12$coef[1]
est.theta1=ARMA12$coef[2]
est.theta2=ARMA12$coef[3]
est.sigma2=ARMA12$sigma2

a.obs=rep(0,times=n)

LogLik=-0.5*(n-2)*log(2*pi*est.sigma2)
for (t in 3:n) {
  a.obs[t]=adj_price[t]-est.phi1*adj_price[t-1]-est.theta1*a.obs[t-1]-est.theta2*a.obs[t-2]
  LogLik=LogLik-0.5*(a.obs[t]^2)/est.sigma2
}
Deviance=-2*LogLik

AIC[7]=Deviance+2*dim
BIC[7]=Deviance+dim*log(n)
AIC.tsa[7]=ARMA12$aic

# ARMA(2,2) #

# Z_{t} = phi1 * Z_{t-1} + phi1 * Z_{t-1} + a_{t} 

#+ theta1 * a_{t-1} + theta2 * a_{t-2} #

dim=5
ARMA22=arima(adj_price,order=c(2,0,2),method='ML')

est.phi1=ARMA22$coef[1]
est.phi2=ARMA22$coef[2]
est.theta1=ARMA22$coef[3]
est.theta2=ARMA22$coef[4]
est.sigma2=ARMA22$sigma2

a.obs=rep(0,times=n)

LogLik=-0.5*(n-2)*log(2*pi*est.sigma2)
for (t in 4:n) {
  a.obs[t]=adj_price[t]-est.phi1*adj_price[t-1]-est.phi2*adj_price[t-2]
  -est.theta1*a.obs[t-1]-est.theta2*a.obs[t-2]
  LogLik=LogLik-0.5*(a.obs[t]^2)/est.sigma2
}

Deviance=-2*LogLik
AIC[8]=Deviance+2*dim
BIC[8]=Deviance+dim*log(n)
AIC.tsa[8]=ARMA22$aic

AIC
BIC
AIC.tsa

min(AIC)		# -3323.873   ARMA(2,1)
min(BIC)	  # -3307.818   AR(2)
min(AIC.tsa) # -3328 ARMA(2,1)

#The model comparison gives us the following result: the model that minimize the BIC is AR(2). Now we should
#use it to go further in the analysis and proceeding in the estimetion parameters step, but...


#After performing the Model Specification having assumed that our data are stationary, we can improve try to improve our analysis by going deeper and performing a test that allows us to understand whether the data are effectively stationary or nonstationary.
#Recall that stationarity means that the data maintain the same characteristics in the temporal distribution (the same distribution if we are talking about strictly stationary or the same covariances if we are talking about weakly stationary). 


#TEST STATIONARITY: Augmented Dickey Fuller Test 
#Will be adopted a statistical test used to check the stationarity.

library(tseries) 
x <- ts(euro95_cut$PRICE)
adf.test(x)

#When the test statistic is lower than the critical value shown, you reject the null hypothesis and infer that the time series is stationary. This is NOT the case. As we can see, the p-value is greater than 0.05, so the data are not stationary. 

#In this case we can use the ARIMA model, with the integration operation which is used to remove the non stationarity of the data. It is based on the differentiation between a value of the series and the previous one. 
#The order 'd' which will be put in the function ARIMA(p, d, q) means how many times the integration operation will be done.  
#Before applying the function, we develop the analytical code in order to execute de integration operation and then
#plot the data. This is good to understand the efficiency of the differentiation and also to replicate the Augmented Dickey Fuller Test checking the stationarity.

n=length(adj_price)
adj_price.diff=rep(NA,times=n)		
# 2:n not 1:n-1
for (t in 2:n) {			# z[t]=z[t+1]-z[t]
  adj_price.diff[t]=adj_price[t]-adj_price[t-1]
}

mean=mean(adj_price.diff,na.rm=TRUE)

mean

adj_price.diff=adj_price.diff-mean
mean(adj_price.diff,na.rm=TRUE)

range(adj_price.diff,na.rm=TRUE)

val_x=c()
val_y=c()
plot(adj_price.diff,main="Adjusted prices",xlab="date",ylab="adjusted prices",xlim=c(0,435),ylim=c(-0.040,0.026),
     xaxt="n",yaxt="n",type="l",lwd=3)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(rep(mean(adj_price.diff,na.rm=TRUE),times=435),col="red",lwd=2)

#It is clear that the distribution is quite better than the initial distributio. It extends more neatly through the mean horizontal line.


adj_price.diff[1] = -1.022099e-19 #replace the first element (NA) with the mean of the new distribution in order to compute the test

library(tseries) 
x <- ts(adj_price.diff)
adf.test(x) 

#The data are stationary since the null hp of the test is that the data are non stationary and the p-value <0.05

#Let's now go deeply into the choice of the best ARIMA model applying directly the R function to the original data. 
#After some implicit analysis in addition to what the PACF and ACF function suggest, we thought that it will be useful to compare ARIMA models with non zero order respectively for the AR part and the integration part, because as we have seen, the MA model is not so suitable for our data. 

#We compare the AIC and BIC between:
 # ARIMA(1, 1, 0)   ARIMA(2, 1, 0)   ARIMA(3, 1, 0) 

n=length(adj_price)
AIC_arima=rep(0,times=3)
BIC_arima=rep(0,times=3)
AIC.tsa_arima=rep(0,times=3)

#ARIMA(1, 1, 0)
ARI11=arima(adj_price,order=c(1,1,0),method='ML')  
dim=2 #k

est.phi1=ARI11$coef[1]
est.sigma2=ARI11$sigma2


resMod = ARI11$residuals
loglik <- sum(dnorm(resMod, mean = 0, sd = sd(resMod), log = TRUE))
AIC_arima[1] <- -2 * loglik + 2 * dim
BIC_arima[1] <- -2 * loglik + dim * log(n)
AIC.tsa_arima[1] <- AIC(ARI11)

#ARIMA(2, 1, 0)
ARI21=arima(adj_price,order=c(2,1,0),method='ML')  
dim=3 

est.phi1=ARI21$coef[1]
est.phi2=ARI21$coef[2]
est.sigma2=ARI21$sigma2

resMod = ARI21$residuals
loglik <- sum(dnorm(resMod, mean = 0, sd = sd(resMod), log = TRUE))
AIC_arima[2] <- -2 * loglik + 2 * dim
BIC_arima[2] <- -2 * loglik + dim * log(n)
AIC.tsa_arima[2] <- AIC(ARI21)


#ARIMA(3, 1, 0)
ARI31=arima(adj_price,order=c(3,1,0),method='ML')  
dim=4 #k

est.phi1=ARI31$coef[1]
est.phi2=ARI31$coef[2]
est.phi2=ARI31$coef[3]
est.sigma2=ARI31$sigma2

resMod = ARI31$residuals
loglik <- sum(dnorm(resMod, mean = 0, sd = sd(resMod), log = TRUE))
AIC_arima[3] <- -2 * loglik + 2 * dim
BIC_arima[3] <- -2 * loglik + dim * log(n)
AIC.tsa_arima[3] <- AIC(ARI31)

AIC_arima
BIC_arima
AIC.tsa_arima

# Which is the best model?
min(AIC_arima)
min(BIC_arima)
min(AIC.tsa_arima)

#choosing BIC as main reference, we'll chose ARIMA(1, 1, 0) as the best model. 


#We also find confirmation in the BIC minor model comparison between models with assumed stationarity. 
#We see that the BIC of ARIMA(1, 1, 0) is less than the BIC of AR(2) which is the least among models without Integration ('d' order).

min(BIC_arima) # ARIMA
min(BIC)       # AR


#ARIMA(1, 1, 0)
#recall the parameters of the chosen model
est.phi1=ARI11$coef[1]
est.sigma2=ARI11$sigma2

#we will chose the ARIMA(1, 1, 0) as model. 


#ESTIMATION PARAMETER STEP

adj_price.fit=rep(0,times=n)

adj_price.fit[1]=NA

for (t in 2:n) {
  adj_price.fit[t]=est.phi1*adj_price[t-1]
}


val_x=c(0,(1:20)*25,430)
val_y=(c(-2:2)*2.5)
plot(adj_price,main="EuroSuper95",xlab="quarters",ylab="price",xlim=c(0,430),ylim=c(-0.15,0.15),
     xaxt="n",yaxt="n",type="p",lwd=3)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(adj_price.fit,col="red",lwd=2)

#Confidence intervals

# Left (lower bound)
left=rep(0,times=n)
left[1]=NA

for (t in 2:n) {
  left[t]=adj_price.fit[t]-1.960*sd(adj_price)	#P(|Z|<1.960)=0.95   Z=Standard Normal
}						#P(|Z|<2.576)=0.99

# Right (upper bound)
right=rep(0,times=n)
right[1]=NA

for (t in 2:n) {
  right[t]=adj_price.fit[t]+1.960*sd(adj_price)	#P(|Z|<1.960)=0.95   Z=Standard Normal
}

val_x=c(0,(1:20)*25,430)
val_y=c(0,(-15:15)*0.05,0.30)
#win.graph(width=12,height=6)
plot(adj_price,main="EuroSuper95",xlab="quarters",ylab="price",xlim=c(0,430),ylim=c(-0.28,0.28),
     xaxt="n",yaxt="n",type="p",lwd=3)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(adj_price.fit,col="red",lwd=2)
lines(left,col="blue",lwd=2)
lines(right,col="blue",lwd=2)

adj_price.res=ARI11$residuals

val_x=c(0,(1:20)*25,430)
val_y=c()
#win.graph(width=12,height=6)
plot(adj_price.res,main="residuals",xlab="quarters",ylab="value",xlim=c(0,430),ylim=c(-0.025,0.025),
     xaxt="n",yaxt="n",type="h",lwd=3)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(rep(0,times=n),type="l",col="red",lwd=2)

RMSE=sqrt(mean(adj_price.res^2,na.rm=TRUE))
RMSE

MAE=mean(abs(adj_price.res),na.rm=TRUE)
MAE

#Observing the Residuals

# Normality (only for MLE, not for LSE and MoM)
#win.graph(width=6,height=6)
hist(adj_price.res,main="Histogram of the residuals",xlab="",freq=F,
     xlim=c(-0.03,0.03),ylim=c(),breaks=20)
lines(density(adj_price.res),col="blue",lwd=3)
zz=seq(-0.03,0.03,length=400)
f.zz=dnorm(zz,mean(adj_price.res),sd(adj_price.res))
lines(zz,f.zz,col="red",lwd=2)


#win.graph(width=12,height=6)
par(mfrow=c(1,2))
boxplot(adj_price.res,main="box-plot")
qqnorm(adj_price.res,main="qq-plot")
qqline(adj_price.res)

shapiro.test(adj_price.res) #Shapiro-Wilk Normality - independent data!
#The residuals are Normal if the p-value is above 0.05. This is not the case. The residuals are not normal. 

library (tseries)
jarque.bera.test(adj_price.res) #The data are Normal if the p-value is above 0.05. Here we have a really low p-value

acf(adj_price.res)

#Residuals clearly do not follow a normal distribution. We already could state that since we proved before that the original data already is not normally distributed. 
#Although we can observe by the acf plot that there is no systematic pattern or relationship between the residuals at different time points. This is a desirable property in statistical modeling, as it suggests that the model is adequately capturing the underlying patterns and trends in the data.




#FORECAST STEP

#In order to check the goodness of the one-step ahead error, we cut the last 5 observations of adj_price to check how good the forecast actually is, by forecasting the values that we cut

#initialize 'nn' which is the total number of data that we have, plus the number of data that we want to predict. 
nn=length(adj_price)
n=nn-5

adj_price.for=rep(NA,times=nn)
adj_price.for[n+1]=est.phi1*adj_price[n]

for (t in (n+2):nn) {
  adj_price.for[t]=est.phi1*adj_price.for[t-1]
}

adj_price.for

#Here there are the 5 predicted values. As we can see in the formula, they are estimated using the estimation of the parameter phi computed in the estimation parameters step.


adj_price.er.for=rep(NA,times=nn)    # one-step-ahead forecast error

for (t in (n+1):nn) {
  adj_price.er.for[t]=adj_price[t]-adj_price.for[t]
}

RMSE.for=sqrt(mean(adj_price.er.for^2,na.rm=TRUE))

RMSE.for

MAE.for=mean(abs(adj_price.er.for),na.rm=TRUE)

MAE.for 

#given our previous results such as the non normality of the data and the residuals, we already expecteded MAE.for values not similar with the other MAE.
#To be satisfied with the forecast error we should expect very similar MAE and MAE.for, unfortunately this is not the case

val_x=c(0,(1:20)*25,430)
val_y=c()
#win.graph(width=20,height=10)
plot(adj_price,main="Fuel Price",xlab="date",ylab="prices",xlim=c(0,430),ylim=c(-0.14,0.14),
     xaxt="n",yaxt="n",type="p",lwd=2)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(adj_price.fit,col="red",lwd=3)
lines(adj_price.for,type="p",lwd=3)
abline(v=n,col="black",lwd=3,lty=4)

val_x=c(0,(1:20)*25,430)
val_y=c()
#win.graph(width=20,height=10)
plot(adj_price,main="Fuel Price",xlab="date",ylab="prices",xlim=c(410,430),ylim=c(-0.14,0.14),
     xaxt="n",yaxt="n",type="p",lwd=2)
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(adj_price.fit,col="red",lwd=3)
lines(adj_price.for,type="p",col="blue",lwd=3)
abline(v=n,col="black",lwd=3,lty=4)

#As we can see in the graph, the 5 predicted values are after the vertical line.
#The predicted values are the blue dots, while the actual values are the black dots.
#As we can see, and as the mae analysis showed, the forecast is not reliable


#Let's try to fit our prediction, by giving continuity to the fitted values that we found above.

adj_price.fit[n+1]=adj_price[n] # just for the plot

val_x=c(0,(1:4)*25,430)
val_y=c()
#win.graph(width=12,height=6)
plot(adj_price,main="Fuel prices",xlab="quarters",ylab="prices",xlim=c(0, 430),ylim=c(-0.14,0.14),
     type="p",xaxt="n",yaxt="n")
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(rep(mean(adj_price,na.rm=TRUE),times=n),col="black",lwd=2)
abline(v=n,col="blue",lwd=3,lty=4)
lines(adj_price.for,type="l",col="red",lwd=1)
lines(adj_price.for,type="p",lwd=3)
lines(adj_price.fit,type="l",col="red",lwd=1)


val_x=c(0,(1:4)*25,430)
val_y=c()
#win.graph(width=12,height=6)
plot(adj_price,main="Fuel prices",xlab="quarters",ylab="prices",xlim=c(410, 430),ylim=c(-0.14,0.14),
     type="p",xaxt="n",yaxt="n")
axis(1,val_x,val_x)
axis(2,val_y,val_y)
lines(rep(mean(adj_price,na.rm=TRUE),times=nn),col="black",lwd=2)
abline(v=n,col="blue",lwd=3,lty=4)
lines(adj_price.for,type="p",col="blue",lwd=3)
lines(adj_price.fit,type="l",col="red",lwd=1)

#Let's compute the prediction intervals. They are computed using Var(one-step-ahead forecast errors). 

psi=rep(0,times=nn-n)
psi.0=1
psi[1]=est.phi1
for (j in 2:5) {
  psi[j]=est.phi1*psi[j-1]
}
psi

var.er=rep(0,times=nn-n)
var.er[1]=est.sigma2

for (j in 2:5) {
  var.er[j]=est.sigma2*(psi.0+sum(psi[1:(j-1)]^2))
}
var.er

# Left (lower bound)
left.er=rep(NA,times=nn)
for (t in (n+1):nn) {
  left.er[t]=adj_price.for[t]-1.960*sqrt(var.er[t-n]) #P(|Z|<1.960)=0.95   Z=Standard Normal
}						                   #P(|Z|<2.576)=0.99
# Right (upper bound)
right.er=rep(NA,times=nn)
for (t in (n+1):nn) {
  right.er[t]=adj_price.for[t]+1.960*sqrt(var.er[t-n])
}

#x11();
val_y=c()
#win.graph(width=12,height=6)
plot(adj_price.for,xlim=c(n+1,nn),type="p",yaxt="n",ylim=c(-0.14,0.14),yaxt="n")
axis(2,val_y,val_y)
lines(adj_price.for,type="l",col="red",lwd=2)
lines(left.er,type="l",col="blue",lwd=2)
lines(right.er,type="l",col="blue",lwd=2)

#CONCLUSIONS

#we started by having all non normal distributions, and we chose the one that looked the least complicated, i.e. the one that looked like a normal distribution the most.
#After that, we tried to normalize data by appliying a log transformation and then by centralizing the distributon.
#We started to analyze the series by assuming stationarity of the data.
#As we saw by the model comparison, the best model that fitted our data was AR(2) which was the model that provided us the lowest BIC.
#In order to understand if our assumption of stationarity was right, we executed an Augmented Dickey Fuller test that showed that our data was non stationary.
#After that, we applied an ARIMA model, specifically an ARIMA(1,1,0), which is suitable for our data because it executes the integration operation that actually made our data stationary.
#In the end we forecasted the data and observed that the forecasting process for this model with these data is not reliable since MAE and MAE.for are not similar numbers.