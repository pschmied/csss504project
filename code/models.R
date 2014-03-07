## Playing around with the data

fremont <- read.csv("../data/weatherbike.csv", header=TRUE)
library(lmtest)
library(GGally)
library(ggplot2)
library(ggthemes)
library(lubridate)

##### DEFINING VARIABLES #######################

date <- fremont$Date
count <- fremont$count
x <- fremont$X

precip <- fremont$precipIntensityMax
precip2 <- fremont$precipIntensity
precipType <- fremont$precipType
precipTime <- fremont$precipIntensityMaxTime

min_temp <- fremont$temperatureMin
min_temp2 <- fremont$apparentTemperatureMin
max_temp <- fremont$temperatureMax
max_temp2 <- fremont$apparentTemperatureMax

icon <- fremont$icon;  
icon[which(icon=="partly-cloudy-night")] <- "partly-cloudy-day"

cloudCover <- fremont$cloudCover
windSpeed <- fremont$windSpeed
humidity <- fremont$humidity
visibility <- fremont$visibility
pressure <- fremont$pressure
daylight <- fremont$sunsetTime - fremont$sunriseTime
dewPoint <- fremont$dewPoint
moonPhase <- fremont$moonPhase

## Create a dummy variable for each day of the week, with Monday baseline
day <- seq(1, 365, 7)
Tues <- ifelse((x %in% day), 1, 0)
Wed <- ifelse((x %in% (day+1)), 1, 0)
Thurs <- ifelse((x %in% (day+2)), 1, 0)
Fri <- ifelse((x %in% (day+3)), 1, 0)
Sat <- ifelse((x %in% (day+4)), 1, 0)
Sun <- ifelse((x %in% (day+5)), 1, 0)
weekend <- Sat + Sun


## Create a dummy variable for holidays
hols <- c("01-01", "01-21", "02-18", "05-27", "07-04", "09-02", "11-11", 
	"11-28", "11-29", "12-24", "12-25", "12-31")
holiday <- numeric(length(x))
for(i in 1:length(hols)){y <- grep(hols[i], date); holiday[y] <- 1}


## Create a "season" factor, based on Farmer's Almanac dates of Equinox
season <- factor(date, levels=c("Winter", "Spring", "Summer", "Fall"))
season[date] <- "Winter"
season[x >= grep("2013-03-20", date) & x < grep("2013-06-21", date)] <- "Spring"
season[x >= grep("2013-06-21", date) & x < grep("2013-09-22", date)] <- "Summer"
season[x >= grep("2013-09-22", date) & x < grep("2013-12-21", date)] <- "Fall"


## Variable for the UW academic year
UW <- ifelse((x >= grep("2013-01-07", date) & x <= grep("2013-03-21", date)) | 
	(x >= grep("2013-04-01", date) & x <= grep("2013-06-13", date)) |
	(x >= grep("2013-09-25", date) & x <= grep("2013-12-12", date)), 1, 0)

## Create a variable for yesterday's precip
yesterday <- numeric(length(precip))
for(i in 2:length(yesterday)){yesterday[i] <- precip[i-1]}
yesterday[1] <- yesterday[2]

last3days <- numeric(length(precip))
for(i in 4:length(last3days)){last3days[i] <- mean(precip[(i-3):(i-1)])}
last3days[3] <- mean(precip[1:2])
last3days[1:2] <- precip[1]

## Take care of the last data point being NA for precip variable
precip[365] <- precip[364]


####### BEST WORKING MODEL #############################

bestmod <- lm(count ~ Fri + Sat + Sun + holiday + UW
	 + daylight + max_temp + precip + cloudCover + x)
summary(bestmod) 
AIC(bestmod)
BIC(bestmod)
acf(bestmod$resid)
dwtest(bestmod)
plot(bestmod$resid);  abline(h=0, col="red")
## bestmod adjusted R-sq = 0.8739;  AIC = 3944;  BIC 3988;  DW test p-val 1e-07

bestmod2 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip + windSpeed)
## bestmod2 is mod4 below;  selected based on highest AIC/BIC
## mod4 adjusted R-sq = 0.8492;  AIC = 5010;  BIC 5053;  DW test p-val 8e-09


####### MODELS WITH YESTERDAY'S WEATHER, INTERACTION TERMS, ETC. #####

mod2 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip)
summary(mod2); AIC(mod2); BIC(mod2); acf(mod2$resid); dwtest(mod2)
## mod2 adjusted R-sq = 0.8441;  AIC = 5021;  BIC 5060;  DW test p-val 5e-09
## This is just bestmod without the faulty cloudCover variable.
## AIC and BIC went way up by getting rid of it, but adjusted R-sq went down by .03

mod3 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip + windSpeed + yesterday)
summary(mod3); AIC(mod3); BIC(mod3); acf(mod3$resid); dwtest(mod3)
## mod3 adjusted R-sq = 0.8528;  AIC = 5002;  BIC 5049;  DW test p-val 7e-10
## Inclusion of yesterday's weather variable makes auto-correlation worse :(
## last3days precip variable was insignificant when tested

mod4 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip + windSpeed)
summary(mod4); AIC(mod4); BIC(mod4); acf(mod4$resid); dwtest(mod4)
## mod4 adjusted R-sq = 0.8492;  AIC = 5010;  BIC 5053;  DW test p-val 8e-09
## Without cloudCover in the model, windSpeed is highly significant.

mod5 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip + windSpeed + humidity)
summary(mod5); AIC(mod5); BIC(mod5); acf(mod5$resid); dwtest(mod5)
## mod5 adjusted R-sq = 0.8584;  AIC = 4988;  BIC 5035;  DW test p-val 3e-08
## humidity is highly significant also, but AIC/BIC penalize for added variable
## dewPoint can subsitute for humidity, but humidity is slightly better.

mod6 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + max_temp + precip + windSpeed + dewPoint 
	+ windSpeed*max_temp + windSpeed*precip + daylight*precip + humidity*precip)
summary(mod6); AIC(mod6); BIC(mod6); acf(mod6$resid); dwtest(mod6)
## I played around with these and other interaction terms, didn't find any significant

mod7 <- lm(count ~ Fri + Sat + Sun + holiday + UW
	+ daylight + I(max_temp^3) + precip + windSpeed + humidity)
summary(mod7); AIC(mod7); BIC(mod7); acf(mod7$resid); dwtest(mod7)
## mod7 adjusted R-sq = 0.8517;  AIC = 5005;  BIC 5052;  DW test p-val 8e-10
## Played around with various higher-order terms; no dramatic improvements found.

plot(mod7$resid);  abline(h=0, col="red")


########## SUMMARY PLOTS #####################

## Daily bike count data, with months as labels
the_1st <- grep("*-01$", date)
plot(date, count, xaxt="n", xlab="Date", ylab="Aggregate Bike Count", 
	main="Aggregate Bike Count per day,  Jan. 1, 2013 - Dec. 31, 2013")
axis(side=1, labels=month.abb, at=the_1st)

## Plot pairs for variables in bestmod
pairs(cbind(count, daylight, max_temp, precip, cloudCover, x), 
	pch=20, col="darkblue")

## Individual variable plots, to look for problems
plot(cloudCover) ## has lots of NAs in latter half of year
plot(daylight) ## perfect sinusoid, as expected
plot(windSpeed) ## very scattered, looks legit
plot(humidity) ## roughly inverse to daylight, but with significant scatter
plot(visibility) ## interesting shape, no problems
plot(pressure) ## similar shape to humidity
plot(dewPoint) ## possibly-suspicious drop-off in values at end of year
plot(moonPhase) ## linear trend by month
plot(precip) ## has a lot of zeroes, but looks legit
plot(precip2) ## similar to precip, but with even more zeroes
plot(precipTime) ## perfectly linear;  don't know what this variable means, but don't use.
plot(min_temp) ## roughly sinusoidal, large correlation w/ daylight
plot(min_temp2) ## very similar to min_temp
plot(max_temp) ## very similar to min_temp
plot(max_temp2) ## also very similar to min_temp



####### SOME MODELS PREVIOUSLY TESTED #########

## Bike count linear model with day-of-week and holiday covariates
datemod1 <- lm(count ~ Tues + Wed + Thurs + Fri + Sat + Sun + holiday)
summary(datemod1)

datemod2 <- lm(count ~ Fri + Sat + Sun + holiday)
summary(datemod2) 

datemod3 <- lm(count ~ Fri + weekend + holiday)
summary(datemod3)

weathermod1 <- lm(count ~ min_temp + icon + precip + Fri + Sat + Sun + holiday, na.omit=TRUE)
summary(weathermod1)  ## Uses the weather "icon" in place of windSpeed and such

weathermod2 <- lm(count ~ Fri + Sat + Sun + holiday 
	+ min_temp + max_temp + precip + cloudCover + windSpeed + humidity)
summary(weathermod2)  ## Adds in several more weather variables; some are insignificant

weathermod3 <- lm(count ~ Fri + Sat + Sun + holiday 
	+ max_temp + precip + visibility + pressure)
summary(weathermod3)  ## All factors highly significant, R-squared of 0.83

weathermod4 <- lm(count ~ Tues + Wed + Thurs + Fri + Sat + Sun + holiday 
	+ max_temp + precip + visibility + pressure)
summary(weathermod4)  ## The additional days of the week are still not significant

## Model which adds "season" covariate
mod5 <- lm(count ~ Fri + Sat + Sun + holiday + season
	+ max_temp + precip + visibility + pressure)
summary(mod5)
plot(mod5$fit, mod5$resid);  abline(c(0,0), col="red")
date[which(mod5$resid == min(mod5$resid))]
date[which(mod5$resid == max(mod5$resid))]


########################################################

## Create a separate model for each day of the week

mydat <- data.frame(count, date, holiday, season, min_temp, max_temp, precip, 
	visibility, pressure, Tues, Wed, Thurs, Fri, Sat, Sun, weekend, 
	cloudCover, windSpeed, humidity, UW, daylight, x)
head(mydat)

dMon <- subset(mydat, Tues + Wed + Thurs + Fri + Sat + Sun == 0)
mMon <- lm(count ~ holiday + UW + daylight + max_temp + precip + cloudCover + x,
	data = dMon)
summary(mMon)
acf(mMon$resid)
dwtest(mMon)  ## p-val 0.76

dTues <- subset(mydat, Tues == 1)
mTues <- lm(count ~ holiday + UW + daylight + max_temp + precip + cloudCover + x,
	data = dTues)
summary(mTues)
acf(mTues$resid)
dwtest(mTues)  ## p-val 0.77

dWed <- subset(mydat, Wed == 1)
mWed <- lm(count ~ holiday + UW + daylight + max_temp + precip + cloudCover + x,
	data = dWed)
summary(mWed)
acf(mWed$resid)
dwtest(mWed)  ## p-val 0.02

dThurs <- subset(mydat, Thurs == 1)
mThurs <- lm(count ~ holiday + UW + daylight + max_temp + precip + cloudCover + x,
	data = dThurs)
summary(mThurs)
acf(mThurs$resid)
dwtest(mThurs)  ## p-val 0.12

dFri <- subset(mydat, Fri == 1)
mFri <- lm(count ~ UW + daylight + max_temp + precip + cloudCover + x,
	data = dFri)
summary(mFri)
acf(mFri$resid)
dwtest(mFri)  ## p-val 0.16

dSat <- subset(mydat, Sat == 1)
mSat <- lm(count ~ UW + daylight + max_temp + precip + cloudCover + x,
	data = dSat)
summary(mSat)
acf(mSat$resid)
dwtest(mSat)  ## p-val 0.16

dSun <- subset(mydat, Sun == 1)
mSun <- lm(count ~ UW + daylight + max_temp + precip + cloudCover + x,
	data = dSun)
summary(mSun)
acf(mSun$resid)
dwtest(mSun)  ## p-val 0.90

















