## Playing around with the data

Fremont <- read.csv("C:/Users/Robin Gold/Documents/GitHub/csss504project/data/weatherbike.csv")
library(lmtest)

##### DEFINING VARIABLES #######################

date <- Fremont$Date
count <- Fremont$count
x <- Fremont$X
precip <- Fremont$precipIntensityMax
min_temp <- Fremont$temperatureMin
min_temp2 <- Fremont$apparentTemperatureMin
max_temp <- Fremont$apparentTemperatureMax

icon <- Fremont$icon;  
icon[which(icon=="partly-cloudy-night")] <- "partly-cloudy-day"

cloudCover <- Fremont$cloudCover
windSpeed <- Fremont$windSpeed
humidity <- Fremont$humidity
visibility <- Fremont$visibility
pressure <- Fremont$pressure
daylight <- Fremont$sunsetTime - Fremont$sunriseTime


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


## Dummy variable for UW academic year
UW <- ifelse(x < grep("2013-06-14", date) | x >= grep("2013-09-25", date), 1, 0)


####### BEST WORKING MODEL #############################

bestmod <- lm(count ~ Fri + Sat + Sun + holiday + UW
	 + daylight + max_temp + precip + cloudCover + x)
summary(bestmod) 
acf(bestmod$resid)
dwtest(bestmod)
plot(bestmod$resid);  abline(h=0, col="red")


########## SUMMARY PLOTS #####################

## Daily bike count data, with months as labels
the_1st <- grep("*-01$", date)
plot(date, count, xaxt="n", xlab="Date", ylab="Aggregate Bike Count", 
	main="Aggregate Bike Count per day,  Jan. 1, 2013 - Dec. 31, 2013")
axis(side=1, labels=month.abb, at=the_1st)

## Plot pairs for variables in bestmod
pairs(cbind(count, daylight, max_temp, precip, cloudCover, x))


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

mydat <- data.frame(count, date, holiday, season, min_temp, max_temp, precip, visibility, pressure, 
	Tues, Wed, Thurs, Fri, Sat, Sun, weekend, cloudCover, windSpeed, humidity)
head(mydat)

dTues <- subset(mydat, Tues == 1)
m1Tues <- lm(count ~ holiday + daylight + max_temp + precip + visibility + pressure, 
	data = dTues)
summary(m1Tues)

Winter <- ifelse(dTues$season == "Winter", 1, 0)  
m2Tues <- lm(count ~ holiday + Winter + max_temp + precip, data = dTues)
summary(m2Tues)




















