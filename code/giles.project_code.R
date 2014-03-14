# 504 Project Script - Giles

# Read in data file 'weatherbike.csv'
weatherbike <- read.csv("weatherbike.csv")

# subset
# Date and time directly correlated
# Date & sunrisetime, sunsettime directly correlated
# Date & temperatureMinTime directly correlated
# Date & temperatureMaxTime dc
# Date & apparentTemperatureMinTime
# Date & apparentTemperatureMaxTime
# Date & precipIntensityMaxTime
# summary not useful maybe?
weatherbike.sub <- subset(weatherbike, 
                          select=-c(X, time, sunriseTime, sunsetTime, temperatureMinTime,
                                    temperatureMaxTime, apparentTemperatureMinTime,
                                    apparentTemperatureMaxTime, precipIntensityMaxTime))

pairs(weatherbike.sub)

weatherbike.clean <- subset(weatherbike,
                            select=c(Date, count, precipIntensity, precipIntensityMax, 
                                     temperatureMin,
                                     temperatureMax, dewPoint, humidity, windSpeed, 
                                     visibility, pressure))
acf(weatherbike.clean)

# Correlation
acf(weatherbike.sub$precipIntensity, type="correlation")
acf(weatherbike.sub$precipProbability, type="correlation")

library(lmtest)
lm.1 <- lm(count~precipIntensity + temperatureMin + windSpeed + pressure, data=weatherbike.sub)
dwtest(lm.1)
acf(lm.1$residuals)

dwt(lm.1)
dwt(lm.1, max.lag=10)

lm.2 <- lm(count~precipIntensity + temperatureMin + windSpeed + pressure + dewPoint +
             humidity + visibility, data=weatherbike.clean)
lm.2
boxcox(lm.2)

# Days of week
weatherbike.clean$Days <- factor(strftime(weatherbike.clean$Date, "%A"), 
                                 levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                          "Friday", "Saturday", "Sunday"))
lm.3 <- lm(count~precipIntensity + temperatureMin + Days, data=weatherbike.clean)
dwt(lm.3)
acf(lm.3$residuals)

library(car)

combinefactors <- function(x) {
  if (x == "Monday" | x == "Tuesday" | x == "Wednesday" | x == "Thursday") {
    return("Weekday")
  } else {
    return(x)
  }
}

weatherbike.clean$Days.Mod <- weatherbike.clean$Days
levels(weatherbike.clean$Days.Mod) <- sapply(levels(weatherbike.clean$Days), FUN=combinefactors)

hols <- c("01-01", "01-21", "02-18", "05-27", "07-04", "09-02", "11-11", 
          "11-28", "11-29", "12-24", "12-25")
holiday <- numeric(365)
for(i in 1:length(hols)) {
  y <- grep(hols[i], weatherbike.clean$Date) 
  holiday[y] <- 1
}

weatherbike.clean$Holiday <- holiday
weatherbike.clean$precipProbability <- weatherbike$precipProbability
lm.4 <- lm(count ~ Days.Mod + Holiday + precipProbability + temperatureMax + 
     pressure + visibility, data=weatherbike.clean)
summary(lm.4)
plot(lm.4$fitted.values, lm.4$residuals)
dwtest(lm.4)
acf(lm.4$residuals)

lm.5 <- lm(count ~ Days + Holiday + precipProbability + temperatureMax + 
             pressure + visibility, data=weatherbike.clean)
summary(lm.5)
plot(lm.5$fitted.values, lm.5$residuals)
dwtest(lm.5)
acf(lm.5$residuals)
which(lm.5$residuals == min(lm.5$residuals))
which(lm.5$residuals == max(lm.5$residuals))
weatherbike.clean[which(lm.5$residuals == min(lm.5$residuals)), ]
weatherbike.clean[which(lm.5$residuals == max(lm.5$residuals)), ]

lm.6 <- lm(count ~ Days + Holiday + precipIntensity + temperatureMax + 
             pressure + visibility, data=weatherbike.clean)
summary(lm.6)
plot(lm.6$fitted.values, lm.6$residuals)
dwtest(lm.6)
acf(lm.6$residuals)
which(lm.6$residuals == min(lm.5$residuals))
which(lm.6$residuals == max(lm.5$residuals))
weatherbike.clean[which(lm.6$residuals == min(lm.6$residuals)), ]
weatherbike.clean[which(lm.6$residuals == max(lm.6$residuals)), ]

models <- list()
for (i in 1:7) {
  day <- levels(weatherbike.clean$Days)[i]
  print(day)
  weatherbike.sub <- weatherbike.clean[weatherbike.clean$Days == day, ]
  models[[day]] <- lm(count ~ Holiday + precipIntensity + temperatureMax +
                    pressure + visibility, data=weatherbike.sub)
}

lm.7 <- lm(count ~ Days + Holiday + precipIntensity + temperatureMax + 
             pressure + visibility + temperatureMax*pressure + temperatureMax*visibility +
             pressure*visibility, 
           data=weatherbike.clean)
summary(lm.7)
acf(lm.7$residuals)

lm.8 <- lm(count ~ Days + Holiday + precipIntensityMax + temperatureMax + 
             pressure + visibility, data=weatherbike.clean)
summary(lm.8)

par(mfrow=c(2,1))
plot(lm.8$fitted.values, lm.8$residuals, main="Fitted Values vs Residuals",
     xlab="Fitted Values", ylab="residuals")
abline(h=0, col="red")
acf(lm.8$residuals, main="Auto-Correlation of Residuals")
par(mfrow=c(2,2))

par(mfcol=c(1,2))
plot(lm.8$fitted.values, lm.8$residuals, main="Fitted Values vs Residuals",
     xlab="Fitted Values", ylab="residuals")
abline(h=0, col="red")
acf(lm.8$residuals, main="Auto-Correlation of Residuals")
par(mfcol=c(2,2))

####### BEST WORKING MODEL #############################
fremont <- read.csv("weatherbike.csv")

## Dummy variable for UW academic year
UW <- ifelse(x < grep("2013-06-14", date) | x >= grep("2013-09-25", date), 1, 0)
hols <- c("01-01", "01-21", "02-18", "05-27", "07-04", "09-02", "11-11", 
          "11-28", "11-29", "12-24", "12-25", "12-31")

cloudCover <- fremont$cloudCover
windSpeed <- fremont$windSpeed
humidity <- fremont$humidity
visibility <- fremont$visibility
pressure <- fremont$pressure
daylight <- fremont$sunsetTime - fremont$sunriseTime

date <- fremont$Date
count <- fremont$count
x <- fremont$X
precip <- fremont$precipIntensityMax
min_temp <- fremont$temperatureMin
min_temp2 <- fremont$apparentTemperatureMin
max_temp <- fremont$apparentTemperatureMax

holiday <- numeric(length(x))
for(i in 1:length(hols)){y <- grep(hols[i], date); holiday[y] <- 1}
day <- seq(1, 365, 7)
Tues <- ifelse((x %in% day), 1, 0)
Wed <- ifelse((x %in% (day+1)), 1, 0)
Thurs <- ifelse((x %in% (day+2)), 1, 0)
Fri <- ifelse((x %in% (day+3)), 1, 0)
Sat <- ifelse((x %in% (day+4)), 1, 0)
Sun <- ifelse((x %in% (day+5)), 1, 0)
weekend <- Sat + Sun

tmp.fac <- data.frame(count=count, Days=weatherbike.clean$Days.Mod, holiday=holiday, UW=UW,
                      daylight=daylight, max_temp=max_temp, precip=precip, x=x)
ggpairs(tmp.fac[1:364, ])

bestmod <- lm(count ~ Fri + Sat + Sun + holiday + UW
              + daylight + max_temp + precip + cloudCover + x)
summary(bestmod) 

bestmod2 <- lm(count ~ Fri + Sat + Sun + holiday + UW
              + daylight + max_temp + precip + x)
summary(bestmod2) 
acf(bestmod2$resid)
plot(bestmod2$fitted.values, bestmod2$resid)

par(mfrow=c(2,1))
acf(bestmod$resid, main="Auto-Correlation of Residuals")
dwtest(bestmod)
plot(bestmod$fitted.values, bestmod$resid, main="Fitted Values vs Residuals",
     xlab="Fitted Values", ylab="Residuals");  abline(h=0, col="red")
par(mfrow=c(1,1))

Fri.fac <- as.factor(Fri)
Sat.fac <- as.factor(Sat)
Sun.fac <- as.factor(Sun)
holiday.fac <- as.factor(holiday)
UW.fac <- as.factor(UW)

bestmod3 <- lm(count ~ weatherbike.clean$Days.Mod + holiday.fac + UW.fac
              + daylight + max_temp + precip + x)
