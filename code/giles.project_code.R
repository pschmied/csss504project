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
                            select=c(Date, count, precipIntensity, temperatureMin,
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

