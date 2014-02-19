## Playing around with the data

Fremont <- read.csv("C:/Users/dogfloss/Documents/GitHub/csss504project/data/weatherbike.csv")

date <- Fremont$Date
count <- Fremont$count
x <- Fremont$X

## Plot the daily bike count data, with months as labels
the_1st <- grep("*-01$", date)
plot(date, count, xaxt="n", xlab="Date", ylab="Aggregate Bike Count", 
	main="Aggregate Bike Count per day,  Jan. 1, 2013 - Dec. 31, 2013")
axis(side=1, labels=month.abb, at=the_1st)


## Create a dummy variable for each day of the week, with Monday baseline
day <- seq(1, 365, 7)
Tues <- ifelse((x %in% day), 1, 0)
Wed <- ifelse((x %in% (day+1)), 1, 0)
Thurs <- ifelse((x %in% (day+2)), 1, 0)
Fri <- ifelse((x %in% (day+3)), 1, 0)
Sat <- ifelse((x %in% (day+4)), 1, 0)
Sun <- ifelse((x %in% (day+5)), 1, 0)

## Create a dummy variable for holidays
hols <- c("01-01", "01-21", "02-18", "05-27", "07-04", "09-02", "11-11", 
	"11-28", "11-29", "12-24", "12-25")
holiday <- numeric(length(x))
for(i in 1:length(hols)){y <- grep(hols[i], date); holiday[y] <- 1}

## Bike count linear model with day-of-week and holiday covariates
datemod1 <- lm(count ~ Tues + Wed + Thurs + Fri + Sat + Sun + holiday)
summary(datemod1)

datemod2 <- lm(count ~ Fri + Sat + Sun + holiday)
summary(datemod2) 

weekend <- Sat + Sun
datemod3 <- lm(count ~ Fri + weekend + holiday)
summary(datemod3)

## Culling some useful weather variables
precip <- Fremont$precipProbability
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
sunset <- Fremont$sunsetTime

weathermod1 <- lm(count ~ min_temp + icon + precip + Fri + Sat + Sun + holiday, na.omit=TRUE)
summary(weathermod1)  ## Uses the weather "icon" in place of windSpeed and such

weathermod2 <- lm(count ~ Fri + Sat + Sun + holiday 
	+ min_temp + max_temp + precip + cloudCover + windSpeed + humidity)
summary(weathermod2)  ## Adds in several more weather variables; some are insignificant

weathermod3 <- lm(count ~ Fri + Sat + Sun + holiday 
	+ max_temp + precip + visibility + pressure)
summary(weathermod3)  ## All factors highly significant, R-squared of 0.83

weathermod4 <- weathermod3 <- lm(count ~ Tues + Wed + Thurs + Fri + Sat + Sun + holiday 
	+ max_temp + precip + visibility + pressure)
summary(weathermod4)  ## The additional days of the week are still not significant

## Define the seasons
months <- factor(date, levels=c("Winter", "Spring", "Summer", "Fall"))
months[grep("-12-", date)] <- "Winter"
months[grep("-04-", date)] <- "Spring"

