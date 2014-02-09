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
datemod <- lm(count ~ Tues + Wed + Thurs + Fri + Sat + Sun + holiday)
summary(datemod)



