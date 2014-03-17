# Packages required for analysis
# install.packages(c("lmtest", "GGally", "ggplot2", "ggthemes",
# "lubridate", "leaps"))
library(lmtest)
library(GGally)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(leaps)

# Load the dataset as previously provided to Elena and Jim. For
# details on the construction of the dataset, please refer to
# 'dataset-generation.R'.
fremont <- read.csv("weatherbike.csv", header=TRUE)

# Remove last datapoint because of an NA value
fremont <- fremont[-365,]

# Extract individual variables from the loaded dataset dataframe. This
# is reduntant, but simplified the merger of two separate approaches
# in which one did some recoding outside of the dataframe.
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

cloudCover <- fremont$cloudCover
windSpeed <- fremont$windSpeed
humidity <- fremont$humidity
visibility <- fremont$visibility
pressure <- fremont$pressure
daylight <- fremont$sunsetTime - fremont$sunriseTime
dewPoint <- fremont$dewPoint
moonPhase <- fremont$moonPhase

# Manually create a set of dummy variables for the day of the week.
# Our initial model specifications looked for data in this format,
# which is why we did this instead of simply using a day of week
# factor.
day <- seq(1, 365, 7)
Tues <- ifelse((x %in% day), 1, 0)
Wed <- ifelse((x %in% (day+1)), 1, 0)
Thurs <- ifelse((x %in% (day+2)), 1, 0)
Fri <- ifelse((x %in% (day+3)), 1, 0)
Sat <- ifelse((x %in% (day+4)), 1, 0)
Sun <- ifelse((x %in% (day+5)), 1, 0)
Mon <- ifelse((x %in% (day+6)), 1, 0)
weekend <- Sat + Sun

# Other model code DID look for a factor rather than the individual
# dummy variables. This is equivalent to the manually coded dummy
# variables above.
Days <-
    factor(strftime(date, "%A"),
           levels=c("Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday", "Sunday"))

# Function takes a single factor, returns "Weekday" for
# Monday-Thursday. Friday, Saturday, Sunday are kept separate.
combinefactors <- function(x) {
  if (x == "Monday" |
      x == "Tuesday" |
      x == "Wednesday" |
      x == "Thursday") {      
      return("Weekday")
  } else {
    return(x)
  }
}

# Applies the combinefactors() function to the vector of day factors
Days.sub <- Days
levels(Days.sub) <- sapply(levels(Days), FUN=combinefactors)

# Manually input holidays
hols <- c("01-01", "01-21", "02-18", "05-27", "07-04", "09-02", "11-11", 
          "11-28", "11-29", "12-24", "12-25", "12-31")
holiday <- numeric(length(x))

# Create vector of same length as our dataset, set holiday to 1
for(i in 1:length(hols)){y <- grep(hols[i], date); holiday[y] <- 1}

# Create vector of UW in-session status 1 = in session, 0 = not in session
UW <-
    ifelse((x >= grep("2013-01-07", date) & x <= grep("2013-03-21", date)) |
           (x >= grep("2013-04-01", date) & x <= grep("2013-06-13", date)) |
           (x >= grep("2013-09-25", date) & x <= grep("2013-12-12", date)),
           1, 0)


# Best model as discovered through manual means
bestmod2 <- lm(count ~
               Fri + Sat + Sun + holiday + UW +
               daylight + max_temp + precip + windSpeed)

summary(bestmod2)


####################################
# Automated model variable selection
####################################

# Rejoin extracted variables into a data.frame again, keeping Days
# separate.
sel_data <-
    data.frame(count=count, Fri=Fri, Sat=Sat, Sun=Sun,
               holiday=holiday, UW=UW, daylight=daylight, max_temp=max_temp,
               precip=precip, windSpeed=windSpeed, pressure=pressure,
               visibility=visibility, humidity=humidity)

# Cast numeric dummy variables to factor type so that R handles them
# properly
sel_data.fac <-
    data.frame(count=count, Fri=as.factor(Fri),
               Sat=as.factor(Sat), Sun=as.factor(Sun),
               holiday=as.factor(holiday), UW=as.factor(UW),
               daylight=daylight, max_temp=max_temp,
               precip=precip, windSpeed=windSpeed,
               pressure=pressure, visibility=visibility,
               humidity=humidity)

# Turn things into factors and join all the days into one
# variable
sel_data.fac2 <-
    data.frame(count=count, Days=Days.sub, holiday=as.factor(holiday),
               UW=as.factor(UW), daylight=daylight, max_temp=max_temp,
               precip=precip, windSpeed=windSpeed, pressure=pressure,
               visibility=visibility, humidity=humidity)

# Same as above, but with x (1-365) included
sel_data.fac3 <-
    data.frame(count=count, Days=Days.sub, holiday=as.factor(holiday),
               UW=as.factor(UW), daylight=daylight, max_temp=max_temp,
               precip=precip, windSpeed=windSpeed, pressure=pressure,
               visibility=visibility, humidity=humidity, x=x)

# First model Selection - We did not use this one
mod_sel <- regsubsets(count ~ Fri + Sat + Sun + holiday + UW +
                      daylight + max_temp + precip + windSpeed + 
                      pressure + visibility + humidity, data=sel_data,
                      nvmax=16)
summary(mod_sel)

# Using last set of data with factors and days as single variable
mod_sel3 <-
    regsubsets(count ~ Days + holiday + UW + daylight +
               max_temp + precip + windSpeed + pressure +
               visibility + humidity,
               data=sel_data.fac2, nvmax=18)

summary(mod_sel3)

# Results of model 11, which includes everything except visibility
mod_sel_res <-
    data.frame(Size=c(1:12), RSS=summary(mod_sel3)$rss,
               R2=summary(mod_sel3)$rsq, AdjR2=summary(mod_sel3)$adjr2,
               Cp=summary(mod_sel3)$cp, BIC=summary(mod_sel3)$bic)

###################################
# Model Selection With Interactions
###################################

mod_sel.int <-
    regsubsets(count ~ Days + holiday + UW + daylight + max_temp +
               precip + windSpeed + pressure + humidity +
               max_temp*precip + max_temp*pressure +
               max_temp*daylight + max_temp*humidity +
               pressure*windSpeed + daylight*windSpeed +
               daylight*humidity + pressure*humidity +
               max_temp*windSpeed,
               data=sel_data.fac2, nvmax=25,
               force.in=c(1:11))

# Model 15
mod_sel_res.int <-
    data.frame(Size=c(12:20), RSS=summary(mod_sel.int)$rss,
               R2=summary(mod_sel.int)$rsq,
               AdjR2=summary(mod_sel.int)$adjr2,
               Cp=summary(mod_sel.int)$cp,
               BIC=summary(mod_sel.int)$bic)

checkmod.int <-
    lm(count ~ Days + holiday + UW + daylight + max_temp +
       precip + windSpeed + pressure + humidity +
       max_temp*pressure + max_temp*daylight +
       pressure*humidity + max_temp*windSpeed,
       data=sel_data.fac2)

summary(checkmod.int)

# Output results of the choses model, which includes interactions, in
# a formatted xtable
xtable(summary(checkmod.int)$coef, digits=2)

###############
# Summary plots
###############

# Pair plot
ggpairs(sel_data.fac2[,-c(10)])

# For plot formatting
the_1st <- grep("*-01$", date)

# Overall counts
plot(date, count, xaxt="n", xlab="Date", ylab="Aggregate Bike Count", 
     main="Aggregate Bike Count per day,  Jan. 1, 2013 - Dec. 31, 2013")
axis(side=1, labels=month.abb, at=the_1st)

par(mfrow=c(2,2))

# Fitted vs. Residuals plot
plot(fitted(checkmod.int), resid(checkmod.int), xlab="Fitted Values", 
     ylab="Residuals", main="Fitted Values vs Residuals")
abline(h=0, col="red")

# Response values vs fitted values
plot(sel_data.fac2$count, fitted(checkmod.int), xlab="Response Values",
     ylab="Fitted Values", main="Response vs Fitted Values")
qqnorm(resid(checkmod.int), main="QQ-Plot of Residuals")
qqline(resid(checkmod.int), col="red")

# Plot illustrating the autocorrelation problem
acf(resid(checkmod.int), main="Auto-Correlation Estimation of Residuals", 
    type="correlation")
mtext(paste("DW =", round(dwtest(checkmod.int)$statistic, 4)), side=3,
      adj=.97, line=-1)
mtext(paste("P-Value =", format(dwtest(checkmod.int)$p.value, digits=4)),
      side=3, adj=.97, line=-2)
par(mfrow=c(1,1))

######################################################
# Spliting Data for powertransforms *work in progress*
# NOTE: We did not conclude this work, though we
# include it here for reference.
######################################################

split.data2 <- list()
split.data3 <- list()
split.data <- split(sel_data.fac2, sel_data.fac2$Days)

for (i in 1:4) {
  tmp <- split(split.data[[i]], split.data[[i]]$holiday)
  split.data2[[i*2-1]] <- tmp[[1]]
  split.data2[[i*2]] <- tmp[[2]]
}

for (i in 1:8) {
  tmp <- split(split.data2[[i]], split.data2[[i]]$UW)
  split.data3[[i*2-1]] <- tmp[[1]]
  split.data3[[i*2]] <- tmp[[2]]
}

split.pt <- list()
for (i in 1:16) {
  tmp <- split.data3[[i]]
  if (dim(tmp)[1] == 0) {
    split.pt[[i]] <- rep(1,7)
  } else {
    tmp.pt <- powerTransform(cbind(daylight, max_temp, precip, windSpeed, 
                                   pressure, visibility, humidity) ~ 1, 
                             data=tmp, family="yjPower")
    split.pt[[i]] <- coef(tmp.pt, round=T)
  }
}

# Count transform
inverse.response.plot(checkmod.int)

#######################
# Checking for outliers
#######################
range(stdres(checkmod.int))
range(studres(checkmod.int))
qqnorm(stdres(checkmod.int))
halfnorm(stdres(checkmod.int))

outliers <-
    data.frame(date=gsub("\\d+-(\\d+)-(\\d+)", "\\1/\\2",
                   as.character(date), perl=T),
               count=count, fitted=fitted(checkmod.int),
               studres=studres(checkmod.int),
               stdres=stdres(checkmod.int))[abs(stdres(checkmod.int))>2,]

# Format as xtable for inclusion in presentation
xtable(outliers, digits=2)

# Run final model with outliers removed
checkmod.int.no_out <-
    lm(count ~ Days + holiday + UW + daylight + max_temp +
       precip + windSpeed + pressure + humidity +
       max_temp*pressure + max_temp*daylight +
       pressure*humidity + max_temp*windSpeed,
       data=sel_data.fac2,
       subset=-as.numeric(row.names(outliers)))

# Comparison, with and without
comparing_outliers <-
    data.frame(ckmod.coef=summary(checkmod.int)$coef[,1],
               no_out.coef=summary(checkmod.int.no_out)$coef[,1],
               ckmod.pval=summary(checkmod.int)$coef[,4],
               no_out.pval=summary(checkmod.int.no_out)$coef[,4])


# Playing around with halfnorm and the outliers
halfnorm(stdres(checkmod.int), nlab=5,
         labs=gsub("\\d+-(\\d+)-(\\d+)", "\\1/\\2", date, perl=T))


# Check if x (1-365) to determine whether bicycle counts were
# generally increasing or decreasing over the span of the year. They
# are neither.
mod_sel4 <- regsubsets(count ~ ., data=sel_data.fac3, nvmax=20)
mod_sel4.sum <- summary(mod_sel4)
data.frame(rsq=mod_sel4.sum$rsq, adjr2=mod_sel4.sum$adjr2,
           cp=mod_sel4.sum$cp, bic=mod_sel4.sum$bic)

# Use ANOVA to compare model with and without x (1-365) variable, 
anova(lm(count ~ Days + holiday + UW + daylight + max_temp + precip + 
           windSpeed + pressure + humidity, data=sel_data.fac3),
      lm(count ~ Days + holiday + UW + daylight + max_temp + precip + 
           windSpeed + pressure + humidity + x, data=sel_data.fac3))

#################
# Influence plots
#################
checkmod.int.inf <- influence(checkmod.int)
par(mfrow=c(3,2))
for (i in 7:12) {
  qqnorml(checkmod.int.inf$coefficients[,i],
          main=paste("Variable Influence Plot - Leaving Out", 
                     colnames(checkmod.int.inf$coefficients)[i]))
}
par(mfrow=c(1,1))

##############
# Cook's Plots
##############
halfnorm(cooks.distance(checkmod.int), nlab=7,
         labs=gsub("\\d+-(\\d+)-(\\d+)", "\\1/\\2", date, perl=T),
         main="Cook's Distance Plot of Residuals With Labels as Dates")

cutoff <- 4/((nrow(sel_data.fac2)-length(checkmod.int$coefficients)-2)) 

plot(checkmod.int, which=4, cook.levels=cutoff, sub="")
