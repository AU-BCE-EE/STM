# Simplify Uppsala weather data a bit

library(lubridate)
source('wsine.R')

dat <- read.csv('Uppsala_weather.csv', skip = 2)

dat$hour <- sprintf('%04d', dat$hour)
dat$date.time <- ymd_hm(paste(dat$date, dat$hour))
dat$doy <- as.integer(as.character(dat$date.time, format = '%j'))
dat$year <- as.integer(as.character(dat$date.time, format = '%Y'))
dat$date <- date(dat$date.time)

plot(temp.1.5m ~ date.time, data = dat, type = 'l')
plot(rad ~ date.time, data = dat, type = 'l')

# Summarize by doy
temp <- aggregate(dat$temp.1.5m, list(year = dat$year, doy = dat$doy, date = dat$date), mean)
names(temp)[4] <- 'temp'
rad <- aggregate(dat$rad, list(year = dat$year, doy = dat$doy, date = dat$date), mean)
names(rad)[4] <- 'rad'
datd <- merge(temp, rad)
datd <- datd[order(datd$doy), ]

plot(rad ~ doy, data = datd, type = 'l')

# Fit
tmod <- nls(temp ~ mini + amp * 0.5 *  
            (sin((doy - maxdoy)/365 * 2 *pi + 0.5 * pi) + 1),
            data = datd, start = list(mini = min(datd$temp), 
                                      amp = diff(range(datd$temp)),
                                      maxdoy = 200)
           )
tmod

datd$temp.pred <- predict(tmod)

plot(temp ~ doy, data = datd, type = 'l')
lines(temp.pred ~ doy, data = datd, col = 'red')

rmod <- nls(rad ~ mini + amp * 0.5 *  
            (sin((doy - maxdoy)/365 * 2 *pi + 0.5 * pi) + 1),
            data = datd, start = list(mini = min(datd$rad), 
                                      amp = diff(range(datd$rad)),
                                      maxdoy = 200)
           )
rmod
datd$rad.pred <- predict(rmod)
# NTS: need to set rad min to 0 in model!

plot(rad ~ doy, data = datd, type = 'l')
lines(rad.pred ~ doy, data = datd, col = 'red')
