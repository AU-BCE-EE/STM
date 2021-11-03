# Prepare Uppsala weather data for input in stm model

library(lubridate)
source('../functions/rounddf.R')

dat <- read.csv('../measurements/Uppsala_weather.csv', skip = 2)

# Sort out time
dat$hour <- sprintf('%04d', dat$hour)
dat$date.time <- ymd_hm(paste(dat$date, dat$hour))
dat$doy <- as.integer(as.character(dat$date.time, format = '%j'))
dat$year <- as.integer(as.character(dat$date.time, format = '%Y'))
dat$date <- date(dat$date.time)

# year and date not used now

# Summarize by doy
datd <- aggregate(dat[, c('temp.1.5m', 'rad')], dat[, c('doy'), drop = FALSE], mean)
datd <- rounddf(datd, 3, signif)

write.table(datd, '../output/weather.txt', row.names = FALSE)
