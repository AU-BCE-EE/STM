# Prepare Uppsala weather data for input in stm model
# Note that model currently accepts one obs per DOY
# Measurements have some 2020 and 2021 values for May, so this is a bit of a problem

library(lubridate)
source('../functions/rounddf.R')

dat <- read.csv('../measurements/Uppsala_weather.csv', skip = 2)

# Sort out time
dat$hour <- sprintf('%04d', dat$hour)
dat$date.time <- ymd_hm(paste(dat$date, dat$hour))
dat$doy <- as.integer(as.character(dat$date.time, format = '%j'))
dat$year <- as.integer(as.character(dat$date.time, format = '%Y'))
dat$date <- date(dat$date.time)

# Subset
dat <- subset(dat$date >= ymd('2020 05 01') & dat$date <= ymd('2021 05 31'))

# Summarize by doy
mns <- aggregate(dat[, c('temp.1.5m', 'rad')], dat[, c('doy'), drop = FALSE], mean)
yrs <- aggregate(dat[, c('year')], dat[, c('doy'), drop = FALSE], function(x) x[1])
ns <- aggregate(dat[, c('temp.1.5m')], dat[, c('doy'), drop = FALSE], function(x) length(x))
datd <- merge(yrs, mns, by = 'doy')
datd <- rounddf(datd, 3, signif)

# Check n
if (any(ns$n > 24)) stop('Count error')

write.table(datd, '../output/Uppsala_doy_weather.txt', row.names = FALSE)
