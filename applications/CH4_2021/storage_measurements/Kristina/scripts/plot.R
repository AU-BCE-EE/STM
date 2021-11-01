# Plots temperature data from Kristina

library(ggplot2)
library(lubridate)

# Measurements
#dat <- read.csv('../data/temperature.csv')
dat <- read.csv('../storage_measurements/Kristina/data/temperature.csv')
dat$date.time <- ymd_hm(dat$date.time)
dat$date <- as.POSIXct(substr(dat$date.time, 1, 10))
dat$height <- paste(dat$height, ' m')

class(dat$date)

# Predictions
#ss <- read.table('../../../sims/temp_0001.txt', skip = 2)
ss <- read.table('temp_0001.txt', skip = 2)
names(ss) <- c('dos', 'doy', 'year', 'mass.slurry', 'depth.slurry', 
               'temp.air', 'temp.wall', 'temp.floor', 'temp.slurry')
ss$year <- 2019 + ss$year
ss$date <- as.POSIXct(paste(ss$year, ss$doy), format = '%Y %j')

# Add in model predictions by date
dat <- merge(dat, ss, by = 'date', all = FALSE)

ggplot(dat, aes(date.time, temp, colour = factor(height))) +
  geom_line() +
  geom_line(aes(date, temp.slurry), colour = 'black') +
  geom_line(aes(date, temp.air), colour = 'skyblue', lty = 2) +
  facet_wrap(~ site) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('../plots/stor_temp_4.png', height = 6, width = 8)
