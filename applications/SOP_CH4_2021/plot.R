# Plots temperature predictions
# S. Hafner
# 2021

library(lubridate)
meas <- read.csv('measured_temperature/temperature.csv')
meas$date.time <- ymd_hm(meas$date.time)

# Normal simulation
dat <- read.table('temp_0001.txt', header = FALSE, skip = 2)
names(dat) <- c('dos', 'doy', 'year', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry')
head(dat)
dat$year <- 2019 + dat$year 

dat$date.time <- as.POSIXct(paste(dat$doy, dat$year), format = '%j %Y')

png('slurry_temp.png', height = 8, width = 6, units = 'in', res = 600)
  plot(slurry ~ date.time, data = dat, type = 'l', ylim = c(-5, 20), 
       xlab = '', ylab = expression('Temperature'~(degree*C)), las = 1)
x <- subset(meas, site == 'Backa' & height == 0.5)
y <- subset(meas, site == 'Backa' & height == 1.5)
lines(temp ~ date.time, data = x, col = 'red')
lines(temp ~ date.time, data = y, col = 'orange')
  grid()
dev.off()

#weather <- read.table('weather_0001.txt', header = FALSE, skip = 2)
#names(weather) <- c('dos', 'doy', 'air', 'rad')

#png('slurry_temp.png', height = 8, width = 6, units = 'in', res = 600)
#  par(mfrow = c(4, 1), mar = c(4, 4.5, 0.2, 1))
#
#  plot(slurry ~ dos, data = dat, type = 'l', ylim = c(-5, 20), 
#       xlab = '', ylab = expression('Temperature'~(degree*C)), xaxt = 'n', las = 1)
#  grid()
#
#  plot(depth ~ dos, data = dat, type = 'l', col = 'brown', xlab = '', ylab = 'Slurry depth (m)')
#  grid()
#
#  plot(air ~ dos, data = dat, type = 'l', lty = 2, col = 'skyblue', ylim = c(-5, 20), 
#       xlab = '', ylab = expression('Temperature'~(degree*C)), xaxt = 'n', las = 1)
#  lines(floor ~ dos, data = dat, type = 'l', col = 'darkgreen', lty = 2)
#  lines(wall ~ dos, data = dat, type = 'l', col = 'black', lty = 2)
#  grid()
#
#  plot(rad ~ dos, data = weather, type = 'l', col = 'red', xlab = 'Day of simulation', ylab = 'Radiation (W/m2)')
#  grid()
#
#
#dev.off()
