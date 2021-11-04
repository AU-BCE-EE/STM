# Parameter estimation attempt

rm(list = ls())

library(ggplot2)
library(lubridate)

# Measurements
measd <- read.csv('../meas_temp/temperature.csv')
measd$measde.time <- ymd_hm(measd$date.time)
measd$date <- as.POSIXct(substr(measd$measde.time, 1, 10))
measd$height <- paste(measd$height, ' m')

# Fix site
measd$site <- substr(measd$site, 1, 4)
measd$site[measd$site == 'Rånä'] <- 'Raan'

measd <- subset(measd, site == 'Back')

# Average temperature
meas <- aggregate(temp ~ site + date, data = measd, FUN = mean)

# Define residuals function
resCalc <- function(p, meas.dat){

  p['soilDamp'] <- abs(p['soilDamp'])
  meas <- meas.dat

  # Write parameter values to file
  system('cp ../pars/pars_template.txt ../pars/pars.txt')
  for (i in 1:length(p)) {
    system(paste0('sed -i s/^', names(p)[i], '/', p[i], '/g ../pars/pars.txt'))
  }

  # Run model
  cat('. ')
  system('./stm Back ../pars/pars.txt ../pars/user_pars.txt ../weather/weather.txt')

  # Move output
  system('mv *_temp.txt* ../stm_output')
  system('mv *_weather* ../stm_output')
  system('mv *_rates* ../stm_output')

  # Read in calculated temperatures
  mod <- read.table('../stm_output/Back_temp.txt', skip = 2, header = TRUE)
  names(mod) <- c('dos', 'doy', 'year', 'mass.slurry', 'mass.frozen', 
                'depth.slurry', 'temp.air', 'temp.wall', 'temp.floor', 
                'temp.slurry')


  mod$year <- 2019 + mod$year
  mod$date <- as.POSIXct(paste(mod$year, mod$doy), format = '%Y %j')

  # Merge measured and calculated
  dat <- merge(meas[, c('date', 'temp')], mod[, c('date', 'temp.slurry')], by = 'date')
  dat <<- dat

  res <- dat$temp.slurry - dat$temp
  obj <- sum(abs(res))

  return(obj)
}

# Initial par guesses
p <- c(uAir = 20, glConc = 1, glSlur = 0.3, absorp = 0.05, soilDamp = 10)

# Optimize
m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')

m

plot(temp.slurry ~ date, data = dat, col = 'black', type = 'l')
lines(temp ~ date, data = dat, col = 'red')

