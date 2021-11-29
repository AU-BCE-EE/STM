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

# Use only 3 for calibration
measd <- subset(measd, site %in% c('Back', 'Fitt', 'Raan'))

# Average temperature
meas <- aggregate(temp ~ site + date, data = measd, FUN = mean)

# Define residuals function
resCalc <- function(p, meas.dat, fixed){

  # Cheap fix for negative parameter values
  p <- abs(p)
  if (!missing(fixed)) {
    p <- c(p, fixed)
  }

  # Change name 
  meas <- meas.dat

  # Write parameter values to file
  system('cp ../pars/pars_template.txt ../pars/pars.txt')
  for (i in 1:length(p)) {
    system(paste0('sed -i s/', names(p)[i], '/', p[i], '/g ../pars/pars.txt'))
  }

  # Run model
  cat('. ')
  system('./stm Back ../pars/pars.txt ../pars/Back_u_pars.txt ../weather/weather.txt ../level/Back_level.txt &
          ./stm Fitt ../pars/pars.txt ../pars/Fitt_u_pars.txt ../weather/weather.txt ../level/Fitt_level.txt &
          ./stm Raan ../pars/pars.txt ../pars/Raan_u_pars.txt ../weather/weather.txt ../level/Raan_level.txt
         ')

  # Move output
  system('mv *_temp.txt* ../stm_output')
  system('mv *_weather* ../stm_output')
  system('mv *_rates* ../stm_output')

  # Read in calculated temperatures
  mod <- data.frame()
  ff <- list.files('../stm_output', pattern = 'temp.txt')
  for (i in ff) {
    d <- read.table(paste0('../stm_output/', i), skip = 2, header = TRUE)
    names(d) <- c('dos', 'doy', 'year', 'mass.slurry', 'mass.frozen', 
                  'depth.slurry', 'temp.air', 'temp.wall', 'temp.floor', 
                  'temp.slurry')
    d$site <- substr(i, 1, 4)
    mod <- rbind(mod, d)
  }

  mod$year <- 2019 + mod$year
  mod$date <- as.POSIXct(paste(mod$year, mod$doy), format = '%Y %j')

  # Merge measured and calculated
  dat <- merge(meas[, c('site', 'date', 'temp')], mod[, c('site', 'date', 'temp.slurry')], by = c('site', 'date'))
  nddat <<- dat

  res <- dat$temp.slurry - dat$temp
  obj <- sum(abs(res))

  return(obj)
}

# Initial par guesses
p <- c(Rair = 0.02, Rconc = 0.15, Rslur = 0.7, Rsoil = 1.0, absorp = 0.02, soilDamp = 3)
m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')
m

#p <- m$par
#m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')
#m
#
#p <- c(Rair = 0.015, Rconc = 0.6, Rslur = 0.6, Rsoil = 1.5, absorp = 0.02, soilDamp = 3)
#m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')
#m
#
#p <- c(Rair = 0.04, Rconc = 1.3, Rslur = 0.6, Rsoil = 0.8, absorp = 0.03, soilDamp = 2.5)
#m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')
#m
#
#m1 <- m


##p <- c(Rair = 0.02, Rconc = 0.15, Rslur = 0.7, Rsoil = 1.0, absorp = 0.02, soilDamp = 1)
##p <- abs(m$par)
##mb <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'L-BFGS-B', 
##            lower = c(Rair = 0.001, Rconc = 0.001, Rslur = 0.001, Rsoil = 0.001, absorp = 0.0, soilDamp = 0.1),
##            upper = c(Rair = 10, Rconc = 10, Rslur = 10, Rsoil = 10, absorp = 1.0, soilDamp = 6))
##mb
##
##p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.01, soilDamp = 3)
##p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.01)
##p <- c(glSlur = 3, glConc = 0.3, glSoil = 0.3, absorp = 0.01)
##fixed <- c(soilDamp = 6, uAir = 50)
##fixed <- c(soilDamp = 6, uAir = 50)
##
###p <- c(uAir = 100, glConc = 1, glSlur = 0.3, absorp = 0.01, soilDamp = 3)
##
### Optimize
##p <- c(glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
##fixed <- c(uAir = 50, glConc = 0.15, soilDamp = 4)
##p <- c(uAir = 50, glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
##fixed <- c(glConc = 0.15, soilDamp = 4)
##
##m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas, fixed = fixed), method = 'Nelder-Mead')
##mm <- m
##
### Used this last
##p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.02, soilDamp = 3)
##m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')
##
##m
##mm
##
##p <- c(glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
##fixed <- c(uAir = 50, glConc = 0.15, soilDamp = 4)
##mb <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas, fixed = fixed), method = 'L-BFGS-B', 
##            lower = c(uAir = 1, glSlur = 0.01, glConc = 0.01, glSoil = 0.01, absorp = 0, soilDamp = 0.1),
##            upper = c(uAir = 500, glSlur = 3, glConc = 3, glSoil = 0.01, absorp = 1.0, soilDamp = 10))
##
##mb
