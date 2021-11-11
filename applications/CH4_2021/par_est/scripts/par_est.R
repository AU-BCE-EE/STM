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
    system(paste0('sed -i s/^', names(p)[i], '/', p[i], '/g ../pars/pars.txt'))
  }

  # Run model
  cat('. ')
  system('./stm Back ../pars/pars.txt ../pars/Back_u_pars.txt ../weather/weather.txt &
          ./stm Fitt ../pars/pars.txt ../pars/Fitt_u_pars.txt ../weather/weather.txt &
          ./stm Lind ../pars/pars.txt ../pars/Lind_u_pars.txt ../weather/weather.txt &
          ./stm Raan ../pars/pars.txt ../pars/Raan_u_pars.txt ../weather/weather.txt
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
p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.01, soilDamp = 3)
p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.01)
p <- c(glSlur = 3, glConc = 0.3, glSoil = 0.3, absorp = 0.01)
fixed <- c(soilDamp = 6, uAir = 50)
fixed <- c(soilDamp = 6, uAir = 50)

#p <- c(uAir = 100, glConc = 1, glSlur = 0.3, absorp = 0.01, soilDamp = 3)

# Optimize
p <- c(glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
fixed <- c(uAir = 50, glConc = 0.15, soilDamp = 4)
p <- c(uAir = 50, glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
fixed <- c(glConc = 0.15, soilDamp = 4)

m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas, fixed = fixed), method = 'Nelder-Mead')

# Used this last
p <- c(uAir = 50, glSlur = 0.3, glConc = 1, glSoil = 0.3, absorp = 0.02, soilDamp = 3)
m <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas), method = 'Nelder-Mead')

m

p <- c(glSlur = 0.3, glSoil = 0.3, absorp = 0.01)
fixed <- c(uAir = 50, glConc = 0.15, soilDamp = 4)
mb <- optim(par = p, fn = function(par) resCalc(p = par, meas.dat = meas, fixed = fixed), method = 'L-BFGS-B', 
            lower = c(uAir = 1, glSlur = 0.01, glConc = 0.01, glSoil = 0.01, absorp = 0, soilDamp = 0.1),
            upper = c(uAir = 500, glSlur = 3, glConc = 3, glSoil = 0.01, absorp = 1.0, soilDamp = 10))

mb
