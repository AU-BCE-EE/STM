library(lubridate)
library(dplyr)

dat <- read.csv2('DRY_radiation_hourly_6109.csv')
names(dat)[3] <- 'rad'
dat$rad <- as.numeric(dat$rad)
dat$date.time <- ymd_h(dat$timeobs)
dat$doy <- yday(dat$date.time)

dd <- as.data.frame(summarise(group_by(dat, doy), rad = mean(rad)))


# Mean by doy

  head(dat)
unique(dat$timeobs)

plot(rad ~ doy, data = dat)
plot(rad ~ doy, data = dd)

# Shows around 100 W/m2 at day 0 and 250 W/m2 around day 180

