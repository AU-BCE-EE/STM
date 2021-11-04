# Plots temperature data from Kristina

library(ggplot2)
library(lubridate)

# Measurements
dat <- read.csv('meas_temp/temperature.csv')
dat$date.time <- ymd_hm(dat$date.time)
dat$date <- as.POSIXct(substr(dat$date.time, 1, 10))
dat$height <- paste(dat$height, ' m')

# Fix site
dat$site <- substr(dat$site, 1, 4)
dat$site[dat$site == 'Rånä'] <- 'Raan'

# Average temperature
dm <- aggregate(temp ~ site + date, data = dat, FUN = mean)

# Model results
mod <- data.frame()
ff <- list.files('stm_output', pattern = 'temp.txt')
for (i in ff) {
  d <- read.table(paste0('stm_output/', i), skip = 2, header = TRUE)
  names(d) <- c('dos', 'doy', 'year', 'mass.slurry', 'mass.frozen', 
                'depth.slurry', 'temp.air', 'temp.wall', 'temp.floor', 
                'temp.slurry')
  d$site <- substr(i, 1, 4)
  mod <- rbind(mod, d)
}

mod$year <- 2019 + mod$year
mod$date <- as.POSIXct(paste(mod$year, mod$doy), format = '%Y %j')

# Add in model predictions by date
dat <- merge(dat, mod, by = c('site', 'date'), all = FALSE)
dm <- merge(dm, mod, by = c('site', 'date'), all = FALSE)

ggplot(dm, aes(date, temp)) +
  geom_line(col = 'red') +
  geom_line(aes(date, temp.slurry), colour = 'black') +
  geom_line(aes(date, temp.air), colour = 'skyblue', lty = 2) +
  facet_wrap(~ site) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/ave_stor_temp_4.png', height = 6, width = 8)

ggplot(dat, aes(date.time, temp, colour = factor(height))) +
  geom_line() +
  geom_line(aes(date, temp.slurry), colour = 'black') +
  geom_line(aes(date, temp.air), colour = 'skyblue', lty = 2) +
  facet_wrap(~ site) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/stor_temp_4.png', height = 6, width = 8)

ggplot(dat, aes(date.time, depth.slurry)) +
  geom_line() +
  facet_wrap(~ site) +
  labs(x = 'Date', y = 'Slurry depth (m)', 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/slurry_depth_4.png', height = 6, width = 8)

ggplot(dat, aes(date.time, mass.slurry)) +
  geom_line() +
  facet_wrap(~ site) +
  labs(x = 'Date', y = 'Slurry mass (Mg)', 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/slurry_mass_4.png', height = 6, width = 8)
