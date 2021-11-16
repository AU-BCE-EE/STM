# Plots measured and calculated temperatures

library(ggplot2)
library(lubridate)

# Measurements
dat <- read.csv('meas_temp/Sweden_storage_temp.csv')
dat$date.time <- ymd_hm(dat$date.time)
dat$date <- as.POSIXct(substr(dat$date.time, 1, 10))
dat$depth <- paste(dat$depth, ' m')

# Fix site
dat$site <- substr(dat$site, 1, 4)
dat$site[dat$site == 'Rånä'] <- 'Raan'

# Average temperature
dm <- aggregate(temp ~ site + date, data = dat, FUN = mean)

# Tjele data
ddt <- read.csv('meas_temp/Tjele_storage_temp.csv')
ddt$date.time <- ymd_hm(ddt$date.time)
ddt$date <- as.POSIXct(substr(ddt$date.time, 1, 10))
dmt <- aggregate(temp ~ site + date, data = ddt, FUN = mean)

dm <- rbind(dm, dmt)

head(ddt, 2)
head(dat, 2)
dat <- rbind(dat, ddt)
# NTS: name problems with height!

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
dat <- merge(dat, mod, by = c('site', 'date'), all = TRUE)
dm <- merge(dm, mod, by = c('site', 'date'), all = TRUE)

dm$group <- 'A'
dm$group[dm$site %in% c('Back', 'Fitt')] <- 'B'

ggplot(dm, aes(date, temp, colour = site)) +
  geom_line(lty = 1) +
  geom_line(aes(date, temp.slurry), lty = 2) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Site') +
  theme(legend.position = 'top')
ggsave('plots/ave_stor_temp.png', height = 6, width = 8)

ggplot(dm, aes(date, temp)) +
  geom_line(aes(date, temp.air), colour = 'skyblue', lty = 1) +
  geom_line(col = 'red') +
  geom_line(aes(date, temp.slurry), colour = 'black') +
  facet_wrap(~ site) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/ave_stor_temp_4.png', height = 6, width = 8)

ggplot(dat, aes(date.time, temp, colour = factor(depth))) +
  geom_line(aes(date, temp.air), colour = 'skyblue', lty = 1) +
  geom_line() +
  geom_line(aes(date, temp.slurry), colour = 'black') +
  facet_wrap(~ site) +
  labs(x = 'Date', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/stor_temp_4.png', height = 6, width = 8)

ggplot(dm, aes(date, depth.slurry)) +
  geom_line() +
  facet_wrap(~ site) +
  labs(x = 'Date', y = 'Slurry depth (m)', 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/slurry_depth_4.png', height = 6, width = 8)

ggplot(dm, aes(date, mass.slurry)) +
  geom_line() +
  facet_wrap(~ site) +
  labs(x = 'Date', y = 'Slurry mass (Mg)', 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/slurry_mass_4.png', height = 6, width = 8)
