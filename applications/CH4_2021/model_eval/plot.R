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

mod$year <- 2018 + mod$year
mod$date <- as.POSIXct(paste(mod$year, mod$doy), format = '%Y %j')
# Drop start-up year
mod <- subset(mod, year > 2019)

# Add in model predictions by date
dat <- merge(dat, mod, by = c('site', 'date'), all = TRUE)
dm <- merge(dm, mod, by = c('site', 'date'), all = TRUE)

dm <- dm[order(dm$site, dm$date), ]

# Very strange that some parts of the temp (meas) lines are not plotted with geom_line()
# Can eliminate by dropping temp == NA
ggplot(dm, aes(doy, temp)) +
  geom_line(aes(doy, temp.air), colour = 'skyblue', lty = 1) +
  geom_line(aes(doy, temp.slurry), colour = 'black') +
  geom_path(col = 'red') +
  facet_wrap(~ site) +
  labs(x = 'Day of year', y = expression('Temperature'~(degree*C)), 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/ave_stor_temp_doy.png', height = 6, width = 8)

ggplot(dm, aes(doy, depth.slurry)) +
  geom_line() +
  facet_wrap(~ site) +
  labs(x = 'Date', y = 'Slurry depth (m)', 
       colour = 'Position (from surface)') +
  theme(legend.position = 'top')
ggsave('plots/slurry_depth_doy.png', height = 6, width = 8)
