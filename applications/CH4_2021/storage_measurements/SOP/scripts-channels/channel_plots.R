
library(readxl)
library(lubridate)
library(ggplot2)

ff <- list.files(fp <- '../data-channels')
dat <- data.frame()

for (i in ff) {
  dd <- as.data.frame(read_xlsx(paste0(fp, '/', i), sheet = 2, skip = 6))[, 1:3]
  names(dd) <- c('rnum', 'date.time', 'temp')
  dd$site <- gsub(' [0-9]+-[0-9]+.xlsx', '', i)
  dat <- rbind(dat, dd)
}

dat$date <- as.Date(as.character(ymd_hms(dat$date.time), format = '%Y-%m-%d'))
dat$doy <- as.numeric(as.character(ymd_hms(dat$date.time), format = '%j'))
dat$cooling <- grepl('[Cc]ooling', dat$site)

ggplot(dat, aes(doy, temp, colour = site, lty = cooling)) +
  geom_line() +
  labs(lty = 'Cooling', colour = 'Site', x = 'Day of year', y = expression('Slurry (channel) temperature'~(degree*C))) +
  theme(legend.position = 'top')
ggsave('../plots/channel_temp_doy.png', height = 5, width = 8)

ggplot(dat, aes(date, temp, colour = site, lty = cooling)) +
  geom_line() +
  labs(lty = 'Cooling', colour = 'Site', x = 'Date', y = expression('Slurry (channel) temperature'~(degree*C))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 90))
ggsave('../plots/channel_temp_date.png', height = 5, width = 8)
