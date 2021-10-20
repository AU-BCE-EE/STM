# Plots temperature predictions
# S. Hafner
# 2021

# Normal simulation
dat <- read.table('temp_0001.txt', header = FALSE, skip = 2)
names(dat) <- names(dat) <- c('dos', 'doy', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry')

png('slurry_temp.png', height = 4, width = 6, units = 'in', res = 600)
  #par(mfrow = c(3, 1), mar = c(4, 4.5, 2, 1))

  plot(slurry ~ dos, data = dat, type = 'l', ylim = c(0, 32), 
       xlab = 'Day of year', ylab = expression('Temperature'~(degree*C)), xaxt = 'n', las = 1)
  axis(1, at = c(c(0, 100, 200, 365), c(100, 200, 365) + 365, c(100, 200, 365) + 2*365), 
       labels = c(0, 100, 200, 365, 100, 200, 365, 100, 200, 365), las = 3)
  lines(air ~ dos, data = dat, type = 'l', col = 'gray45', lty = 2)
  grid()
  #legend('topleft', c('Small', 'Big', 'Air'), lty = c(1, 1, 2), col = c('black', 'red', 'gray45'), cex = 0.6)

dev.off()
