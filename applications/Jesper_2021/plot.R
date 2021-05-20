# Plots temperature predictions
# S. Hafner
# 2021

# Normal simulation
d01b <- read.table('temp_01_b.txt', header = FALSE, skip = 2)
d02s <- read.table('temp_02_s.txt', header = FALSE, skip = 2)
names(d01b) <- names(d02s) <- c('dos', 'doy', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry')

png('slurry_temp.png', height = 4, width = 5, units = 'in', res = 600)
  #par(mfrow = c(3, 1), mar = c(4, 4.5, 2, 1))

  plot(slurry ~ dos, data = d02s, type = 'l', ylim = c(0, 32), xlab = 'Day of year', ylab = expression('Temperature'~(degree*C)), xaxt = 'n')
  axis(1, at = c(0, 100, 200, 365, 465, 565, 730), labels = c(0, 100, 200, 365, 100, 200, 365))
  lines(slurry ~ dos, data = d01b, type = 'l', col = 'red')
  lines(air ~ dos, data = d02s, type = 'l', col = 'gray45', lty = 2)
  grid()
  legend('topleft', c('Small', 'Big', 'Air'), lty = c(1, 1, 2), col = c('black', 'red', 'gray45'), cex = 0.6)

dev.off()
