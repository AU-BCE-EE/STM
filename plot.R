
d10cc <- read.table('temp_10cc.txt', header = FALSE, skip = 2)
d10nr <- read.table('temp_pred_10nr.txt', header = FALSE, skip = 2)
d10 <- read.table('temp_pred_10dc.txt', header = FALSE, skip = 2)
d20 <- read.table('temp_pred_20dc.txt', header = FALSE, skip = 2)
d30 <- read.table('temp_pred_30dc.txt', header = FALSE, skip = 2)
names(d10cc) <- names(d10nr) <- names(d10) <- names(d20) <- names(d30) <- c('dos', 'doy', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry')

plot(slurry ~ dos, data = d10, type = 'l', ylim = c(5, 25))
lines(air ~ dos, data = d10, type = 'l', col = 'gray45', lty = 2)
lines(slurry ~ dos, data = d20, type = 'l', col = 'orange')
lines(slurry ~ dos, data = d30, type = 'l', col = 'red')
lines(slurry ~ dos, data = d10nr, type = 'l', col = 'black')
lines(slurry ~ dos, data = d10cc, type = 'l', col = 'purple')


x <- subset(d10, doy > (365 - 153))
x$air
mean(x$air)
x$doy
d10$air

subset(d10, doy == 1)
