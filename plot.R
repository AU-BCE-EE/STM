d2 <- read.table('temp_pred_0002.txt', header = FALSE, skip = 2)
d3 <- read.table('temp_pred_0003.txt', header = FALSE, skip = 2)
d4 <- read.table('temp_pred_0004.txt', header = FALSE, skip = 2)
names(d2) <- names(d3) <- names(d4) <- c('dos', 'doy', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry')
names(d2) <- c('dos', 'doy', 'smass', 'depth', 'air', 'wall', 'floor', 'slurry', 'qout')
head(d2)

plot(slurry ~ dos, data = d2, type = 'l', ylim = c(0, 32))
lines(air ~ dos, data = d2, type = 'l', col = 'gray45', lty = 2)
lines(slurry ~ dos, data = d3, type = 'l', col = 'orange')
lines(slurry ~ dos, data = d4, type = 'l', col = 'red')

d2$air
