chickdiet1 <-subset(ChickWeight, ChickWeight$Diet == 1)
chickdiet2 <-subset(ChickWeight, ChickWeight$Diet == 2)
chickdiet3 <-subset(ChickWeight, ChickWeight$Diet == 3)
chickdiet4 <-subset(ChickWeight, ChickWeight$Diet == 4)
boxplot(weight~Time,data = ChickWeight, xlab="Time in days", ylab="Weight in grams", main = "Chick Weights over time")
plot(weight~Time, data = chick1)

par(mfrow=(c(4,1)))

chick1 <- subset(chickdiet1, chickdiet1$Chick == 1)
xrange <- range(chickdiet1$Time)
yrange <- range(chickdiet1$weight)
nchicks <- max(chickdiet1$Chick)

plot(xrange, yrange, type = "n", xlab = "Age (Days)", ylab = "Weight (Grams)")
colours <-rainbow(nchicks)
linetype <- c(1:nchicks)

for (i in 1:nchicks) {
  chick <-subset(chickdiet1, Chick == i)
  lines(chick$Time, chick$weight, type = "b", lwd = 1.5, lty = linetype[i], col = colours[i], pch = 21)
}
title("Chick Growth - Diet 1")
#legend(xrange[1], yrange[2], 1:nchicks, cex = 0.8, col = colours, pch = 21, lty = linetype, title = "Chicks")

chick2 <- subset(chickdiet2, chickdiet2$Chick == 21)
xrange <- range(chickdiet2$Time)
yrange <- range(chickdiet2$weight)
nchicks <- max(chickdiet2$Chick)

plot(xrange, yrange, type = "n", xlab = "Age (Days)", ylab = "Weight (Grams)")
colours <-rainbow(nchicks)
linetype <- c(1:nchicks)

for (i in 1:nchicks) {
  chick1 <-subset(chickdiet2, Chick == i)
  lines(chick1$Time, chick1$weight, type = "b", lwd = 1.5, lty = linetype[i], col = colours[i], pch = 21)
}
title("Chick Growth - Diet 2")
#legend(xrange[1], yrange[2], 1:nchicks, cex = 0.8, col = colours, pch = 21, lty = linetype, title = "Chicks")

chick3 <- subset(chickdiet3, chickdiet3$Chick == 21)
xrange <- range(chickdiet3$Time)
yrange <- range(chickdiet3$weight)
nchicks <- max(chickdiet3$Chick)

plot(xrange, yrange, type = "n", xlab = "Age (Days)", ylab = "Weight (Grams)")
colours <-rainbow(nchicks)
linetype <- c(1:nchicks)

for (i in 1:nchicks) {
  chick2 <-subset(chickdiet3, Chick == i)
  lines(chick2$Time, chick2$weight, type = "b", lwd = 1.5, lty = linetype[i], col = colours[i], pch = 21)
}
title("Chick Growth - Diet 3")
#legend(xrange[1], yrange[2], 1:nchicks, cex = 0.8, col = colours, pch = 21, lty = linetype, title = "Chicks")

chick4 <- subset(chickdiet4, chickdiet3$Chick == 21)
xrange <- range(chickdiet4$Time)
yrange <- range(chickdiet4$weight)
nchicks <- max(chickdiet4$Chick)

plot(xrange, yrange, type = "n", xlab = "Age (Days)", ylab = "Weight (Grams)")
colours <-rainbow(nchicks)
linetype <- c(1:nchicks)

for (i in 1:nchicks) {
  chick3 <-subset(chickdiet4, Chick == i)
  lines(chick3$Time, chick3$weight, type = "b", lwd = 1.5, lty = linetype[i], col = colours[i], pch = 21)
}
title("Chick Growth - Diet 4")
#legend(xrange[1], yrange[2], 1:nchicks, cex = 0.8, col = colours, pch = 21, lty = linetype, title = "Chicks")

max_increase1 <- chickdiet1$weight[which.max(chickdiet1$weight)]
max_increase2 <- chickdiet2$weight[which.max(chickdiet2$weight)]
max_increase3 <- chickdiet3$weight[which.max(chickdiet3$weight)]
max_increase4 <- chickdiet4$weight[which.max(chickdiet4$weight)]
avg_increase1 <- chickdiet1$weight[mean(chickdiet1$weight)]
avg_increase2 <- chickdiet2$weight[mean(chickdiet2$weight)]
avg_increase3 <- chickdiet3$weight[mean(chickdiet3$weight)]
avg_increase4 <- chickdiet4$weight[mean(chickdiet4$weight)]