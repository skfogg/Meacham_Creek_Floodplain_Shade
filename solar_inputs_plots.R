
library(lubridate)
library(xts)
library(zoo)

clearskytable <- read.table("nasa_radiation.txt", col.names = c("s","rad"))
shadetable <- read.table("nasa_radiation_shady.txt", col.names = c("s", "rad"))

times <- ymd_hms("2015-01-01 00:00:00", tz = "America/Denver") + seq(0, by = 3600, length.out = nrow(clearskytable))
clearsky <- xts(zoo(clearskytable$rad, order.by = times))
shady <- xts(zoo(shadetable$rad, order.by = times))


plot.zoo(clearsky["2015-07-31/2015-08-04"], col = "orange", lwd = 2)
lines(as.zoo(sun), col = "gold", lwd = 2)
lines(as.zoo(shade), col = "forestgreen", lwd = 2)
lines(as.zoo(shady), col = "limegreen", lwd = 2)

png("plots/solar_inputs.png", width = 1000*5, height = 800*5, res = 72*5)
par(bty = "l",
    mar = c(2,2,1,2),
    mfrow = c(2,1),
    oma = c(3,3,1,0),
    cex.axis = 1.5, 
    cex.lab = 1.5)
plot.zoo(clearsky["2015"], 
         ylab = expression(paste("Solar Radiation (W ", m^{-2}, ")")),
         xlab = "Month",
         col = "goldenrod1",
         lwd = 2,
         ylim = c(0, 1000))
plot(as.zoo(shady["2015"]), 
     col = "gray25", 
     lwd = 2,
     ylim = c(0, 1000),
     ylab = expression(paste("Solar Radiation (W ", m^{-2}, ")")),
     xlab = "Month",)
mtext("Month", side = 1, line = 1, outer = T, cex = 1.5)
mtext(expression(paste("Solar Radiation (W ", m^{-2}, ")")), side = 2, line = 1, outer = T, cex = 1.5)
dev.off()



