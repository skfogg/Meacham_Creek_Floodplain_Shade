##
## Manuscript plot: 
##


library(xts)
library(zoo)
library(lubridate)
source("includeNATimes.R")

river <- read.table("meachamwatertemp.txt", col.names = c("s", "e", "temp"))
riverx <- xts(zoo(river$temp, order.by = mdy_hms("01-01-2020 00:00:00") + river$s ))

meachRiver <- read.csv("watertemp.csv")
meachRiverx <- includeNATimes(xts(zoo(meachRiver$Temp, order.by = ymd_hms(meachRiver$DateTime))))

air <- read.table("meachamsunnyair.txt", skip = 1, col.names = c("s", "e", "temp"), nrows = 87601)
airx <- xts(zoo(air$temp, order.by = mdy_hms("01-01-2020 00:00:00") + air$s ))

meachAir <- read.csv("sunnyatm.csv")
sunnyairx <- includeNATimes(xts(zoo(meachAir$Temp, order.by = mdy_hms(meachAir$DateTime))))

png("plots/input_river_and_air_manuscript.png", width = 1000*5, height = 850*5, res = 72*5)
par(mfrow = c(2,1),
    oma = c(2,3,0,0),
    cex.axis = 2,
    cex.lab = 2, 
    mar = c(4,3,1,1))
plot.zoo(riverx["2020/2021"], col = "gray", ylim = c(0,25),
         ylab = "",
         xlab = "")
lines(as.zoo(meachRiverx))
legend("topright", c("Measured", "Modeled"), col = c("black", "gray"), lwd = 2, bty = "n", cex = 1.5)
text(x = ymd_hms("2020-01-13 00:00:00"), y = 22.5, "A", cex = 6)

plot.zoo(as.zoo(airx["2020/2021"]), col = "gray75", ylim = c(-15, 55),
         ylab = "",
         xlab = "")
lines(as.zoo(sunnyairx))
mtext(expression(paste("Temperature (", degree, "C)")), side = 2, outer = T, cex = 2)
text(x = ymd_hms("2020-01-13 00:00:00"), y = 47, "B", cex = 6)
legend("topright", c("Measured", "Modeled"), col = c("black", "gray"), lwd = 2, bty = "n", cex = 1.5)
mtext("Time", side = 1, outer = T, cex = 2, line = 0)

dev.off()



