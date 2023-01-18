##
## My input files
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


plot(air[air$e <= (86400*365),]$temp, col = "gold", type = "l")
lines(river[river$e <= (86400*365),]$temp, col ="dodgerblue")

plot.zoo(airx["2021-06-01/2021-06-07"], col = "orange")
lines(as.zoo(riverx["2021-06-01/2021-06-07"]), col = "dodgerblue")

#### LOOK INTO AIR MODEL AND MEASURED DATA ###
plot.zoo(airx["2020/2021"], type = "l", col = "orange", ylim = c(-15, 60))
lines(as.zoo(sunnyairx["2020/2021"]), col = "coral3")

plot.zoo(airx["2021-01-01"], type = "l", col = "orange", ylim = c(-6,10))
lines(as.zoo(sunnyairx["2021-01-01"]), col = "coral3")

plot.zoo(airx["2021-02-01"], type = "l", col = "orange", ylim = c(-6,15))
lines(as.zoo(sunnyairx["2021-02-01"]), col = "coral3")

plot.zoo(airx["2021-06-01"], type = "l", col = "orange", ylim = c(5,50))
lines(as.zoo(sunnyairx["2021-06-01"]), col = "coral3")


plot.zoo(sunnyairx["2021-06-26/2021-06-28"], type = "o", col = "orange")
abline(v = ymd_hms("2021-06-26 00:00:00") + (endpoints(index(sunnyairx["2021-05-28/2021-07-20"]), on = "days")*3600), lty = 2)


### LOOK INTO RIVER MODEL AND MEASURED DATA ####
plot.zoo(airx["2021-06-01/2021-06-07"], col = "orange", ylim = c(0,50))
lines(as.zoo(sunnyairx["2021-06-01/2021-06-07"]), col = "coral3")

lines(as.zoo(riverx["2021-06-01/2021-06-07"]), col = "dodgerblue")
lines(as.zoo(meachRiverx["2021-06-01/2021-06-07"]), col = "blue4")

### CHANGE START DATE OF INPUT TO JDAY 355 ####
plot.zoo(coredata(airx['2021-12-21/2021-12-23']), col = "orange")
lines(coredata(airx['2022-01-01/2022-01-03']), col = "red")

plot.zoo(coredata(riverx['2021-12-21/2021-12-23']), col = "blue", ylim = c(3,6))
lines(coredata(riverx['2022-01-01/2022-01-03']), col = "forestgreen")

river355 <- subset(river, s > 62208000)
air355 <- subset(air, s > 62208000)
plot(river355$temp, type = "l")
lines(river$temp, col = "dodgerblue")

river355 <- data.frame(s = seq(0, by = 3600, length.out = 70320),
                         e = seq(3600, by = 3600, length.out = 70320),
                         temp = river355$temp)
air355 <- data.frame(s = seq(0, by = 3600, length.out = 70320),
                       e = seq(3600, by = 3600, length.out = 70320),
                       temp = air355$temp)
write.csv(river355, "meachamwater355df.csv")
write.csv(air355, "meachamsunnyair355df.csv")
