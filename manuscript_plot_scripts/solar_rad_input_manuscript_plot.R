##
## PLOTS FOR MANUSCRIPT
##


##
##### SOLAR RADIATION INPUT PLOTS ####
##

library(RODBC)
library(stringr)
library(xts)
library(zoo)
library(lubridate)
source('includeNATimes.R')


## Nasa radiation data
clearskytable <- read.table("nasa_radiation.txt", col.names = c("s","rad"))
shadetable <- read.table("nasa_radiation_shady.txt", col.names = c("s", "rad"))

times <- ymd_hms("2015-01-01 00:00:00", tz = "America/Denver") + seq(0, by = 3600, length.out = nrow(clearskytable))
clearsky <- xts(zoo(clearskytable$rad, order.by = times))
shady <- xts(zoo(shadetable$rad, order.by = times))

## Shady model data
meacham <- odbcConnect("meacham_heat")
ssunnywspy <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SSunnyWSPyranometerUp%')")

sshadywspy <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SShadyWSPyranometerUp%')")

sun <- xts(zoo(ssunnywspy$value, order.by = ssunnywspy$observationTime))
shade <- xts(zoo(sshadywspy$value, order.by = sshadywspy$observationTime))

sunshadetable <- data.frame(sunny = c(coredata(sun["2015-07-31 12:15:00/2015-07-31 16:45:00"]),
                                      coredata(sun["2015-08-01 12:15:00/2015-08-01 16:45:00"]),
                                      coredata(sun["2015-08-02 12:15:00/2015-08-02 16:45:00"])),
                            shady = c(coredata(shade["2015-07-31 12:15:00/2015-07-31 16:45:00"]),
                                      coredata(shade["2015-08-01 12:15:00/2015-08-01 16:45:00"]),
                                      coredata(shade["2015-08-02 12:15:00/2015-08-02 16:45:00"])))

sunshademodel <- lm(shady ~ sunny, sunshadetable)


## Plot ##
png("plots/solar_model_and_ts.png", width = 1500*5, height = 600*5, res = 72*5)
par(bty = "l",
    mar = c(7,7,1,2),
    oma = c(0,0,0,0),
    cex.axis = 2, 
    cex.lab = 2)
layout(matrix(c(1,2,2), 1, 3))

plot(shady ~ sunny, sunshadetable, ylim = c(0, 200), xlim = c(0,1000), 
     pch = 1,
     cex = 2,
     ylab = "",
     xlab = "")
lines(sunshademodel$coefficients[1] + sunshademodel$coefficients[2]*c(1:1000),
      col = "black", lwd = 3)
mtext(expression(paste("'Shady' Site Solar Radiation (W ", m^{-2}, ")")), 
      side = 2, line = 4, cex = 1.5)
mtext(expression(paste("'Sunny' Site Solar Radiation (W ", m^{-2}, ")")), 
      side = 1, line = 4, cex = 1.5)
text(x = 200, y = 15, expression(y == paste(37.36 + 0.082, x)), cex =2.2)
text(x = 100, y = 190, "A", cex = 9)

plot.zoo(clearsky["2015"], 
         ylab = "",
         xlab = "",
         col = "goldenrod1",
         lwd = 2,
         ylim = c(0, 1000))
lines(as.zoo(shady["2015"]), 
      col = "gray25", 
      lwd = 2,
      ylim = c(0, 1000),
      ylab = "",
      xlab = "")
legend("topright", c("Sunny", "Shady"),lwd = 2, 
       col = c("goldenrod1", "gray25"), bty = "n", title = "Radiation Time-series",
       cex = 2.5)
mtext(expression(paste("Solar Radiation (W ", m^{-2}, ")")), 
      side = 2, line = 4, cex = 1.5)
mtext("Month", 
      side = 1, line = 4, cex = 1.5)
text(x = ymd_hms("2015-01-12, 00-00-00"), y = 950, "B", cex = 9)

dev.off()
