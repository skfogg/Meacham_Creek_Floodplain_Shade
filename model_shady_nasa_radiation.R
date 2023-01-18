library(RODBC)
library(stringr)
library(xts)
library(zoo)
library(lubridate)
source('includeNATimes.R')

##
## MODEL OF SHADY SOL-RAD FROM SUNNY SOL-RAD USING MEACHAM 2015 DATA
##
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

plot.zoo(sun)
lines(as.zoo(shade), col = "goldenrod")
abline(h = 65, col = "red", lty = 2)

## USE AFTERNOON DATA ONLY, THIS IS WHEN THE "SHADY" STATION IS ACTUALLY IN THE SHADE
plot.zoo(sun["2015-07-31 12:15:00/2015-07-31 16:45:00"], ylim =c(0,1000))
lines(as.zoo(shade["2015-07-31 12:15:00/2015-07-31 16:45:00"]), col = "goldenrod")

plot.zoo(sun["2015-08-01 12:15:00/2015-08-01 16:45:00"], ylim =c(0,1000))
lines(as.zoo(shade["2015-08-01 12:15:00/2015-08-01 16:45:00"]), col = "goldenrod")

plot.zoo(sun["2015-08-02 12:15:00/2015-08-02 16:45:00"], ylim =c(0,1000))
lines(as.zoo(shade["2015-08-02 12:15:00/2015-08-02 16:45:00"]), col = "goldenrod")

sunshadetable <- data.frame(sunny = c(coredata(sun["2015-07-31 12:15:00/2015-07-31 16:45:00"]),
                                      coredata(sun["2015-08-01 12:15:00/2015-08-01 16:45:00"]),
                                      coredata(sun["2015-08-02 12:15:00/2015-08-02 16:45:00"])),
                            shady = c(coredata(shade["2015-07-31 12:15:00/2015-07-31 16:45:00"]),
                                      coredata(shade["2015-08-01 12:15:00/2015-08-01 16:45:00"]),
                                      coredata(shade["2015-08-02 12:15:00/2015-08-02 16:45:00"])))

sunshademodel <- lm(shady ~ sunny, sunshadetable)
summary(sunshademodel)

png("plots/sunny_to_shady_regression.png", height = 500*5, width = 500*5, res = 72*5)
par(mar = c(5,5,2,2),
    oma = c(0,0,0,0), 
    lend = 1,
    bty = "l")
plot(shady ~ sunny, sunshadetable, ylim = c(0, 200), xlim = c(0,1000), 
     pch = 1,
     cex = 1.5,
     ylab = expression(paste("'Shady' Site Solar Radiation (W ", m^{-2}, ")")),
     xlab = expression(paste("'Sunny' Site Solar Radiation (W ", m^{-2}, ")")))
lines(sunshademodel$coefficients[1] + sunshademodel$coefficients[2]*c(1:1000),
      col = "navy", lwd = 3)
dev.off()

## NASA DATA ###
clearsky <- read.table("nasa_radiation.txt")
clearsky <- clearsky[,2]
plot(clearsky, type = "l")

plot(clearsky[1:48], type = "l")

shadysky <- numeric(87840)
shadysky <- clearsky

shadysky[shadysky > 65.0] <- clearsky[clearsky > 65.0]*sunshademodel$coefficients[2] + sunshademodel$coefficients[1]
plot(clearsky[1:48], type = "o")
lines(shadysky[1:48], col = "dodgerblue", type = "o")

plot(clearsky[1:20000], type = "l")
lines(shadysky[1:20000], col = "dodgerblue")


shadyskydf <- data.frame(s = signif(seq(0.0, by = 3600.0, length.out = length(shadysky)), 2), 
                         rad = shadysky)
write.table(shadyskydf, "nasa_radiation_shady.txt", row.names = F, col.names = F)

