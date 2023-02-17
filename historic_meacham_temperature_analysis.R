##
## HISTORIC MEACHAM UPSTREAM DOWNSTREAM PLOTS
##



library("lubridate")
library("xts")
library("zoo")
source("~/Meacham_Creek_Floodplain_Shade/includeNATimes.R")

historicMC <- read.csv("historic_meacham/MeachamLoggerData.csv")
locations <- read.csv("historic_meacham/loggerLocations.csv")

## upstream of restoration (Meacham 3):
upstream <- subset(historicMC, LocationId == 1864)
upstream$WaterTemperature <- as.numeric(upstream$WaterTemperature)
upstream$ReadingDateTime <- ymd_hms(upstream$ReadingDateTime)
upstreamTemp <- xts(zoo(upstream$WaterTemperature, order.by = upstream$ReadingDateTime))


## downstream of restoration (Meacham 2):
downstream <- subset(historicMC, LocationId == 1863)
downstream$WaterTemperature <- as.numeric(downstream$WaterTemperature)
downstream$ReadingDateTime <- ymd_hms(downstream$ReadingDateTime)
downstreamTemp <- xts(zoo(downstream$WaterTemperature, order.by = downstream$ReadingDateTime))

plot.zoo(downstreamTemp["2006"], col = "seagreen3", ylim = c(8,30))
lines(as.zoo(upstreamTemp["2006"]), col = "darkorange")

plot.zoo(downstreamTemp["2013"], col = "seagreen3", ylim = c(8,30))
lines(as.zoo(upstreamTemp["2013"]), col = "darkorange")

plot.zoo(apply.daily((downstreamTemp["2006"]), "mean"), col = "seagreen3", lwd = 3,
         ylim = c(8,30), type = "o")
lines(as.zoo(apply.daily(includeNATimes(upstreamTemp["2006"]), "mean")), col = "darkorange",
      lwd = 3, type = "o")

plot.zoo(apply.daily((downstreamTemp["2013"]), "mean"), col = "seagreen3", lwd = 3,
         ylim = c(8,30), type = "o")
lines(as.zoo(apply.daily(includeNATimes(upstreamTemp["2013"]), "mean")), col = "darkorange",
      lwd = 3, type = "o")

## Max and min lines: 
# lines(as.zoo(period.max(downstreamTemp["2005"], endpoints(downstreamTemp["2005"], 'days'))), col = "seagreen3")
# lines(as.zoo(period.min(downstreamTemp["2005"], endpoints(downstreamTemp["2005"], 'days'))), col = "seagreen3")
# 
# lines(as.zoo(period.max(includeNATimes(upstreamTemp["2005"]), endpoints(downstreamTemp["2005"], 'days'))), col = "darkorange")
# lines(as.zoo(period.min(includeNATimes(upstreamTemp["2005"]), endpoints(downstreamTemp["2005"], 'days'))), col = "darkorange")

legend("topright", c("upstream", "downstream"), col = c("darkorange", "seagreen3"), lwd = 3)



yearsToPlot <- 2005:2018
yearsToPlot <- as.character(yearsToPlot)
# par(mfrow = c(7,2),
#     mar = c(3,2,2,0))
for(i in 1:length(yearsToPlot)){
  plot.zoo(apply.daily((downstreamTemp[yearsToPlot[i]]), "mean"), col = "seagreen3", lwd = 3,
           ylim = c(6,30),
           xlim = c(mdy_hms(paste0("06-01-", yearsToPlot[i], " 00:00:00")), mdy_hms(paste0("10-31-", yearsToPlot[i], " 00:00:00"))),
           main = yearsToPlot[i],
           ylab = "Temperature",
           xlab = "Month",
           type = "o"
           )
  lines(as.zoo(apply.daily(includeNATimes(upstreamTemp[yearsToPlot[i]]), "mean")), 
        col = "darkorange", lwd = 3,
        type = "o")
  
  
  ## Max and min lines: 
  lines(as.zoo(period.max(downstreamTemp[yearsToPlot[i]], endpoints(downstreamTemp[yearsToPlot[i]], 'days'))), col = "seagreen3")
  lines(as.zoo(period.min(downstreamTemp[yearsToPlot[i]], endpoints(downstreamTemp[yearsToPlot[i]], 'days'))), col = "seagreen3")

  lines(as.zoo(period.max(includeNATimes(upstreamTemp[yearsToPlot[i]]), endpoints(upstreamTemp[yearsToPlot[i]], 'days'))), col = "darkorange")
  lines(as.zoo(period.min(includeNATimes(upstreamTemp[yearsToPlot[i]]), endpoints(upstreamTemp[yearsToPlot[i]], 'days'))), col = "darkorange")

  
  # legend("topright", c("upstream", "downstream"), col = c("darkorange", "seagreen3"), lwd = 3)
}
par(mfrow = c(1,1))


yearsOfAug <- as.character(c(2006:2011, 2013:2017))
monthlyMeans_d <- apply.monthly(downstreamTemp[yearsOfAug], "mean")
monthlyMeans_u <- apply.monthly(upstreamTemp[yearsOfAug], "mean")

monthlyMeans_d[paste0(yearsOfAug, "-08-31")]
monthlyMeans_u[paste0(yearsOfAug, "-08-31")]

downstreamAug <- downstreamTemp[paste0(yearsOfAug, "-08-01/", yearsOfAug, "08-31")]
upstreamAug <-  upstreamTemp[paste0(yearsOfAug, "-08-01/", yearsOfAug, "08-31")]


augustData <- data.frame(temperature = coredata(downstreamAug[yearsOfAug[1]]), 
                         year = yearsOfAug[1],
                         datetime = index(downstreamAug[yearsOfAug[1]]),
                         site = "downstream")
for(i in 2:length(yearsOfAug)){
  augustData <- rbind(augustData, data.frame(temperature = coredata(downstreamAug[yearsOfAug[i]]), 
                               year = yearsOfAug[i],
                               datetime = index(downstreamAug[yearsOfAug[i]]),
                               site = "downstream")
        )
}
for(i in 1:length(yearsOfAug)){
  augustData <- rbind(augustData, data.frame(temperature = coredata(upstreamAug[yearsOfAug[i]]), 
                                             year = yearsOfAug[i],
                                             datetime = index(upstreamAug[yearsOfAug[i]]),
                                             site = "upstream")
  )
}

augustData$year <- as.factor(augustData$year)
augustData$site <- as.factor(augustData$site)
augustData$site <- relevel(augustData$site, "upstream")

write.csv(augustData, "historic_meacham/augustData.csv", row.names = F)

boxplot(x ~ site + year, augustData, col = c("darkorange", "seagreen3"))
boxplot(x ~ site + year, augustData, subset = site == "upstream", col = "darkorange")
boxplot(x ~ site + year, augustData, subset = site == "downstream", col = "seagreen3")

apply.daily(downstreamAug, "max")
apply.daily(upstreamAug, "max")

### August warming:
dailyAugMean_d <- apply.daily(downstreamAug, "mean")
dailyAugMean_u <- apply.daily(upstreamAug, "mean")
augDiff <- dailyAugMean_d - dailyAugMean_u

plot(augDiff)


#average august downstream warming across reach :
avg_aug_warming <- apply.yearly(augDiff, "mean")
plot(yearsOfAug, coredata(avg_aug_warming), cex = 2, pch = 19)


## daily maximum differences:
dailyAugMax_d <- apply.daily(downstreamAug, "max")
dailyAugMax_u <- apply.daily(upstreamAug, "max")
augMaxDiff <- dailyAugMax_d - dailyAugMax_u

avg_aug_max_diff <- apply.yearly(augMaxDiff, "mean")
plot(yearsOfAug, coredata(avg_aug_max_diff), cex = 2, pch = 19, col = "red")


## daily minimum differences:
dailyAugMin_d <- apply.daily(downstreamAug, "min")
dailyAugMin_u <- apply.daily(upstreamAug, "min")
augMinDiff <- dailyAugMin_d - dailyAugMin_u

avg_aug_min_diff <- apply.yearly(augMinDiff, "mean")
plot(yearsOfAug, coredata(avg_aug_min_diff), cex = 2, pch = 19, col = "dodgerblue")



plot(coredata(apply.daily(downstreamAug[yearsOfAug[1]], "max")), type = "l", ylim = c(6,29))
mapply(function(y,c) lines(coredata(apply.daily(downstreamAug[y], "max")), col = c, type = "l", lwd =2),
       y = yearsOfAug,
       c = hcl.colors(length(yearsOfAug)))
