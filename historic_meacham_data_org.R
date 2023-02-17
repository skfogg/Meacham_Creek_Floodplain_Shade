##
## "HISTORIC" MEACHAM CREEK DATA ORGANIZATION
##

library(sp)
library(rgdal)

historicMC <- read.csv("historic_meacham/MeachamLoggerData.csv")

length(unique(historicMC$GPSEasting))

#### LOCATIONS SUMMARY TABLE ####
locationsDF <- data.frame(locationID = unique(historicMC$LocationId), siteName = NA, easting = NA, northing = NA)
locationsDF

for(i in 1:length(locationsDF$locationID)){
  locationsDF$siteName[i] <- historicMC$LoggerSite[historicMC$LocationId == locationsDF$locationID[i]][1] 
  locationsDF$easting[i] <- historicMC$GPSEasting[historicMC$LocationId == locationsDF$locationID[i]][1]
  locationsDF$northing[i] <- historicMC$GPSNorthing[historicMC$LocationId == locationsDF$locationID[i]][1]
}


points <- data.frame(x = locationsDF$easting, y = locationsDF$northing)
coordinates(points) <- ~x+y
class(points)
proj4string(points) <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +ellps=WGS84") 
points2 <- spTransform(points, CRS("+proj=longlat +datum=WGS84"))

locationsDF <- cbind(locationsDF, points2)
write.csv(locationsDF, "historic_meacham/loggerLocations.csv", row.names = F)

#### DEPLOYMENT DURATIONS OF UPSTREAM AND DOWNSTREAM OF RESTORAITON SITES TABLE ####

## upstream of restoration (Meacham 3):
upstream <- subset(historicMC, LocationId == 1864)
upstream$WaterTemperature <- as.numeric(upstream$WaterTemperature)
upstream$ReadingDateTime <- ymd_hms(upstream$ReadingDateTime)
upstreamTemp <- xts(zoo(upstream$WaterTemperature, order.by = upstream$ReadingDateTime))


yr_endpoints <- endpoints(upstreamTemp, 'years')
end <- upstreamTemp[yr_endpoints]
start <- upstreamTemp[yr_endpoints[1:15]+1]

annualDurations <- data.frame(start = index(start), end = index(end), locationID = 1864, siteName = locations[locations$locationID == 1864, 'siteName'])

## downstream of restoration (Meacham 2):
downstream <- subset(historicMC, LocationId == 1863)
downstream$WaterTemperature <- as.numeric(downstream$WaterTemperature)
downstream$ReadingDateTime <- ymd_hms(downstream$ReadingDateTime)
downstreamTemp <- xts(zoo(downstream$WaterTemperature, order.by = downstream$ReadingDateTime))


yr_endpoints_d <- endpoints(downstreamTemp, 'years')
end_d <- downstreamTemp[yr_endpoints_d]
start_d <- downstreamTemp[yr_endpoints_d[1:length(yr_endpoints_d)-1]+1]

annualDurations_d <- data.frame(start = index(start_d), 
                                end = index(end_d), 
                                locationID = 1863, siteName = locations[locations$locationID == 1863, 'siteName'])
annualDurations <- rbind(annualDurations, annualDurations_d)
write.csv(annualDurations, "historic_meacham/annualDurations.csv", row.names = F)

