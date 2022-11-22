##
## Investigation of temperature integrated 
## over different floodplain areas
##

library(lubridate)
library(zoo)
library(xts)

streamtemp <- read.table("new_analysis_fall22/post_restoration/avg_stream_temp_post.txt",
                         col.names = c("SolutionTime", "Temperature"),
                         nrows = 599)
upstream <- read.table("new_analysis_fall22/post_restoration/avg_upstream_10m_temp.txt",
                         col.names = c("SolutionTime", "Temperature"),
                         nrows = 599)
downstream <- read.table("new_analysis_fall22/post_restoration/avg_downstream_15m_temp.txt",
                       col.names = c("SolutionTime", "Temperature"),
                       nrows = 599)
floodplain <- read.table("new_analysis_fall22/post_restoration/avg_floodplain surface_only_temp.txt",
                         col.names = c("SolutionTime", "Temperature"),
                         nrows = 599)
fullsurface <- read.table("new_analysis_fall22/post_restoration/avg_restoration_reach_surface.txt",
                         col.names = c("SolutionTime", "Temperature"),
                         nrows = 599)


lubritime <- mdy_hms("01-01-2020 00:00:00") + (upstream$SolutionTime - upstream$SolutionTime[1])
outputdays <- as.character(lubritime[seq(1,599, by = 24)])
up <- xts(zoo(upstream$Temperature, 
              order.by = lubritime))
down <- xts(zoo(downstream$Temperature, 
              order.by = lubritime))
stream <- xts(zoo(streamtemp$Temperature, 
                  order.by = lubritime))
fp <- xts(zoo(floodplain$Temperature, 
              order.by = lubritime))
fullreach <- xts(zoo(fullsurface$Temperature, 
                     order.by = lubritime))

for(idx in 1:24){
  plot.zoo(up[outputdays[idx]], col = "navy", 
           lwd = 2, 
           ylim = c(min(up[outputdays[idx]], down[outputdays[idx]]),
                    max(up[outputdays[idx]], down[outputdays[idx]])),
           main = outputdays[idx])
  lines(as.zoo(down[outputdays[idx]]), col = "dodgerblue", lwd = 2)
}

plot.zoo(fp, lwd = 2, col = "goldenrod")
lines(as.zoo(fullreach), col = "forestgreen", lwd = 2)
lines(as.zoo(stream), col = "blue", lwd = 2)





