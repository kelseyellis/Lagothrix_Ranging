library(dplyr)
library(xts)
library(lubridate)

####Merging Avistaje Tracks
####Set WD here when working on laptop####
setwd("C:/Users/Kelsey/Box Sync/Tiputini/GPS Data/Avistajes2014/Rtemp")
getwd()

####Set WD here when working at school#####
setwd("C:/Users/kme479/Box Sync/Tiputini/GPS Data 2015 from Tony/RTemp")
getwd()

###files to be merged can be found in folder below
myFiles = list.files(path="C:/Users/kme479/Box Sync/Tiputini/GPS Data 2015 from Tony/RTemp", pattern="*.txt")
myFiles
data <- lapply(myFiles, read.table, sep=",", header=FALSE)
combined.data <- do.call(rbind, data)
combined.data$file_origin <- row.names(combined.data)

###Merged data will then be output to file in folder below
###will need to add Observer column, change xutm yutm and altitude to number at 4 decimals
###then set up 15MIN column as logical using excel forumla =mod(minute(T2),15)=0
head(combined.data)
write.table(combined.data, file="C:/Users/Kme479/Box Sync/Tiputini/GPS Data/GPSData2015/MonthlyTracks15Mins/temp.csv",row.names=TRUE,col.names=TRUE,sep=",")


###NOW FILTER TRACKS and aggregate at 15 min intervals
####Set WD here when working on laptop####
#setwd("C:/Users/Kelsey/Box Sync/Dissertate/LocationCleaning&Analyses")
#getwd()

####Set WD here when working at school#####
#setwd("C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses")
#getwd()

#pull in Avistaje Tracks
Tracks<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(Tracks)
names(Tracks)

Tracks15<-filter(Tracks, X15MIN == "TRUE" | X14MIN == "TRUE" | X29MIN == "TRUE" | X44MIN == "TRUE" | X59MIN == "TRUE")
head(Tracks15)

class(Tracks15$ltimeRounded)


realtime<-as.POSIXct(Tracks15$ltimeRounded,format="%m/%d/%y %H:%M")
realtime
Tracks15Time<-cbind(Tracks15, realtime)
head(Tracks15Time)


ave_y<-aggregate(y_proj ~ realtime, Tracks15Time, mean)
ave_x<-aggregate(x_proj ~ realtime, Tracks15Time, mean)
ave_alt<-aggregate(altitude ~ realtime, Tracks15Time, mean)

output<-cbind(ave_x, ave_y[,2], ave_alt[,2])
head(output)

####ALWAYS CHANGE NAME OF OUTPUT FILE####
write.table(output, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/15MinGPS/December2015/December2015_EP_2Min.csv",row.names=TRUE,col.names=TRUE,sep=",")




