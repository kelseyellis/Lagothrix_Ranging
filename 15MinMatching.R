library(dplyr)
library(xts)
library(lubridate)

setwd("C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses")
getwd()

###Bring in first set of GPS points
ALL_GPS<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(ALL_GPS)
names(ALL_GPS)
unique(ALL_GPS$GPS)

GPS1<-filter(ALL_GPS, GPS == "GPS IDPAS")
head(GPS1)

GPS2<-filter(ALL_GPS, GPS=="GPS PP13")
head(GPS2)

GPS3<-filter(ALL_GPS, GPS == "GPS PP10")
head(GPS3)

merge1<-merge(GPS1, GPS2[, c("GPS", "realtime", "utm_x", "utm_y")], by="realtime", all = TRUE)
merge1
names(merge1)


#If only two GPS tracks to test distances use this
merge1$Dist1.2<-sqrt((merge1$utm_x.y-merge1$utm_x.x)^2 + (merge1$utm_y.y-merge1$utm_y.x)^2)
merge1

write.table(merge1, file= "C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/15MinGPS/December2015/Distances_2mins.csv",row.names=TRUE,col.names=TRUE,sep=",")

#if testing distances between 3 GPS tracks use this

merge2<-merge(merge1, GPS3[, c("GPS", "realtime", "utm_x", "utm_y")], by="realtime", all = TRUE)
merge2
names(merge2)

merge2$Dist1.2<-sqrt((merge2$utm_x.y-merge2$utm_x.x)^2 + (merge2$utm_y.y-merge2$utm_y.x)^2)
merge2

merge2$Dist1.3<-sqrt((merge2$utm_x-merge2$utm_x.x)^2 + (merge2$utm_y-merge2$utm_y.x)^2)
merge2

merge2$Dist2.3<-sqrt((merge2$utm_x-merge2$utm_x.y)^2 + (merge2$utm_y-merge2$utm_y.y)^2)
merge2

write.table(merge2, file= "C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/15MinGPS/December2015/Distances_2mins.csv",row.names=TRUE,col.names=TRUE,sep=",")

###Do not need to do this anymore, can run 2014 data same as 2015 data
#For2014data need to separate by GPS then put merge back based on date
GPS2014<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(GPS2014)
names(GPS2014)

GPS1<-filter(GPS2014, GPS=="GPS PP10")
head(GPS1)

GPS2<-filter(GPS2014, GPS=="GPS PP11")
head(GPS2)

GPS3<-filter(GPS2014, GPS=="GPS PP4" | GPS=="GPS PP12")
head(GPS3)

merge1<-merge(GPS1, GPS2[, c("GPS", "GroupID", "realtime", "utm_x", "utm_y")], by="realtime", all = TRUE)
merge1
names(merge1)

merge2<-merge(merge1, GPS3[, c("GPS", "GroupID", "realtime", "utm_x", "utm_y")], by="realtime", all = TRUE)
merge2
names(merge2)

merge2$Dist1.2<-sqrt((merge2$utm_x.y-merge2$utm_x.x)^2 + (merge2$utm_y.y-merge2$utm_y.x)^2)
merge2

merge2$Dist1.3<-sqrt((merge2$utm_x-merge2$utm_x.x)^2 + (merge2$utm_y-merge2$utm_y.x)^2)
merge2

merge2$Dist2.3<-sqrt((merge2$utm_x-merge2$utm_x.y)^2 + (merge2$utm_y-merge2$utm_y.y)^2)
merge2

write.table(merge2, file= "C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/15MinGPS/Distances_All_months_2014.csv",row.names=TRUE,col.names=TRUE,sep=",")
