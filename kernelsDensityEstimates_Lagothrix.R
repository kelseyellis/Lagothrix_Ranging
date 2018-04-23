#import.csv "xxxx.csv" in xxxx Folder in Kelsey Dropbox folder
#use LagoLocsByGroup2014-2015noNAforR from UT box Locations folder in Dissertate
dataxy<-file.choose()
hr<-read.csv(dataxy, header = TRUE, sep = ",")
head(hr)
names(hr)


#For entire study periods
# extract x and y and add them to a data frame with id
Fx<-hr$Final_x
Fy<-hr$Final_y
Fid<-hr$GroupID
FxFy<-cbind(Fx, Fy)
head(FxFy)


#For monthly group ranges of a single group
hrg<-hr[hr$GroupID == "Lagothrix P",]
Fx<-hrg$Final_x
Fy<-hrg$Final_y
Fid<-hrg$MoYR
FxFy<-cbind(Fx, Fy)
head(FxFy)

#group home ranges by month
month<-hr[hr$Month=="8" & hr$Year=="2015",]
head(month)
Fx<-month$Final_x
Fy<-month$Final_y
Fid<-month$GroupID
FxFy<-cbind(Fx, Fy)
head(FxFy)

mydata2<-sample_n(mydata, 300)
mydata2

###NOW Continue to build DF
mydata<-data.frame(x = Fx,y = Fy, id = Fid)
head (mydata)

xy<-mydata[,c('x','y')]

id<-mydata[,'id']


# mcp and kernel ranges determined with adehabitat library
# adehabitatHR should not be loaded!
detach("package:adehabitatHR", unload=TRUE)
library(adehabitat)

mcphr95<-mcp(xy,id,percent=95)
#mcphr75<-mcp(xy,id,percent=75)
mcphr50<-mcp(xy,id,percent=50)
plot(mcphr95)

mcphrarea95<-mcp.area(xy,id,percent=95,plotit=TRUE)
mcphrarea95

mcphrarea100<-mcp.area(xy,id,percent=100,plotit=TRUE)
mcphrarea100

MCP_Out<-rbind(mcphrarea95, mcphrarea100)
MCP_Out

write.table(MCP_Out, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/OutputTables/mcparea_2008_DG&C.csv",row.names=TRUE,col.names=TRUE,sep=",")


###For Kernel estimate using grid size of 100 and manual smoothing paramter of 85
khr<-kernelUD(xy,id,h=85,grid=100,same4all=TRUE)


# now load adehabitatHR
library(adehabitatHR)
# update adehabitat data class to adehabitatHR dataclass
khrestUDm<-khr2estUDm(khr)

khrestUDmvert50<-getverticeshr(khrestUDm,percent=50)
#khrestUDmvert80<-getverticeshr(khrestUDm,percent=80)
khrestUDmvert95<-getverticeshr(khrestUDm,percent=95)


khroverlapHR95<-kerneloverlaphr(khrestUDm,method="HR",percent=95)
khroverlapPHR95<-kerneloverlaphr(khrestUDm,method="PHR",percent=95)
khroverlapVI95<-kerneloverlaphr(khrestUDm,method="VI",percent=95)
khroverlapUDOI95<-kerneloverlaphr(khrestUDm,method="UDOI",percent=95)
khroverlapHR50<-kerneloverlaphr(khrestUDm,method="HR",percent=50)
khroverlapPHR50<-kerneloverlaphr(khrestUDm,method="PHR",percent=50)
khroverlapVI50<-kerneloverlaphr(khrestUDm,method="VI",percent=50)
khroverlapUDOI50<-kerneloverlaphr(khrestUDm,method="UDOI",percent=50)
#khroverlapHR80<-kerneloverlaphr(khrestUDm,method="HR",percent=80)
#khroverlapPHR80<-kerneloverlaphr(khrestUDm,method="PHR",percent=80)
#khroverlapVI80<-kerneloverlaphr(khrestUDm,method="VI",percent=80)
#khroverlapUDOI80<-kerneloverlaphr(khrestUDm,method="UDOI",percent=80)


# to export as shapefiles use library rgdal
library(rgdal)
#write files to desktop
#Make sure to change directory location when using a different computer and to tell it whch folder to go to

#For LSCV
#directory<-"C:/Users/Kelsey/Box Sync/EulemurDataAnalysis/EulemurCleanup/GISstuff/July2016Folder/LSCV"
setwd(directory)

#For Href
#directory<-"C:/Users/Kelsey/Box Sync/EulemurDataAnalysis/EulemurCleanup/GISstuff/July2016Folder/HRef"
setwd(directory)

#For manual input of H
setwd("C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/Shapefiles")
getwd()


writeOGR(khrestUDmvert50,getwd(),"kernelHR-50-2008_DG&C-85",driver="ESRI Shapefile")
#writeOGR(khrestUDmvert75,getwd(),"kernelHR-75-Nov2014Ranging_CGDP-sp75",driver="ESRI Shapefile")
writeOGR(khrestUDmvert95,getwd(),"kernelHR-95-2008_DG&C-sp85",driver="ESRI Shapefile")


#to export overlap matrices

write.table(khroverlapHR50, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_HR_overlap_50.csv",sep=",")
write.table(khroverlapHR95, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_HR_overlap_95.csv",sep=",")
write.table(khroverlapUDOI50, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_UDOI_overlap_50.csv",sep=",")
write.table(khroverlapUDOI95, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_UDOI_overlap_95.csv",sep=",")
write.table(khroverlapPHR50, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_PHR_overlap_50.csv",sep=",")
write.table(khroverlapPHR95, "C:\\Users\\Kme479\\Box Sync\\Dissertate\\LocationCleaning&Analyses\\OutputTables\\Aug2015_PHR_overlap_95.csv",sep=",")


#To LSCV folder
#write.table(khroverlapHR50,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\LSCV\\Overlap50-all.csv",sep=",")
#write.table(khroverlapHR95,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\LSCV\\Overlap95-all.csv",sep=",")

#To HRef folder
write.table(khroverlapHR50,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\HRef\\Overlap50-all.csv",sep=",")
write.table(khroverlapHR95,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\HRef\\Overlap95-all.csv",sep=",")

#To manual H folder
write.table(khroverlapHR50,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\HManual\\Overlap50-all-50sp.csv",sep=",")
write.table(khroverlapHR95,"C:\\Users\\Kelsey\\Box Sync\\EulemurDataAnalysis\\EulemurCleanup\\GISstuff\\July2016Folder\\HManual\\Overlap95-all-50sp.csv",sep=",")
khroverlapHR50
khroverlapHR95



# Save multiple objects

save(khrestUDmvert50groupC, khrestUDmvert50groupD, khrestUDmvert50groupG, khrestUDmvert50groupP, file = "Monthly_CoreAreaCDGP.RData")
save(khrestUDmvert95groupC, khrestUDmvert95groupD, khrestUDmvert95groupG, khrestUDmvert95groupP, file = "Monthly_HR_CDGP.RData")


