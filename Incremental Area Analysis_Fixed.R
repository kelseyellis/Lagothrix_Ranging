#import.csv "xxxx.csv" in xxxx Folder in Kelsey UTBox
dataxy<-file.choose()
hr<-read.csv(dataxy, header = TRUE, sep = ",")
head(hr)

#For entire study periods
# extract x and y and add them to a data frame with id
Fx<-hr$Final_x
Fy<-hr$Final_y
Fid<-hr$GroupID
FxFy<-cbind(Fx, Fy)
head(FxFy)

mydata<-data.frame(x = Fx,y = Fy, id = Fid)
head (mydata)
xy<-mydata[,c('x','y')]
head(xy)
id<-mydata[,'id']


##load dplyr
library(dplyr)

# mcp and kernel ranges determined with adehabitat library
# adehabitatHR should not be loaded!
detach("package:adehabitatHR", unload=TRUE)
library(adehabitat)

###iterative loop that samples each group at intervals of 10 so 10,20,30, etc
findarea<-data.frame()  
  for(i in 1:300){
      g<-mydata%>%
        group_by(id) %>%
        sample_n(i*10, replace = FALSE)
        g
      
      Fx<-g$x
      Fy<-g$y
      Fid<-g$id
      FxFy<-cbind(Fx, Fy)
      head(FxFy)
      
      g<-data.frame(x = Fx,y = Fy, id = Fid)
      xy<-g[,c('x','y')]
      head(xy)
      id<-g[,'id']
      
      mcphrarea95<-mcp.area(xy,id,percent=95,plotit=F)  
      findarea<-rbind(findarea, mcphrarea95)
    }
findarea
samples<-1:nrow(findarea)
samples*10
output<-cbind(samples, findarea)
output


write.table(output, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/OutputTables/IAA_2014-2015_CDGP.csv",row.names=TRUE,col.names=TRUE,sep=",")



