library(dplyr)
library(xts)
library(chron)
library(lme4)
library(lmerTest)
library(glmm)
library(arm)

####Set WD here when working at school#####
setwd("C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses")
getwd()

dayrange<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(dayrange)
names(dayrange)

stepsC<-dayrange[dayrange$Dist != 0 & dayrange$Group == "C/",]
stepsC

stepsD<-dayrange[dayrange$Dist != 0 & dayrange$Group == "D/",]
stepsD

stepsG<-dayrange[dayrange$Dist != 0 & dayrange$Group == "G/",]
stepsG

stepsP<-dayrange[dayrange$Dist != 0 & dayrange$Group == "P/",]
stepsP

hist(stepsC$Dist, 200)
hist(stepsD$Dist, 200)
hist(stepsG$Dist, 200)
hist(stepsP$Dist, 200)

dayrange

t<-t.test(Dist~Test, dayrange)
boxplot(Dist~Test, dayrange)

rDate<-as.POSIXct(dayrange$Date, format="%m/%d/%Y")
head(rDate)
class(rDate)

rTime<-as.POSIXct(dayrange$Time, format="%H:%M")
head(rTime)
class(rTime)


dayrange_DT<-cbind(dayrange, rDate, rTime)
head(dayrange_DT)

dayrange_df<-tbl_df(dayrange_DT)


distance<-group_by(dayrange_df,Group)
distance

names(distance)

g<-summarise(distance, sum(Dist), sum(Minutes), min(rTime), max(rTime))
g

h<-summarise(distance, mean(Dist), sd(Dist), min(Dist), max(Dist), length(Dist))
h

write.table(h, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/OutputTables/DPL/DPL_summary.csv",row.names=TRUE,col.names=TRUE,sep=",")
write.table(h, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/AveDist_summary.csv",row.names=TRUE,col.names=TRUE,sep=",")


library(ggplot2)

dpl<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(dpl)
names(dpl)

#Test differences in day range between groups
cdist<-dpl[dpl$GroupID == "C/",]
cdist<-cdist$Dist
cdist
ddist<-dpl[dpl$GroupID == "D/",]
ddist<-ddist$Dist
ddist
gdist<-dpl[dpl$GroupID == "G/",]
gdist<-gdist$Dist
gdist
pdist<-dpl[dpl$GroupID == "P/",]
pdist<-pdist$Dist
pdist
kruskal.test(list(cdist, ddist, gdist, pdist))


dplSG<-dpl[dpl$AveNoJuvsInfs != "NA",] 
names(dplSG)

hist(dplSG$Dist)
hist(log(dplSG$Dist))
hist(log(dplSG$DistKm))
hist(dplSG$AveNoJuvsInfs)

###Test for monthly variation
#Mixed model with group as a random factor
dpl_Full_glmer<-glmer(Dist ~ AveNoJuvsInfs_scaled + FruitHa_scaled + + CopRate_scaled + (1|GroupID), data = dplSG, family = "gaussian" (link = "log"))
summary(dpl_Full_glmer)
qqnorm(resid(dpl_Full_glmer))
qqline(resid(dpl_Full_glmer))

cor.test(dpl$FruitKm2,dpl$CopRate)
plot(dpl$CopRate~dpl$FruitKm2)

dpl_mod2_glm<-glm(Dist ~ AveNoJuvsInfs + FruitKm2 + CopRate , data = dplSG, family = "gaussian" (link = "log"))
summary(dpl_mod2_glm)


####So far think I should use this model for predicting daily path length
###Actually use the scaled version below...significance is the same, but variables based on Z score
dpl_mod1_lmer<-lmer(log(Dist) ~ AveNoJuvsInfs + FruitKm2 + CopRate + (1|GroupID), data = dplSG, REML = F)
summary(dpl_mod1_lmer)
qqnorm(resid(dpl_mod1_lmer))
qqline(resid(dpl_mod1_lmer))

dpl_null_lmer<-lmer(log(Dist) ~ (1|GroupID), data = dplSG, REML = F)
summary(dpl_null_lmer)
qqnorm(resid(dpl_null_lmer))
qqline(resid(dpl_null_lmer))

plot(allEffects(dpl_mod1_lmer))


anova(dpl_mod1_lmer, dpl_null_lmer)
plot(allEffects(dpl_mod1_lmer))

cor.test(dpl$AveMs,dpl$AveFs)
cor.test(dpl$AveAms,dpl$Dist)
cor.test(dpl$AveAFs,dpl$Dist)
cor.test(dpl$AveInfs+dpl$AveJuvs, dpl$AveNoJuvsInfs)


###Trying to scale variables
dplSG$FruitHa_scaled <- scale(dplSG$FruitHa)[, 1]
dplSG$CopRate_scaled <- scale(dplSG$CopRate)[, 1]
dplSG$AveNoJuvsInfs_scaled <- scale(dplSG$AveNoJuvsInfs)[, 1]
dplSG$Dist_scaled<-scale(dplSG$Dist)[, 1]

#Other tests...
dpl_mod2_all<-lmer(log(Dist) ~ AveNoJuvsInfs_scaled + FruitHa_scaled + CopRate_scaled + (1|GroupID), data = dplSG, REML = F)
summary(dpl_mod2_all)
qqnorm(resid(dpl_mod2_all))
qqline(resid(dpl_mod2_all))
hist(resid(dpl_mod2_all))
shapiro.test(resid(dpl_mod2_all))
plot(allEffects(dpl_mod2_all))


dpl_mod3_all<-lmer(log(Dist) ~ CopRate + (1|MoYr), data = dpl, REML = F)
summary(dpl_mod3_all)
qqnorm(resid(dpl_mod3_all))
qqline(resid(dpl_mod3_all))
hist(resid(dpl_mod3_all))
shapiro.test(resid(dpl_mod3_all))

dpl_null_all<-lmer(log(Dist) ~ (1|GroupID), data = dplSG, REML = F)
summary(dpl_null_all)
qqnorm(resid(dpl_null_all))
qqline(resid(dpl_null_all))
hist(resid(dpl_null_all))
shapiro.test(resid(dpl_null_all))


anova(dpl_Full, dpl_mod2)
anova(dpl_mod2_all, dpl_mod3_all)
anova(dpl_mod2_all, dpl_null_all)
anova(dpl_mod3_all, dpl_null_all)


hist(log(dpl$Dist), 10)
hist(dpl$FruitKm2, 12)

plot(log(dpl$FruitHa))
plot(log(dpl$Dist))

predict2<-predict(movar2)
predict2

plot(predict2)


# grouped boxplot
# re-order the levels in the order of appearance in the data.frame
dpl$MoYr2 <- factor(dpl$MoYr, c("14-Aug", "14-Sep", "14-Oct", "14-Nov", "14-Dec", "15-Apr", "15-May", "15-Jun", "15-Jul", "15-Aug", "15-Sep", "15-Oct", "15-Nov", "15-Dec"))
dpl$MoYr2

p <- ggplot(dpl, aes(x=MoYr2, y=Dist, fill=Group)) + 
  geom_boxplot()
p<-p + scale_fill_manual(values=c("green3", "mediumpurple", "gold1", "deepskyblue"))
p<-p + labs(x = "Month and Year", y = "Distance (m)")
p


#GetQuickSummary of dayranges
dayrange_df<-tbl_df(dayrange)
dsumall<-summarise(dayrange_df, mean(TotDist), sd(TotDist), min(TotDist), max(TotDist), length(Group))
dsumall
dayrange_summ<-group_by(dayrange_df,Group)
dsummary<-summarise(dayrange_summ, mean(TotDist), sd(TotDist), min(TotDist), max(TotDist), length(Group))
dsummary

dayrange_df
rDate<-as.POSIXct(dayrange$Date, format="%m/%d/%Y")
head(rDate)
class(rDate)
d<-cbind(dayrange_df, rDate)

dpl_day<-group_by(d, rDate, Group)
dpl_day_Group<-summarise(dpl_day,mean(TotDist))
dpl_day_Group                         
write.table(dpl_day_Group, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/DPL_summary_GroupDayAverage.csv",row.names=TRUE,col.names=TRUE,sep=",")

#DPL and Fruit availability
dpl<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
dpl

C_dpl_fruit<-filter(dpl, Group == "C/")
C_dpl_fruit
plot(C_dpl_fruit$Distance, C_dpl_fruit$Fruit)
cor.test(C_dpl_fruit$Distance, C_dpl_fruit$Fruit)

D_dpl_fruit<-filter(dpl, Group == "D/")
D_dpl_fruit
plot(D_dpl_fruit$Distance, D_dpl_fruit$Fruit)
cor.test(D_dpl_fruit$Distance, D_dpl_fruit$Fruit)

G_dpl_fruit<-filter(dpl, Group == "G/")
G_dpl_fruit
plot(G_dpl_fruit$Distance, G_dpl_fruit$Fruit)
cor.test(G_dpl_fruit$Distance, G_dpl_fruit$Fruit)

P_dpl_fruit<-filter(dpl, Group == "P/")
P_dpl_fruit
plot(P_dpl_fruit$Distance, P_dpl_fruit$Fruit)
cor.test(P_dpl_fruit$Distance, P_dpl_fruit$Fruit)

LH_dpl<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
names(LH_dpl)
t.test(meters~type, LH_dpl)

##TravelRates
#Pull in AveDist_summary
trates<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(trates)
names(trates)

#Need to average day first then can do month
rDate<-as.POSIXct(trates$rDate, format="%m/%d/%Y")
head(rDate)
class(rDate)

trates_df<-tbl_df(trates)
trates_df

trates_daygrouped<-group_by(trates_df, rDate, Group)
trates_daygrouped
trates_daysumm<-summarise(trates_daygrouped, mean(MeanDist), mean(Duration), mean(Rate)) 
trates_daysumm
                       
write.table(trates_daysumm, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/OutputTables/DPL/travelrate_groupedbyday.csv",row.names=TRUE,col.names=TRUE,sep=",")

trates2<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(trates2)
names(trates2)

trates_cdgp<-filter(trates2, Group == "C/" | Group == "D/" | Group =="G/" | Group == "P/")
trates2_df<-tbl_df(trates_cdgp)

trates_cdgp$MoYr2 <- factor(trates_cdgp$MoYr, c("Aug-14", "Sep-14", "Oct-14", "Nov-14", "Dec-14", "Apr-15", "May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15", "Oct-15", "Nov-15", "Dec-15"))
trates_cdgp

kruskal.test(Group~MeanDist, trates_cdgp)
anova(lm(MeanDist~Group, trates_cdgp))

pp <- ggplot(trates_cdgp, aes(x=MoYr2, y=MeanDist, fill=Group)) + 
  geom_boxplot()
pp<-pp + scale_fill_manual(values=c("green3", "mediumpurple", "gold1", "deepskyblue"))
pp<-pp + labs(x = "Month and Year", y = "Distance (m) traveled every 15 minutes")
pp

#travel rates monthly summary
trates_mgrouped<-group_by(trates2_df, MoYr, Group)
trates_msumm_cdgp<-summarise(trates_mgrouped, mean(MeanDist), mean(Duration), mean(Rate), length(MoYr))
trates_msumm_cdgp

write.table(trates_msumm_cdgp, file="C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses/OutputTables/DPL/travelrate_groupedbymonth.csv",row.names=TRUE,col.names=TRUE,sep=",")

###Test for monthly variation in distances covered per step setting group as random factor
movar_rate<-lmer(MeanDist ~ MoYr + (1 | Group), data = trates_cdgp, REML = F)
summary(movar_rate)
qqnorm(resid(movar_rate))

movar_rate_null<-lmer(MeanDist~ (1|Group), data = trates_cdgp, REML = F)
summary(movar_rate_null)
qqnorm(resid(movar_rate_null))

anova(movar_rate, movar_rate_null)
anova(movar_rate)
