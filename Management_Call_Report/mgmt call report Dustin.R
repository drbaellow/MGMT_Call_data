
rm(list=ls())

library(readxl)
library(tidyverse)
library(openxlsx)
library(zipcode)

SalesforceDate <- "3.26.19"


AllCalls= as.data.frame((read.csv(paste0("G:/My Drive/Clients/Red Hill/Analytics/Monthly.Weekly/MGMT Call Report/RawData/All Calls ", SalesforceDate, ".csv"))))
AllCalls$Zip <- clean.zipcodes(AllCalls$Zip)
colnames(AllCalls)[1] <- "Rep"

SampleCalls= as.data.frame((read.csv(paste0("G:/My Drive/Clients/Red Hill/Analytics/Monthly.Weekly/MGMT Call Report/RawData/Sample Calls ", SalesforceDate, ".csv"))))
SampleCalls$Zip <- clean.zipcodes(SampleCalls$Zip)
colnames(SampleCalls)[1] <- "Rep"

WeekNums= as.data.frame((read_excel(paste0("G:/My Drive/Clients/Red Hill/Analytics/Monthly.Weekly/MGMT Call Report/RawData/weeknums.xlsx"))))
ProdName.Prod= as.data.frame((read_excel(paste0("G:/My Drive/Clients/Red Hill/Analytics/Monthly.Weekly/MGMT Call Report/RawData/ProdName.Prod.xlsx"))))

RepRoster= as.data.frame((read_excel(paste0("G:/My Drive/Clients/Red Hill/Company/Rep Roster.xlsx"))))

WeekNums$Date  <- as.Date(WeekNums$Date, format = "%m/%d/%Y")
AllCalls$Date  <- as.Date(AllCalls$Date , format = "%m/%d/%Y")
AllCalls <- subset (AllCalls, AllCalls$Call..ID !="")
SampleCalls$Date  <- as.Date(SampleCalls$Date , format = "%m/%d/%Y")
SampleCalls <- subset (SampleCalls, SampleCalls$Call.Name!="")

AllCalls <- left_join(AllCalls,WeekNums, by="Date" )
SampleCalls <- left_join(SampleCalls,WeekNums, by="Date" )

ThisWeek <- subset(AllCalls, `Total Week` == max(AllCalls$`Total Week`))
ThisWeek <- head(ThisWeek,1)
ThisWeek <- ThisWeek[c(15:20)]
rownames(ThisWeek) <- NULL


ThisYear <- as.numeric(ThisWeek$Year[1])
ThisYearWeek <- as.numeric(ThisWeek$YearWeek[1])
ThisQuarter <- as.numeric(ThisWeek$Quarter[1])
ThisQuarterWeek <- as.numeric(ThisWeek$QuarterWeek[1])
ThisTotalWeek <- as.numeric(ThisWeek$`Total Week`[1])
ThisTotalQuarter <- as.numeric(ThisWeek$TotalQuarter[1])

SampleCalls<- SampleCalls %>% mutate_if(is.factor, as.character) 
AllCalls %>% mutate_if(is.factor, as.character) -> AllCalls

AllCalls <- left_join(AllCalls,RepRoster, by="Rep" )
AllCalls <- replace(AllCalls, is.na(AllCalls), 0) 
AllCalls <- subset(AllCalls, Region != 0)

SampleCalls <- left_join(SampleCalls,RepRoster, by="Rep" )
SampleCalls <- replace(SampleCalls, is.na(SampleCalls), 0) 
SampleCalls <- subset(SampleCalls, Region != 0)

AllCalls<- AllCalls[c(1, 7, 15:22)]
SampleCalls<- SampleCalls[c(1, 6, 16:23)]
AllCalls$DonnatalTabs <- ifelse(as.character(grepl("DONNATAL TABLETS", AllCalls$Detailed.Products)) == "TRUE", 1, 0)
AllCalls$DonnatalElixir <- ifelse(as.character(grepl("DONNATAL ELIXIR", AllCalls$Detailed.Products)) == "TRUE", 1, 0)
AllCalls$Enteragam <- ifelse(as.character(grepl("ENTERAGAM", AllCalls$Detailed.Products)) == "TRUE", 1, 0)
AllCalls$Esomeprazole <- ifelse(as.character(grepl("ESOMEPRAZOLE STRONTIUM", AllCalls$Detailed.Products)) == "TRUE", 1, 0)
AllCalls$Mytesi <- ifelse(as.character(grepl("MYTESI", AllCalls$Detailed.Products)) == "TRUE", 1, 0)

AllCallsMaster <- AllCalls[c(1:11)]
AllCallsMaster$Prod <- colnames(AllCallsMaster)[11]
colnames(AllCallsMaster)[11] <- "CallCount"

df <- AllCalls[c(1:10, 12)]
df$Prod <- colnames(df)[11]
colnames(df)[11] <- "CallCount"
AllCallsMaster <- rbind(AllCallsMaster, df)

df <- AllCalls[c(1:10, 13)]
df$Prod <- colnames(df)[11]
colnames(df)[11] <- "CallCount"
AllCallsMaster <- rbind(AllCallsMaster, df)

df <- AllCalls[c(1:10, 14)]
df$Prod <- colnames(df)[11]
colnames(df)[11] <- "CallCount"
AllCallsMaster <- rbind(AllCallsMaster, df)

df <- AllCalls[c(1:10, 15)]
df$Prod <- colnames(df)[11]
colnames(df)[11] <- "CallCount"
AllCallsMaster <- rbind(AllCallsMaster, df)

colnames(ProdName.Prod)[1] <- "Product..Product.Name"
SampleCalls <- left_join(SampleCalls,ProdName.Prod, by= "Product..Product.Name" )
SampleCalls$CallCount <- 1


AllCallsMaster$ThisWeekVLastWeek <- ifelse(AllCallsMaster$`Total Week` == ThisTotalWeek, "ThisWeek", 
                                           ifelse(AllCallsMaster$`Total Week` == (ThisTotalWeek-1), "LastWeek", 0))

AllCallsMaster$ThisFourWeekVLastFourWeek <- ifelse(AllCallsMaster$`Total Week` >= ThisTotalWeek-3, "This4Week", 
                                                   ifelse((AllCallsMaster$`Total Week` >= (ThisTotalWeek-7))&(AllCallsMaster$`Total Week` <= (ThisTotalWeek-4)), "Last4Week", 0))

AllCallsMaster$ThisThirteenWeekVLastThirteenWeek <- ifelse(AllCallsMaster$`Total Week` >= ThisTotalWeek-12, "This13Week", 
                                                           ifelse((AllCallsMaster$`Total Week` >= (ThisTotalWeek-25))&(AllCallsMaster$`Total Week` <= (ThisTotalWeek-13)), "Last13Week", 0))
AllCallsMaster$ThisQVLastQ<- ifelse((AllCallsMaster$TotalQuarter == ThisTotalQuarter), "ThisQTD", 
                                    ifelse((AllCallsMaster$TotalQuarter == ThisTotalQuarter-1)&(AllCallsMaster$Quarter<= ThisQuarterWeek), "LastQTD", 0))





SampleCalls$ThisWeekVLastWeek <- ifelse(SampleCalls$`Total Week` == ThisTotalWeek, "ThisWeek", 
                                        ifelse(SampleCalls$`Total Week` == (ThisTotalWeek-1), "LastWeek", 0))

SampleCalls$ThisFourWeekVLastFourWeek <- ifelse(SampleCalls$`Total Week` >= ThisTotalWeek-3, "This4Week", 
                                                ifelse((SampleCalls$`Total Week` >= (ThisTotalWeek-7))&(SampleCalls$`Total Week` <= (ThisTotalWeek-4)), "Last4Week", 0))

SampleCalls$ThisThirteenWeekVLastThirteenWeek <- ifelse(SampleCalls$`Total Week` >= ThisTotalWeek-12, "This13Week", 
                                                        ifelse((SampleCalls$`Total Week` >= (ThisTotalWeek-25))&(SampleCalls$`Total Week` <= (ThisTotalWeek-13)), "Last13Week", 0))
SampleCalls$ThisQVLastQ<- ifelse((SampleCalls$TotalQuarter == ThisTotalQuarter), "ThisQTD", 
                                 ifelse((SampleCalls$TotalQuarter == ThisTotalQuarter-1)&(SampleCalls$Quarter<= ThisQuarterWeek), "LastQTD", 0))


AllCallsThisWVLastW <- AllCallsMaster %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisWeekVLastWeek) %>%
  summarise(CallCount = sum(CallCount))
AllCallsThisWVLastW <- subset(AllCallsThisWVLastW, ThisWeekVLastWeek!=0)

AllCallsThis4VLast4 <- AllCallsMaster %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisFourWeekVLastFourWeek) %>%
  summarise(CallCount = sum(CallCount))
AllCallsThis4VLast4 <- subset(AllCallsThis4VLast4, ThisFourWeekVLastFourWeek!=0)

AllCallsThis13VLast13<- AllCallsMaster %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisThirteenWeekVLastThirteenWeek) %>%
  summarise(CallCount = sum(CallCount))
AllCallsThis13VLast13 <- subset(AllCallsThis13VLast13, ThisThirteenWeekVLastThirteenWeek!=0)

AllCallsThisQTDVLastQTD <- AllCallsMaster %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisQVLastQ) %>%
  summarise(CallCount = sum(CallCount))
AllCallsThisQTDVLastQTD <- subset(AllCallsThisQTDVLastQTD, ThisQVLastQ!=0)

SampleCallsThisWVLastW <- SampleCalls %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisWeekVLastWeek) %>%
  summarise(CallCount = sum(CallCount))
SampleCallsThisWVLastW <- subset(SampleCallsThisWVLastW, ThisWeekVLastWeek!=0)

SampleCallsThis4VLast4 <- SampleCalls %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisFourWeekVLastFourWeek) %>%
  summarise(CallCount = sum(CallCount))
SampleCallsThis4VLast4 <- subset(SampleCallsThis4VLast4, ThisFourWeekVLastFourWeek!=0)

SampleCallsThis13VLast13 <- SampleCalls %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisThirteenWeekVLastThirteenWeek) %>%
  summarise(CallCount = sum(CallCount))
SampleCallsThis13VLast13 <- subset(SampleCallsThis13VLast13, ThisThirteenWeekVLastThirteenWeek!=0)

SampleCallsThisQTDVLastQTD <- SampleCalls %>%
  group_by(Rep, `Territory Name`, Region, Prod, ThisQVLastQ) %>%
  summarise(CallCount = sum(CallCount))
SampleCallsThisQTDVLastQTD <- subset(SampleCallsThisQTDVLastQTD, ThisQVLastQ!=0)


AllCallsThisWVLastW$Type <- "All"
SampleCallsThisWVLastW$Type <- "Sample"
ThisWvLastW <- rbind(AllCallsThisWVLastW,SampleCallsThisWVLastW )
Allw.w <- ThisWvLastW %>%
  group_by( Prod, ThisWeekVLastWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

RegW.W <- ThisWvLastW %>%
  group_by( Region, Prod, ThisWeekVLastWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

TerrW.w <- ThisWvLastW %>%
  group_by( `Territory Name`, Prod, ThisWeekVLastWeek, Type) %>%
  summarise(CallCount = sum(CallCount))



AllCallsThis4VLast4$Type <- "All"
SampleCallsThis4VLast4$Type <- "Sample"
This4WvLast4W <- rbind(AllCallsThis4VLast4,SampleCallsThis4VLast4 )

All4w.4w <- This4WvLast4W %>%
  group_by( Prod, ThisFourWeekVLastFourWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

Reg4w.4w <- This4WvLast4W %>%
  group_by( Region, Prod, ThisFourWeekVLastFourWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

Terr4w.4w <- This4WvLast4W %>%
  group_by( `Territory Name`, Prod, ThisFourWeekVLastFourWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

AllCallsThis13VLast13$Type <- "All"
SampleCallsThis13VLast13$Type <- "Sample"
This13WvLast13W <- rbind(AllCallsThis13VLast13,SampleCallsThis13VLast13 )

All13W.13w <- This13WvLast13W %>%
  group_by( Prod, ThisThirteenWeekVLastThirteenWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

Reg13W.13w <- This13WvLast13W %>%
  group_by( Region, Prod, ThisThirteenWeekVLastThirteenWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

Terr13W.13w <- This13WvLast13W %>%
  group_by( `Territory Name`, Prod, ThisThirteenWeekVLastThirteenWeek, Type) %>%
  summarise(CallCount = sum(CallCount))

AllCallsThisQTDVLastQTD$Type <- "All"
SampleCallsThisQTDVLastQTD$Type <- "Sample"
ThisQTDvLastQTD <- rbind(AllCallsThisQTDVLastQTD,SampleCallsThisQTDVLastQTD )

AllQ.Q<- ThisQTDvLastQTD %>%
  group_by( Prod, ThisQVLastQ, Type) %>%
  summarise(CallCount = sum(CallCount))

RegQ.Q <- ThisQTDvLastQTD %>%
  group_by( Region, Prod, ThisQVLastQ, Type) %>%
  summarise(CallCount = sum(CallCount))

TerrQ.Q <- ThisQTDvLastQTD %>%
  group_by( `Territory Name`, Prod, ThisQVLastQ, Type) %>%
  summarise(CallCount = sum(CallCount))

# ####All Plots
# AllThisW.LastWPlot <- ggplot(Allw.w, aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# All4w.4wPlot <- ggplot(All4w.4w, aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# All13W.13wPlot <- ggplot(All13W.13w, aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# AllQ.QPlot <- ggplot(AllQ.Q, aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)  +labs(title="This QTD v Last QTD")   +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# 
# plot(AllQ.QPlot)
# 
# 
# 
# 
# ####Region Plots
# 
# RegThisW.LastWPlot <- ggplot(subset(RegW.W, Region == SelectedRegion), aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# Reg4w.4wPlot <- ggplot(subset(Reg4w.4w, Region == SelectedRegion), aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# Reg13W.13wPlot <- ggplot(subset(Reg13W.13w, Region == SelectedRegion), aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# RegQ.QPlot <- ggplot(subset(RegQ.Q, Region == SelectedRegion), aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This QTD v Last QTD") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# 
# plot(RegQ.QPlot)
# 
# ####Territory Plots
# SelectedTerr <- "Jacksonville"
# TerrThisW.LastWPlot <- ggplot(subset(TerrW.w, `Territory Name` == SelectedTerr), aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# Terr4w.4wPlot <- ggplot(subset(Terr4w.4w, `Territory Name`  == SelectedTerr), aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# Terr13W.13wPlot <- ggplot(subset(Terr13W.13w, `Territory Name`  == SelectedTerr), aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# TerrQ.QPlot <- ggplot(subset(TerrQ.Q, `Territory Name`  == SelectedTerr), aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) +
#   geom_bar( stat="identity", position=position_dodge()) +
#   facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
#                                               position = position_dodge(1), size=3.5)+labs(title="This QTD v Last QTD") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
# 
# 
# typeof(levels(as.factor(Reg4w.4w$Region)))


