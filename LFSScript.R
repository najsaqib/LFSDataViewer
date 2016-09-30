library(dplyr)
library(ggplot2)
library(CANSIM2R) # this package downloads CANSIM tables
library(Hmisc) # need this package to read labels
library(zoo) #to deal with dates that only have year and month, with no days
library(shiny)
library(DT)
library(car)
library(rmarkdown)
library(rCharts)
library(tidyr)
library(xts)

# Altered Rprofile.site file to work with Rscript.ext (which is used for Task scheduler)

MonthlyLFSx <- mergeCANSIM(c(2820087,2820089)) #downloading and merging these two LFS-related tables from CANSIM
metadataMonthlyx <- data.frame(colnames(MonthlyLFSx),label(MonthlyLFSx)) # create metadata data.frame to view all labels
MonthlyLFSx$Date <- as.Date(as.yearmon(MonthlyLFSx$t,format="%Y/%m")) # turning the time variable into an explicit date variable
MonthlyLFSx1 <- MonthlyLFSx %>% select(t,Date,i,V16,V922,V463,V148,V236,V138,V154,V164,V1057,V1072,V1087)


# MonthlyLFSx1$Var2 <- as.double(as.POSIXct(as.Date(MonthlyLFSx1$Date,"%Y-%m-%d")),origin="1976-01-01")


# Generate functions to calculate growths and attaching lagged values of certain variables

growthMoM <- function(x)round((x-lag(x, n=1L))*1000,digits=2)
growthYoY <- function(x)round((x-lag(x, n=12L))*1000, digits=2)
LastMonthUR <- function(x)(lag(x,n=1L))
LastYearUR <- function(x)(lag(x, n=12L))

# Calcute MoM and YoY growths of certain variables (some are non-sensical)

MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmp15Over = growthMoM(V16))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmp15Over = growthYoY(V16))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMUR15Over = growthMoM(V922))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYUR15Over = growthYoY(V922))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMLF15Over = growthMoM(V463))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYLF15Over = growthYoY(V463))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpFT15Over = growthMoM(V148))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpFT15Over = growthYoY(V148))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpPT15Over = growthMoM(V236))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpPT15Over = growthYoY(V236))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpFT1524 = growthMoM(V138))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpFT1524 = growthYoY(V138))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpFT2554 = growthMoM(V154))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpFT2554 = growthYoY(V154))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpFT54Over = growthMoM(V164))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpFT54Over = growthYoY(V164))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpPrivate = growthMoM(V1057))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpPrivate = growthYoY(V1057))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpPublic = growthMoM(V1072))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpPublic = growthYoY(V1072))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(MoMEmpSelf = growthMoM(V1087))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(YoYEmpSelf = growthYoY(V1087))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(URLast = LastMonthUR(V922))
MonthlyLFSx1 <- MonthlyLFSx1 %>% group_by(i) %>% mutate(URLastYear = LastYearUR(V922))

# Rename variables for better readability

MonthlyLFSx1 <- rename(MonthlyLFSx1,Employment=V16)
MonthlyLFSx1 <- rename(MonthlyLFSx1,UnRate=V922)

# Create new table for the line graph, which retains a working date format

MonthlyLFSx2 <- MonthlyLFSx1
MonthlyLFSx2$Date2 <- as.Date(as.yearmon(MonthlyLFSx2$t, format = "%Y/%m"))

# Rounding figures and suppressing scientific notation

MonthlyLFSx1 <- format(MonthlyLFSx1, big.mark=",", scientific=FALSE, TRIM=TRUE)

# MonthlyLFSx1$Var2 <- as.double(as.POSIXct(as.Date(MonthlyLFSx$Date,"%Y-%m-%d")),origin="1976-01-01")


# Seperate into more columns, for the time series graph

wide_UnRate <- MonthlyLFSx2 %>% select(Date,i,UnRate) %>% spread(i,UnRate)

# Save relevant data frames as RDS files

saveRDS(wide_UnRate, "C:/RProjects/LFS/LFSApp/Data/wideUnRate.rds")
saveRDS(MonthlyLFSx1, "C:/RProjects/LFS/LFSApp/Data/MonthlyLFSx1.rds")
saveRDS(MonthlyLFSx2, "C:/RProjects/LFS/LFSApp/Data/MonthlyLFSx2.rds")


# Run the Shiny app

# runApp('LFSApp')

# Deploy the app on Shiny IO

library(shiny)
#setwd("C:/Users/Najmus/Documents/R/LFSDataViewer")
setwd("C:/RProjects/LFS")
rsconnect::deployApp('LFSApp')