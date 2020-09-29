# Use the DID model to estimate ATE of Unsorted Waste, Biodegradable Waste, Recycling, and Illegal Dumping
# Study duration: Jan/2001 to Dec/2016 for Unsorted waste
# Study duration: Jan/2003 to Dec/2014 for Biodegradable Waste
# Study duration: Jan/2000 to Dec/2016 for Recycling (Except outlier June 2000)
# Study duration: Jan/1998 to Dec/2015 for Illegal dumping

# Load packages
library(ggplot2)
library(stargazer)
library(AER)
library(lmtest)
library(multiwayvcov)
library(dplyr)
library(zoo)  # as.Date
library(gridExtra)  # grid.arrange

# Clean all objects in a Specified Environment
rm(list = ls())

# Input data ------------------------------------------
DF_Unsorted <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Unsorted.csv"))
DF_Biowaste <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Biowaste.csv"))
DF_Recycling <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Recycling.csv"))
DF_ID <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Illegal_dumping.csv"))

# Create UBP dummy
DF_Unsorted$UBP <- ifelse(DF_Unsorted$City_code==2 | 
                    (DF_Unsorted$City_code==1 & DF_Unsorted$TimeIndicator>=DF_Unsorted[DF_Unsorted$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_Biowaste$UBP <- ifelse(DF_Biowaste$City_code==2 | 
                    (DF_Biowaste$City_code==1 & DF_Biowaste$TimeIndicator>=DF_Biowaste[DF_Biowaste$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_Recycling$UBP <- ifelse(DF_Recycling$City_code==2 & DF_Recycling$TimeIndicator>=DF_Recycling[DF_Recycling$Time=="2000-07-01","TimeIndicator"][1] | 
                   (DF_Recycling$City_code==1 & DF_Recycling$TimeIndicator>=DF_Recycling[DF_Recycling$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_ID$UBP <- ifelse(DF_ID$City_code==2 & DF_ID$TimeIndicator>=DF_ID[DF_ID$Time=="2000-07-01","TimeIndicator"][1] | 
                    (DF_ID$City_code==1 & DF_ID$TimeIndicator>=DF_ID[DF_ID$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)

# 0. Summary statistics ------------------------------------------ 
# 0.1 Select variables of interests and insert NAs to make different variables have the same length ------------------------------------------ 
# Unsorted waste (From 2001Jan to 2016Dec)
na.UW <- matrix(data = NA, nrow=(nrow(DF_Recycling)-nrow(DF_Unsorted)), ncol=1)
DF.UW <- append(na.UW, DF_Unsorted[,"Unsorted_per_ca"])

# Biodegradable waste (From 2003Jan to 2014Dec)
na.BW <- matrix(data = NA, nrow=(nrow(DF_Recycling)-nrow(DF_Biowaste)), ncol=1)
DF.BW <- append(na.BW, DF_Biowaste[,"Biowaste_per_ca"])

# Recycling (From 2000Jan to 2016Dec)
Item_list <- c("All_Tot_pca", "Income_thous", "Baby", "Household_Size")
DF.RW <- DF_Recycling[,Item_list]

# Illegal dumping (From 1998Jan to 2015Dec)
na.ID <- matrix(data = NA, nrow=(nrow(DF_Recycling)-nrow(DF_ID)), ncol=1)
DF.ID <- append(na.ID, DF_ID[,"Violation_per_ca"])

# 0.2 Combine all data sets -------------------------------------------------------------------------------
DS.DF <- cbind.data.frame(DF.UW, DF.BW, DF.RW, DF.ID)

# Rearrange the column orders
DS.DF <- DS.DF[,c("DF.UW","DF.BW","All_Tot_pca", "DF.ID", "Income_thous","Baby", "Household_Size" )]

# 0.3 Calculate descriptive summary -------------------------------------------------------------------------------
stargazer(DS.DF,  header = F, digits=2, type = "text", out = "summary statistics.txt",
          covariate.labels=c("Unsorted waste (kg per capita)",  
                             "Biodegradable waste (kg per capita)", "Recycling (kg per capita)",
                             "Illegal dumping incident (count/1,000 ppl)",
                             "Income (NTD 1,000)", "Percentage of babies","Household size"),
          omit.summary.stat = c("p25","p75"),
          title="Summary statistics")

# 1. DID model -------------------------------------------------------------------------------
# 1.1 Unsorted Garbage (2001Jan - 2016Dec) -------------------------------------------------------------------------------
# 1.1.1 [Full FE] Include TPC -------------------------------------------------------------------------------
G1 <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
         + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted)

# Cluster by cities
G1_vcov_city <- cluster.vcov(G1, DF_Unsorted$City_code)
G1_Cluster <- coeftest(G1, G1_vcov_city)

# Calculate the change rate in unsorted garbage (UBP)
NTC <- subset.data.frame(DF_Unsorted, DF_Unsorted$City_code==1)
NTC_G_baseline <- aggregate(Unsorted_per_ca ~ Year, NTC, mean)[7,2]
NTC_G_ATE <- as.numeric(G1$coefficients["TE.NTC"])
NTC_G_change <- round(NTC_G_ATE/NTC_G_baseline, 2)
NTC_G_change

# Calculate the change rate in unsorted garbage (MRSP)
NTC_G_MWR_change <- round(as.numeric(G1$coefficients["MRSP"])/aggregate(Unsorted_per_ca ~ Year, NTC, mean)[5,2], 2)
NTC_G_MWR_change

# Calculate the change rate in unsorted garbage (UBP * MRSP)
NTC_G_Both_change <- round(as.numeric(G1$coefficients["I(UBP * MRSP)"])/aggregate(Unsorted_per_ca ~ Year, NTC, mean)[7,2], 2)
NTC_G_Both_change

# 1.1.2 [Full FE] Exclude TPC -------------------------------------------------------------------------------
DF_Unsorted_G2 <- subset(DF_Unsorted, DF_Unsorted$City_code!=2)
G2 <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
         + TT_NTC + FE_NTC + TE.NTC + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
         + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted_G2)

# Cluster by cities
G2_vcov_city <- cluster.vcov(G2, DF_Unsorted_G2$City_code)
G2_Cluster <- coeftest(G2, G2_vcov_city)

# 1.1.3 [Full FE] Exclude TCC -------------------------------------------------------------------------------
DF_Unsorted_G3 <- subset(DF_Unsorted, DF_Unsorted$City_code!=4)
G3 <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
         + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted_G3)

# Cluster by cities
G3_vcov_city <- cluster.vcov(G3, DF_Unsorted_G3$City_code)
G3_Cluster <- coeftest(G3, G3_vcov_city)

# 1.1.4 [Full FE] Exclude TPC & TCC -------------------------------------------------------------------------------
DF_Unsorted_G4 <- subset(DF_Unsorted, DF_Unsorted$City_code!=2 & DF_Unsorted$City_code!=4)
G4 <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
         + TT_NTC + FE_NTC + TE.NTC + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
         + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted_G4)

# Cluster by cities
G4_vcov_city <- cluster.vcov(G4, DF_Unsorted_G4$City_code)
G4_Cluster <- coeftest(G4, G4_vcov_city)

# 1.1.5 [Full FE] Limit study period within 2006Jan-2016Dec -------------------------------------------------------------------------------
DF_Unsorted_G5 <- subset(DF_Unsorted, DF_Unsorted$TimeIndicator>=DF_Unsorted[DF_Unsorted$Time=="2006-01-01","TimeIndicator"][1])
G5 <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + TimeIndicator 
         + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted_G5)

# Cluster by cities
G5_vcov_city <- cluster.vcov(G5, DF_Unsorted_G5$City_code)
G5_Cluster <- coeftest(G5, G5_vcov_city)

# 1.1.6 Summary -------------------------------------------------------------------------------
stargazer(G1_Cluster, G2_Cluster, G3_Cluster, G4_Cluster, G5_Cluster, 
          header = F, digits=2, type="text",
          out = "DID_Unsored.txt",
          dep.var.labels=c("","","","",""),
          column.labels=c("All data", "Exclude TPC", "Exclude TCC", "Exclude TPC+TCC", "Post Jan-2006"),
          omit=c("FE_TPC", "FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11", "y2000",
                 "y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010",
                 "y2011","y2012","y2013","y2014","y2015"),
          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)",
                             "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
                             "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          add.lines = list(c("Duration", "Jan 2001 - Dec 2016", "Jan 2001 - Dec 2016", 
                             "Jan 2001 - Dec 2016", "Jan 2001 - Dec 2016", "Jan 2006 - Dec 2016"),
                           c("Num. of municipalities", length(unique(DF_Unsorted$City)), length(unique(DF_Unsorted_G2$City)), 
                             length(unique(DF_Unsorted_G3$City)), length(unique(DF_Unsorted_G4$City)), length(unique(DF_Unsorted_G5$City))),
                           c("Observations", nrow(DF_Unsorted), nrow(DF_Unsorted_G2), nrow(DF_Unsorted_G3), nrow(DF_Unsorted_G4), nrow(DF_Unsorted_G5)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.append = TRUE, notes.align = "l", omit.table.layout=c("l"), keep.stat="n",
          title= "Result of Average Treatment Effects on Unsorted Waste", no.space=TRUE)

# 1.2 Biodegradable Waste (2003Jan - 2014Dec) -----------------------------------------------------------------------------
# 1.2.1 [Full FE] Include TPC -------------------------------------------------------------------------------
BW1 <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
         + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste)

# Cluster by cities
BW1_vcov_city <- cluster.vcov(BW1, DF_Biowaste$City_code)
BW1_Cluster <- coeftest(BW1, BW1_vcov_city)

# Calculate the change rate in biowaste (UBP)
NTC <- subset.data.frame(DF_Biowaste, DF_Biowaste$City_code==1)
NTC_BW_baseline <- aggregate(Biowaste_per_ca ~ Year, NTC, mean)[5,2]
NTC_BW_ATE <- as.numeric(BW1$coefficients["TE.NTC"])
NTC_BW_change <- round(NTC_BW_ATE/NTC_BW_baseline, 2)
NTC_BW_change

# Calculate the change rate in biowaste (MRSP)
NTC_BW_MWR_change <- round(as.numeric(BW1$coefficients["MRSP"])/aggregate(Biowaste_per_ca ~ Year, NTC, mean)[5,2], 2)
NTC_BW_MWR_change

# 1.2.2 [Full FE] Exclude TPC -------------------------------------------------------------------------------
DF_Biowaste_BW2 <- subset(DF_Biowaste, DF_Biowaste$City_code!=2)
BW2 <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
         + TT_NTC + FE_NTC + TE.NTC + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
         + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste_BW2)

# Cluster by cities
BW2_vcov_city <- cluster.vcov(BW2, DF_Biowaste_BW2$City_code)
BW2_Cluster <- coeftest(BW2, BW2_vcov_city)

# 1.2.3 [Full FE] Exclude TCC -------------------------------------------------------------------------------
DF_Biowaste_BW3 <- subset(DF_Biowaste, DF_Biowaste$City_code!=4)
BW3 <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
         + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste_BW3)

# Cluster by cities
BW3_vcov_city <- cluster.vcov(BW3, DF_Biowaste_BW3$City_code)
BW3_Cluster <- coeftest(BW3, BW3_vcov_city)

# 1.2.4 [Full FE] Exclude TPC & TCC -------------------------------------------------------------------------------
DF_Biowaste_BW4 <- subset(DF_Biowaste, DF_Biowaste$City_code!=2 & DF_Biowaste$City_code!=4)
BW4 <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
         + TT_NTC + FE_NTC + TE.NTC + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
         + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste_BW4)

# Cluster by cities
BW4_vcov_city <- cluster.vcov(BW4, DF_Biowaste_BW4$City_code)
BW4_Cluster <- coeftest(BW4, BW4_vcov_city)

# 1.2.5 [Full FE] Limit study period within 2006Jan-2016Dec -------------------------------------------------------------------------------
DF_Biowaste_BW5 <- subset(DF_Biowaste, DF_Biowaste$TimeIndicator>=DF_Biowaste[DF_Biowaste$Time=="2006-01-01","TimeIndicator"][1])
BW5 <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
         + FE_TPC + TT_NTC + FE_NTC + TE.NTC + TimeIndicator 
          + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
         + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste_BW5)

# Cluster by cities
BW5_vcov_city <- cluster.vcov(BW5, DF_Biowaste_BW5$City_code)
BW5_Cluster <- coeftest(BW5, BW5_vcov_city)

# 1.2.6 Summary -------------------------------------------------------------------------------
stargazer(BW1_Cluster, BW2_Cluster, BW3_Cluster, BW4_Cluster, BW5_Cluster, 
          header = F, digits=2, type = "text",
          out = "DID_Biodegradable.txt",
          dep.var.labels=c("","","","",""),
          column.labels=c("All data", "Exclude TPC", "Exclude TCC", "Exclude TPC+TCC", "Post Jan-2006"),
          omit=c("FE_TPC","FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11", "y2000",
                 "y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010",
                 "y2011","y2012","y2013","y2014","y2015"),
          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)",
                             "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
                             "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          add.lines = list(c("Duration", "Jan 2003 - Dec 2014", "Jan 2003 - Dec 2014", 
                             "Jan 2003 - Dec 2014", "Jan 2003 - Dec 2014", "Jan 2006 - Dec 2014"),
                           c("Num. of municipalities", length(unique(DF_Biowaste$City)), length(unique(DF_Biowaste_BW2$City)), 
                             length(unique(DF_Biowaste_BW3$City)), length(unique(DF_Biowaste_BW4$City)), length(unique(DF_Biowaste_BW5$City))),
                           c("Observations", nrow(DF_Biowaste), nrow(DF_Biowaste_BW2), 
                             nrow(DF_Biowaste_BW3), nrow(DF_Biowaste_BW4), nrow(DF_Biowaste_BW5)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.append = TRUE, notes.align = "l", omit.table.layout=c("l"), keep.stat="n",
          title= "Result of Average Treatment Effects on Biodegradable Waste", no.space=TRUE)


# 1.3 Quantity of recycling (2000Jan - 2016Dec) ---------------------------------------------------------------------
# Study duration: Jan/2000 to Dec/2016 for Quantity of Recycling (Without dropping 2000.06 observation)
# 1.3.1 [Full FE] Include TPC -------------------------------------------------------------------------------
RW1 <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
          + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(UBP*MRSP) + TimeIndicator 
          + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling)

# Cluster by cities
RW1_vcov_city <- cluster.vcov(RW1, DF_Recycling$City_code)
RW1_Cluster <- coeftest(RW1, RW1_vcov_city)

# Calculate the change rate in biowaste (UBP in TPC)
TPC <- subset.data.frame(DF_Recycling, DF_Recycling$City_code==2)
TPC_RW_baseline <- aggregate(Recycling_per_ca ~ Year, TPC, mean)[1,2]
TPC_RW_ATE <- as.numeric(RW1$coefficients["TE.TPC"])
TPC_RW_change <- round(TPC_RW_ATE/TPC_RW_baseline, 2)
TPC_RW_change

# Calculate the change rate in biowaste (MRSP in TPC)
TPC_RW_MWR_change <- round(as.numeric(RW1$coefficients["MRSP"])/aggregate(Recycling_per_ca ~ Year, TPC, mean)[5,2], 2)
TPC_RW_MWR_change

# Calculate the change rate in biowaste (UBP in NTC)
NTC <- subset.data.frame(DF_Recycling, DF_Recycling$City_code==1)
NTC_RW_baseline <- aggregate(Recycling_per_ca ~ Year, NTC, mean)[8,2]
NTC_RW_ATE <- as.numeric(RW1$coefficients["TE.NTC"])
NTC_RW_change <- round(NTC_RW_ATE/NTC_RW_baseline, 2)
NTC_RW_change

# Calculate the change rate in biowaste (MRSP in NTC)
NTC_RW_MWR_change <- round(as.numeric(RW1$coefficients["MRSP"])/aggregate(Recycling_per_ca ~ Year, NTC, mean)[5,2], 2)
NTC_RW_MWR_change

# 1.3.2 [Full FE] Exclude TPC -------------------------------------------------------------------------------
DF_Recycling_RW2 <- subset(DF_Recycling, DF_Recycling$City_code!=2)
RW2 <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
          + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
          + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling_RW2)

# Cluster by cities
RW2_vcov_city <- cluster.vcov(RW2, DF_Recycling_RW2$City_code)
RW2_Cluster <- coeftest(RW2, RW2_vcov_city)

# 1.3.3 [Full FE] Exclude TCC -------------------------------------------------------------------------------
DF_Recycling_RW3 <- subset(DF_Recycling, DF_Recycling$City_code!=4)
RW3 <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
          + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(UBP*MRSP) + TimeIndicator 
          + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling_RW3)

# Cluster by cities
RW3_vcov_city <- cluster.vcov(RW3, DF_Recycling_RW3$City_code)
RW3_Cluster <- coeftest(RW3, RW3_vcov_city)

# 1.3.4 [Full FE] Exclude TPC & TCC -------------------------------------------------------------------------------
DF_Recycling_RW4 <- subset(DF_Recycling, DF_Recycling$City_code!=2 & DF_Recycling$City_code!=4)
RW4 <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
          + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(TE.NTC*MRSP) + TimeIndicator 
          + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling_RW4)

# Cluster by cities
RW4_vcov_city <- cluster.vcov(RW4, DF_Recycling_RW4$City_code)
RW4_Cluster <- coeftest(RW4, RW4_vcov_city)

# 1.3.5 [Full FE] Limit study period within 2006Jan-2016Dec -------------------------------------------------------------------------------
DF_Recycling_RW5 <- subset(DF_Recycling, DF_Recycling$TimeIndicator>=DF_Recycling[DF_Recycling$Time=="2006-01-01","TimeIndicator"][1])
RW5 <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
          + FE_TPC + TT_NTC + FE_NTC + TE.NTC + TimeIndicator 
          + FE_TYC + FE_TNC + FE_KSC + FE_Yilan
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling_RW5)

# Cluster by cities
RW5_vcov_city <- cluster.vcov(RW5, DF_Recycling_RW5$City_code)
RW5_Cluster <- coeftest(RW5, RW5_vcov_city)

# 1.3.6 Summary -------------------------------------------------------------------------------
stargazer(RW1_Cluster, RW2_Cluster, RW3_Cluster, RW4_Cluster, RW5_Cluster, 
          header = F, digits=2, type="text",
          out = "DID_Recycling.txt",
          dep.var.labels=c("","","","",""),
          column.labels=c("All data", "Exclude TPC", "Exclude TCC", "Exclude TPC+TCC", "Post Jan-2006"),
          omit=c("FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11", "y2000",
                 "y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010",
                 "y2011","y2012","y2013","y2014","y2015"),
          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)",
                             "I(Post July-2000)", "I(TPC)", "I(Post July-2000) x I(TPC)", 
                             "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
                             "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          add.lines = list(c("Duration", "Jan 2000 - Dec 2016", "Jan 2000 - Dec 2016", 
                             "Jan 2000 - Dec 2016", "Jan 2000 - Dec 2016", "Jan 2006 - Dec 2016"),
                           c("Num. of municipalities", length(unique(DF_Recycling$City)), length(unique(DF_Recycling_RW2$City)), 
                             length(unique(DF_Recycling_RW3$City)), length(unique(DF_Recycling_RW4$City)), length(unique(DF_Recycling_RW5$City))),
                           c("Observations", nrow(DF_Recycling), nrow(DF_Recycling_RW2), nrow(DF_Recycling_RW3), 
                             nrow(DF_Recycling_RW4), nrow(DF_Recycling_RW5)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.append = TRUE, notes.align = "l", omit.table.layout=c("l"), keep.stat="n",
          title= "Result of Average Treatment Effects on Recycling", no.space=TRUE)

# 1.4 DID model: Illegal dumping violation (1998Jan - 2015Dec) ---------------------------------------------------------------------
# 1.4.1 [Full FE] Include TPC -------------------------------------------------------------------------------
ID1 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(UBP*MRSP)  + TimeIndicator
          + FE_TYC + FE_TCC + FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID)

# Cluster by cities
ID1_vcov_city <- cluster.vcov(ID1, DF_ID$City_code)
ID1_Cluster <- coeftest(ID1, ID1_vcov_city)

# Calculate the change rate in biowaste (UBP)
NTC <- subset.data.frame(DF_ID, DF_ID$City_code==1)
NTC_ID_baseline <- aggregate(Violation_per_ca ~ Year, NTC, mean)[7,2]
NTC_ID_ATE <- as.numeric(ID1$coefficients["TE.NTC"])
NTC_ID_change <- round(NTC_ID_ATE/NTC_ID_baseline, 2)
NTC_ID_change

# Calculate the change rate in biowaste (MRSP)
NTC_ID_MWR_change <- round(as.numeric(ID1$coefficients["MRSP"])/aggregate(Violation_per_ca ~ Year, NTC, mean)[5,2], 2)
NTC_ID_MWR_change

# 1.4.2 [Full FE] Exclude TPC -------------------------------------------------------------------------------
DF_ID2 <- subset(DF_ID, DF_ID$City_code!=2)
ID2 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_NTC + FE_NTC + TE.NTC + 
          + MRSP + I(UBP*MRSP)  + TimeIndicator
          + FE_TYC + FE_TCC + FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID2)

# Cluster by cities
ID2_vcov_city <- cluster.vcov(ID2, DF_ID2$City_code)
ID2_Cluster <- coeftest(ID2, ID2_vcov_city)

# 1.4.3 [Full FE] Exclude TCC -------------------------------------------------------------------------------
DF_ID3 <- subset(DF_ID, DF_ID$City_code!=4)
ID3 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(UBP*MRSP)  + TimeIndicator
          + FE_TYC + FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID3)

# Cluster by cities
ID3_vcov_city <- cluster.vcov(ID3, DF_ID3$City_code)
ID3_Cluster <- coeftest(ID3, ID3_vcov_city)

# 1.4.4 [Full FE] Exclude TPC & TCC -------------------------------------------------------------------------------
DF_ID4 <- subset(DF_ID, DF_ID$City_code!=2 & DF_ID$City_code!=4)
ID4 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + I(UBP*MRSP)  + TimeIndicator
          + FE_TYC + FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID4)

# Cluster by cities
ID4_vcov_city <- cluster.vcov(ID4, DF_ID4$City_code)
ID4_Cluster <- coeftest(ID4, ID4_vcov_city)

# 1.4.5 [Full FE] Limit study period within 2006Jan-2015Dec -------------------------------------------------------------------------------
DF_ID5 <- subset(DF_ID, DF_ID$TimeIndicator>=DF_ID[DF_ID$Time=="2006-01-01","TimeIndicator"][1])
ID5 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_NTC + FE_NTC + TE.NTC + FE_TPC + TimeIndicator
          + FE_TYC +  FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID5)

# Cluster by cities
ID5_vcov_city <- cluster.vcov(ID5, DF_ID5$City_code)
ID5_Cluster <- coeftest(ID5, ID5_vcov_city)

# 1.4.6 [Full FE] Without I(UBP*MRSP) term -------------------------------------------------------------------------------
ID6 <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
          + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
          + MRSP + TimeIndicator
          + FE_TYC + FE_TCC + FE_TNC
          + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID)

# Cluster by cities
ID6_vcov_city <- cluster.vcov(ID6, DF_ID$City_code)
ID6_Cluster <- coeftest(ID6, ID6_vcov_city)

# 1.4.7 Summary -------------------------------------------------------------------------------
stargazer(ID1_Cluster, ID2_Cluster, ID3_Cluster, ID4_Cluster, ID5_Cluster, ID6_Cluster,
          header = F, digits=2, type = "text",
          out = "DID_Illegal_dumping(YearlyHH).txt",
          dep.var.labels=c("","","","",""),
          column.labels=c("All data", "Exclude TPC", "Exclude TCC", "Exclude TPC+TCC", "Post Jan 2006", "w/o MRSP"),
          omit=c("FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11", "y2000",
                 "y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010",
                 "y2011","y2012","y2013","y2014","y2015"),
#          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)", 
 #                            "I(Post July-2000)", "I(TPC)", "I(Post July-2000) x I(TPC)",
  #                           "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
   #                          "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          add.lines = list(c("Duration", "Jan 1998 - Dec 2015", "Jan 1998 - Dec 2015", 
                             "Jan 1998 - Dec 2015", "Jan 1998 - Dec 2015", "Jan 2006 - Dec 2015", "Jan 1998 - Dec 2015"),
                           c("Num. of municipalities", length(unique(DF_ID$City)), length(unique(DF_ID2$City)), 
                             length(unique(DF_ID3$City)), length(unique(DF_ID4$City)), length(unique(DF_ID5$City)), length(unique(DF_ID$City))),
                           c("Observations", nrow(DF_ID), nrow(DF_ID2), nrow(DF_ID3), 
                             nrow(DF_ID4), nrow(DF_ID5), nrow(DF_ID)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes","Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes","Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.append = TRUE, notes.align = "l", omit.table.layout=c("l"), keep.stat="n",
          title= "Result of Average Treatment Effects on Illegal Dumping", no.space=TRUE)

# 1.5 Wrap up the DID result ------------------------------------------------------------------------------------------
# ATE on unsorted waste and biodegradable waste
stargazer(G1_Cluster, G3_Cluster, G5_Cluster, BW1_Cluster, BW3_Cluster, BW5_Cluster, 
          no.space=TRUE, header = F, digits=2, type = "text", out = "Table2-PartI.txt",
          column.labels=c("", "Unsorted", "", "","Biodegradable", ""),
          omit.table.layout=c("l"),
          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)",
                             "I(TPC)", "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
                             "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          omit=c("FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11"),
          add.lines = list(c("Duration", 
                             "Jan 2001 - Dec 2016", "Jan 2001 - Dec 2016","Jan 2006 - Dec 2016", 
                             "Jan 2003 - Dec 2014", "Jan 2003 - Dec 2014", "Jan 2006 - Dec 2014"),
                           c("Num. of municipalities", 
                             length(unique(DF_Unsorted$City)), length(unique(DF_Unsorted_G3$City)), 
                             length(unique(DF_Unsorted_G5$City)), 
                             length(unique(DF_Biowaste$City)), length(unique(DF_Biowaste_BW3$City)), 
                             length(unique(DF_Biowaste_BW5$City))),
                           c("Observations", 
                             nrow(DF_Unsorted), nrow(DF_Unsorted_G3), nrow(DF_Unsorted_G5), 
                             nrow(DF_Biowaste), nrow(DF_Biowaste_BW3), nrow(DF_Biowaste_BW5)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes", "Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes", "Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.align = "l")

# ATE on recycling and illegal dumping
stargazer(RW1_Cluster, RW3_Cluster, RW5_Cluster, ID1_Cluster, ID3_Cluster, ID5_Cluster,
          no.space=TRUE, header = F, digits=2, type = "text", out = "Table2-PartII.txt",
          column.labels=c("", "Recycling", "", "","Illegal dumping incident", ""),
          omit.table.layout=c("l"),
          covariate.labels=c("Percentage of Babies", "Household Size", "Income (NTD 1,000)",
                             "I(Post July-2000)", "I(TPC)", "I(Post July-2000) x I(TPC)",
                             "I(Post July-2008)", "I(NTC)", "I(Post July-2008) x I(NTC)", 
                             "MR", "I(UBP in NTC) x I(UBP in TPC) x MR","Time trend", "Constant"),
          omit=c("FE_TYC", "FE_TCC","FE_TNC", "FE_KSC","FE_Yilan",
                 "m1", "m2", "m3", "m4", "m5", "m6", "m6", "m7", "m8", "m9", "m10", "m11"),
          add.lines = list(c("Duration", 
                             "Jan 2000 - Dec 2016", "Jan 2000 - Dec 2016", "Jan 2006 - Dec 2016",
                             "Jan 1998 - Dec 2015", "Jan 1998 - Dec 2015", "Jan 2006 - Dec 2015"),
                           c("Num. of municipalities", 
                             length(unique(DF_Recycling$City)), length(unique(DF_Recycling_RW3$City)), length(unique(DF_Recycling_RW5$City)), 
                             length(unique(DF_ID$City)), length(unique(DF_ID3$City)),length(unique(DF_ID5$City))), 
                           c("Observations", 
                             nrow(DF_Recycling), nrow(DF_Recycling_RW3), nrow(DF_Recycling_RW5), nrow(DF_ID), nrow(DF_ID3), nrow(DF_ID5)),
                           c("City fixed effect", "Yes", "Yes" ,"Yes", "Yes" ,"Yes", "Yes"),
                           c("Monthly fixed effect", "Yes", "Yes","Yes", "Yes" ,"Yes", "Yes")),
          notes = c("The unit of Income is NTD thousands (US $1 = NT $30).", 
                    "All estimates control city, and monthly fixed effects. Standard errors are clustered by cities."),
          notes.align = "l")

# 2. DATE model -----------------------------------------------------------------------------------------------------
# Plot Errorbar: DATE -----------------------------------------------------------------------------------------------------
# Create a star filter
Star_filter <- function(DF_Errorbar){
  for(i in 1:nrow(DF_Errorbar)){
    if(DF_Errorbar$pvalue[i] <= 0.01){
      DF_Errorbar$significance[i] <- "***"
    }else if(DF_Errorbar$pvalue[i] <= 0.05 & DF_Errorbar$pvalue[i] > 0.01){
      DF_Errorbar$significance[i] <- "**"
    }else if(DF_Errorbar$pvalue[i] <= 0.1 & DF_Errorbar$pvalue[i] > 0.05){
      DF_Errorbar$significance[i] <- "*"
    }else{
      DF_Errorbar$significance[i] <- ""
    }
  }
  DF_Errorbar
}

# 2.1 Unsorted waste -----------------------------------------------------------------------------------------------------
# [FE] city-FE and full time-FE (monthly and yearly dummies) + Dynamic ATE
G_DATE <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
             + FE_TPC + FE_NTC + TimeIndicator 
             + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan + Treated_y2008
             + DATE_2008 + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012
             + DATE_2013 + DATE_2014 + DATE_2015
             + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11
             + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 
             + y2008 + y2009 + y2010 + y2011 + y2012 + y2013 + y2014, 
             data = DF_Unsorted)

# Cluster by cities
vcov_city_G_DATE <- cluster.vcov(G_DATE, DF_Unsorted$City_code)
Garbage_Cluster_DATE <- coeftest(G_DATE, vcov_city_G_DATE)

# Create dataframe
df.bar.UW <- data.frame(year = c("0", "1", "2", "3", "4", "5", "6", "7"),
                            mean = c(G_DATE$coefficients[14:21]),
                            se = c(Garbage_Cluster_DATE[14:21,2]),
                            pvalue = c(Garbage_Cluster_DATE[14:21,4]))
df.bar.UW$Upper <- df.bar.UW$mean + qnorm(0.975)*df.bar.UW$se
df.bar.UW$Lower <- df.bar.UW$mean - qnorm(0.975)*df.bar.UW$se
df.bar.UW$Group <- rep("Panel A: Unsorted", time=8)
df.bar.UW$location <- df.bar.UW$Upper + 0.3
df.bar.UW <- Star_filter(df.bar.UW)

# 2.2 Biodegradable waste -----------------------------------------------------------------------------------------------------
# [FE] city-FE and full time-FE (monthly and yearly dummies) + Dynamic ATE
BW_DATE <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
             + FE_TPC + FE_NTC 
             + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan + Treated_y2008
             + DATE_2008 + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012
             + DATE_2013 + DATE_2014
             + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11
             + y2003 + y2004 + y2005 + y2006 + y2007 
             + y2008 + y2009 + y2010 + y2011 + y2012 + y2013, 
             data = DF_Biowaste)

# Cluster by cities
vcov_city_BW_DATE <- cluster.vcov(BW_DATE, DF_Biowaste$City_code)
BW_Cluster_DATE <- coeftest(BW_DATE, vcov_city_BW_DATE)

# Create dataframe
df.bar.BW <- data.frame(year = c("0", "1", "2", "3", "4", "5", "6"),
                        mean = c(BW_DATE$coefficients[13:19]),
                        se = c(BW_Cluster_DATE[13:19,2]),
                        pvalue = c(BW_Cluster_DATE[13:19,4]))
df.bar.BW$Upper <- df.bar.BW$mean + qnorm(0.975)*df.bar.BW$se
df.bar.BW$Lower <- df.bar.BW$mean - qnorm(0.975)*df.bar.BW$se
df.bar.BW$Group <- rep("Panel B: Biodegradable", time=7)
df.bar.BW$location <- df.bar.BW$Upper + 0.3
df.bar.BW <- Star_filter(df.bar.BW)

# 2.3 Recyclable waste: -----------------------------------------------------------------------------------------------------
# UBP in NTC ---------------------------------------------------------------------
# [FE] city-FE and full time-FE (monthly and yearly dummies) + Dynamic ATE
R_DATE_NTC <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
             + TT_TPC + FE_TPC + TE.TPC + FE_NTC  
             + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
             + Treated_y2008 + DATE_2008
             + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012
             + DATE_2013 + DATE_2014 + DATE_2015 + DATE_2016
             + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11
             + y2000 + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 
             + y2007 + y2008 + y2009 + y2010 + y2011 + y2012 + y2013 
             + y2014 + y2015, data = DF_Recycling)

# Cluster by cities
vcov_city_R_DATE_NTC <- cluster.vcov(R_DATE_NTC, DF_Recycling$City_code)
R_DATE_NTC_Cluster <- coeftest(R_DATE_NTC, vcov_city_R_DATE_NTC)

# UBP in TPC ---------------------------------------------------------------------
# Add TPC DATE
DF_Recycling_TPC <- DF_Recycling
DF_Recycling_TPC$Time <- as.Date(as.yearmon(paste(DF_Recycling_TPC$Year, DF_Recycling_TPC$Month), "%Y %m"))
DF_Recycling_TPC$Treated_y2000 <- ifelse(DF_Recycling_TPC$Time >= "2000-07-01", 1, 0)
DF_Recycling_TPC$DATE_2000 <- DF_Recycling_TPC$Treated_y2000*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2001 <- DF_Recycling_TPC$y2001*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2002 <- DF_Recycling_TPC$y2002*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2003 <- DF_Recycling_TPC$y2003*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2004 <- DF_Recycling_TPC$y2004*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2005 <- DF_Recycling_TPC$y2005*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2006 <- DF_Recycling_TPC$y2006*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2007 <- DF_Recycling_TPC$y2007*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2008 <- DF_Recycling_TPC$y2008*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2009 <- DF_Recycling_TPC$y2009*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2010 <- DF_Recycling_TPC$y2010*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2011 <- DF_Recycling_TPC$y2011*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2012 <- DF_Recycling_TPC$y2012*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2013 <- DF_Recycling_TPC$y2013*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2014 <- DF_Recycling_TPC$y2014*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2015 <- DF_Recycling_TPC$y2015*DF_Recycling_TPC$FE_TPC
DF_Recycling_TPC$DATE_2016 <- DF_Recycling_TPC$y2016*DF_Recycling_TPC$FE_TPC

## covariate + Dynamic ATE
R_DATE_TPC <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
                       + FE_TPC + FE_NTC + FE_TYC + FE_TCC + FE_TNC 
                       + TT_NTC + TE.NTC + Treated_y2000 + DATE_2000
                       + DATE_2001 + DATE_2002 + DATE_2003 + DATE_2004
                       + DATE_2005 + DATE_2006 + DATE_2007 + DATE_2008
                       + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012
                       + DATE_2013 + DATE_2014 + DATE_2015 + DATE_2016
                       + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11
                       + y2000 + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 + y2008 + y2009 
                       + y2010 + y2011 + y2012 + y2013 + y2014 + y2015, data = DF_Recycling_TPC)

# Cluster by cities
vcov_city_R_DATE_TPC <- cluster.vcov(R_DATE_TPC, DF_Recycling_TPC$City_code)
R_DATE_TPC_Cluster <- coeftest(R_DATE_TPC, vcov_city_R_DATE_TPC)

# Create dataframe
## NTC
df.bar.RW.NTC <- data.frame(year = c("0", "1", "2", "3", "4", "5", "6", "7", "8"),
                        mean = c(R_DATE_NTC_Cluster[15:23,1]),
                        se = c(R_DATE_NTC_Cluster[15:23,2]),
                        pvalue = c(R_DATE_NTC_Cluster[15:23,4]))
df.bar.RW.NTC$Upper <- df.bar.RW.NTC$mean + qnorm(0.975)*df.bar.RW.NTC$se
df.bar.RW.NTC$Lower <- df.bar.RW.NTC$mean - qnorm(0.975)*df.bar.RW.NTC$se
df.bar.RW.NTC$Group <- rep("Panel C: Recyclable (NTC)", time=9)
df.bar.RW.NTC$location <- df.bar.RW.NTC$Upper + 0.3
df.bar.RW.NTC <- Star_filter(df.bar.RW.NTC)

## TPC
df.bar.RW.TPC <- data.frame(year = c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                                     "9", "10", "11", "12", "13", "14", "15", "16"),
                        mean = c(R_DATE_TPC_Cluster[13:29,1]),
                        se = c(R_DATE_TPC_Cluster[13:29,2]),
                        pvalue = c(R_DATE_TPC_Cluster[13:29,4]))
df.bar.RW.TPC$Upper <- df.bar.RW.TPC$mean + qnorm(0.975)*df.bar.RW.TPC$se
df.bar.RW.TPC$Lower <- df.bar.RW.TPC$mean - qnorm(0.975)*df.bar.RW.TPC$se
df.bar.RW.TPC$Group <- rep("Panel D: Recyclable (TPC)", time=17)
df.bar.RW.TPC$location <- df.bar.RW.TPC$Upper + 0.3
df.bar.RW.TPC <- Star_filter(df.bar.RW.TPC)

# 2.4 Illegal dumping: Ticket ---------------------------------------------------------------------
# Create dynamic dummies: TPC
DF_ID_TPC <- subset.data.frame(DF_ID, DF_ID$City_code!=1)
DF_ID_TPC$Treated_y2000 <- ifelse(DF_ID_TPC$Time == "2000-07-01" | DF_ID_TPC$Time == "2000-08-01" |
                                  DF_ID_TPC$Time == "2000-09-01" | DF_ID_TPC$Time == "2000-10-01" |
                                  DF_ID_TPC$Time == "2000-11-01" | DF_ID_TPC$Time == "2000-12-01", 1, 0)
DF_ID_TPC$DATE_2000 <- DF_ID_TPC$Treated_y2000*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2001 <- DF_ID_TPC$y2001*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2002 <- DF_ID_TPC$y2002*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2003 <- DF_ID_TPC$y2003*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2004 <- DF_ID_TPC$y2004*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2005 <- DF_ID_TPC$y2005*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2006 <- DF_ID_TPC$y2006*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2007 <- DF_ID_TPC$y2007*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2008 <- DF_ID_TPC$y2008*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2009 <- DF_ID_TPC$y2009*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2010 <- DF_ID_TPC$y2010*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2011 <- DF_ID_TPC$y2011*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2012 <- DF_ID_TPC$y2012*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2013 <- DF_ID_TPC$y2013*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2014 <- DF_ID_TPC$y2014*DF_ID_TPC$FE_TPC
DF_ID_TPC$DATE_2015 <- DF_ID_TPC$y2015*DF_ID_TPC$FE_TPC

# Create dynamic dummies: NTC
DF_ID_NTC <- subset.data.frame(DF_ID, DF_ID$City_code!=2)
DF_ID_NTC$Treated_y2008 <- ifelse(DF_ID_NTC$Time == "2008-07-01" | DF_ID_NTC$Time == "2008-08-01" |
                                  DF_ID_NTC$Time == "2008-09-01" | DF_ID_NTC$Time == "2008-10-01" |
                                  DF_ID_NTC$Time == "2008-11-01" | DF_ID_NTC$Time == "2008-12-01", 1, 0)
DF_ID_NTC$DATE_2008 <- DF_ID_NTC$Treated_y2008*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2009 <- DF_ID_NTC$y2009*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2010 <- DF_ID_NTC$y2010*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2011 <- DF_ID_NTC$y2011*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2012 <- DF_ID_NTC$y2012*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2013 <- DF_ID_NTC$y2013*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2014 <- DF_ID_NTC$y2014*DF_ID_NTC$FE_NTC
DF_ID_NTC$DATE_2015 <- DF_ID_NTC$y2015*DF_ID_NTC$FE_NTC

## Dynamic ATE: NTC
ID_DATE_NTC <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
                   + Treated_y2008 + FE_NTC + TimeIndicator
                   + DATE_2008 + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012 
                   + DATE_2013 + DATE_2014 + DATE_2015 
                   + FE_TYC + FE_TCC + FE_TNC
                   + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + y1999 + y2000
                   + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 + y2008 + y2009
                   + y2010 + y2011 + y2012 + y2013 + y2014 + y2015, 
                   data = DF_ID_NTC)

# Cluster by cities
vcov_NTC <- cluster.vcov(ID_DATE_NTC, DF_ID_NTC$City)
Cluster_NTC <- coeftest(ID_DATE_NTC, vcov_NTC)

## Dynamic ATE: TPC
ID_DATE_TPC <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
                       + Treated_y2000 + FE_TPC + TimeIndicator + DATE_2000
                       + DATE_2001 + DATE_2002 + DATE_2003 + DATE_2004
                       + DATE_2005 + DATE_2006 + DATE_2007 + DATE_2008
                       + DATE_2009 + DATE_2010 + DATE_2011 + DATE_2012
                       + DATE_2013 + DATE_2014 + DATE_2015 
                       + FE_TYC + FE_TCC + FE_TNC
                       + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11
                       + y1999 + y2000
                       + y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 + y2008 + y2009 + y2010 + y2011
                       + y2012 + y2013 + y2014 + y2015, data = DF_ID_TPC)

# Cluster by cities
vcov_TPC <- cluster.vcov(ID_DATE_TPC, DF_ID_TPC$City)
Cluster_TPC <- coeftest(ID_DATE_TPC, vcov_TPC)

# Plot Errorbar -----------------------------------------------------------------------------------------------------
## NTC
df.bar.NTC <- data.frame(year = c("1", "2", "3", "4", "5", "6", "7", "8"),
                         mean = c(Cluster_NTC[sprintf("DATE_%d",2008:2015), "Estimate"]),
                         se = c(Cluster_NTC[sprintf("DATE_%d",2008:2015), "Std. Error"]),
                         pvalue = c(Cluster_NTC[sprintf("DATE_%d",2008:2015),"Pr(>|t|)"]))
df.bar.NTC$Upper <- df.bar.NTC$mean + qnorm(0.975)*df.bar.NTC$se
df.bar.NTC$Lower <- df.bar.NTC$mean - qnorm(0.975)*df.bar.NTC$se
df.bar.NTC$Group <- rep("Panel E: Illegal dumping (NTC)", time=8)

for(i in 1:nrow(df.bar.NTC)){
  if(df.bar.NTC$Upper[i]>=0){
    df.bar.NTC$location[i] <- df.bar.NTC$Upper[i]*1.05
  }else{
    df.bar.NTC$location[i] <- df.bar.NTC$Upper[i]*0.95
  }
}

df.bar.NTC <- Star_filter(df.bar.NTC)

## TPC
df.bar.TPC <- data.frame(year = c("1", "2", "3", "4", "5", "6", "7", "8",
                                  "9", "10", "11", "12", "13", "14", "15", "16"),
                         mean = c(Cluster_TPC[sprintf("DATE_%d",2000:2015), "Estimate"]),
                         se = c(Cluster_TPC[sprintf("DATE_%d",2000:2015), "Std. Error"]),
                         pvalue = c(Cluster_TPC[sprintf("DATE_%d",2000:2015), "Pr(>|t|)"]))
df.bar.TPC$Upper <- df.bar.TPC$mean + qnorm(0.975)*df.bar.TPC$se
df.bar.TPC$Lower <- df.bar.TPC$mean - qnorm(0.975)*df.bar.TPC$se
df.bar.TPC$Group <- rep("Panel F: Illegal dumping (TPC)", time=16)
for(i in 1:nrow(df.bar.TPC)){
  if(df.bar.TPC$Upper[i]>=0){
    df.bar.TPC$location[i] <- df.bar.TPC$Upper[i]*1.05
  }else{
    df.bar.TPC$location[i] <- df.bar.TPC$Upper[i]*0.95
  }
}

df.bar.TPC <- Star_filter(df.bar.TPC)

## Combine dataset
df.bar <- rbind.data.frame(df.bar.NTC, df.bar.TPC)
df.bar$location <- c(df.bar$Upper[1:8]*1.2, df.bar$Upper[9:24]*1.2)

# Combine all dataset
df.bar.Waste <- rbind.data.frame(df.bar.UW, df.bar.BW, df.bar.RW.NTC, df.bar.RW.TPC)
df.bar.Waste$year <- as.numeric(as.character(df.bar.Waste$year))
df.bar.IDT <- rbind.data.frame(df.bar.NTC, df.bar.TPC)
df.bar.IDT$year <- as.numeric(as.character(df.bar.IDT$year))

# 3. Visualize results -----------------------------------------------------------------------------------------------------
## Waste plot
theme_set(theme_bw())
Errorbar_plot_waste <- ggplot(df.bar.Waste, aes(x=year, y=mean), color="black") + 
  geom_line(aes(x=year, y=mean, group=Group), size=1) +
  geom_ribbon(aes(x=year, ymin=Lower, ymax=Upper, group=Group), fill="grey70", alpha=0.5) +
  geom_hline(yintercept=0,linetype=c("dashed"), size=1, color="gray60") + 
  facet_wrap(~Group, scales ="free",  dir="h", ncol=2) + 
  labs(y = "Dynamic ATE\n(kg/per capita)", size="") + 
  theme(legend.position = 'bottom', text = element_text(size=24), axis.title.x=element_blank()) 
Errorbar_plot_waste

## Illegal dumping ticket plot
theme_set(theme_bw())
Errorbar_plot_IDT <- ggplot(df.bar.IDT, aes(x=year, y=mean), color="black") + 
  geom_line(aes(x=year, y=mean, group=Group), size=1) +
  geom_ribbon(aes(x=year, ymin=Lower, ymax=Upper, group=Group), fill="grey70", alpha=0.5) +
  geom_hline(yintercept=0,linetype=c("dashed"), size=1, color="gray60") + 
  facet_wrap(~Group, scales ="free",  dir="h", ncol=2) + 
  ylim(-2,2) +
  labs(y = "Dynamic ATE\n(Incidents/thous. ppl)", x = "Years since the policy", size="") + 
  theme(legend.position = 'bottom', text = element_text(size=24)) 
Errorbar_plot_IDT

grid.arrange(Errorbar_plot_waste, Errorbar_plot_IDT, heights=c(2:1), nrow = 2)
