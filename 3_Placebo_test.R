# Study duration: Jan/2001 to Dec/2016 for Unsorted waste
# Study duration: Jan/2003 to Dec/2014 for Biodegradable Waste
# Study duration: Jan/2000 to Dec/2016 for Recycling (Except outlier June 2000)
# Study duration: Jan/1998 to Dec/2015 for Illegal dumping

# Load packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(stargazer)
library(gridExtra)  # grid.arrange
library(zoo)  # as.Date

# Clean all objects in a Specified Environment
rm(list = ls())

# Input data ------------------------------------------
DF_Unsorted <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Unsorted.csv"))
DF_Biowaste <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Biowaste.csv"))
DF_Recycling <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Recycling.csv"))
DF_ID <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Illegal_dumping.csv"))

# Convert the time variable as a Date object
DF_Unsorted$Time <- as.Date(as.yearmon(paste(DF_Unsorted$Year, DF_Unsorted$Month), "%Y %m"))
DF_Biowaste$Time <- as.Date(as.yearmon(paste(DF_Biowaste$Year, DF_Biowaste$Month), "%Y %m"))
DF_Recycling$Time <- as.Date(as.yearmon(paste(DF_Recycling$Year, DF_Recycling$Month), "%Y %m"))
DF_ID$Time <- as.Date(as.yearmon(paste(DF_ID$Year, DF_ID$Month), "%Y %m"))

# Create UBP dummies
DF_Unsorted$UBP <- ifelse(DF_Unsorted$City_code==2 | 
                        (DF_Unsorted$City_code==1 & DF_Unsorted$TimeIndicator>=DF_Unsorted[DF_Unsorted$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_Biowaste$UBP <- ifelse(DF_Biowaste$City_code==2 | 
                        (DF_Biowaste$City_code==1 & DF_Biowaste$TimeIndicator>=DF_Biowaste[DF_Biowaste$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_Recycling$UBP <- ifelse(DF_Recycling$City_code==2 & DF_Recycling$TimeIndicator>=DF_Recycling[DF_Recycling$Time=="2000-07-01","TimeIndicator"][1] | 
                       (DF_Recycling$City_code==1 & DF_Recycling$TimeIndicator>=DF_Recycling[DF_Recycling$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)
DF_ID$UBP <- ifelse(DF_ID$City_code==2 | 
                      (DF_ID$City_code==1 & DF_ID$TimeIndicator>=DF_ID[DF_ID$Time=="2008-07-01","TimeIndicator"][1]), 1, 0)

# 1. Create counterfactual dummies ------------------------------------------
# 1.1 Unsorted waste Placebo test (200101 - 200806, 8 municipalities) ----------------------------
# Creat subset of data before 2008 July
DF_Placebo_UW_raw <- DF_Unsorted[DF_Unsorted$Time < "2008-07-01", ]

# Extract a range of TimeIndicator
Range_UW <- unique(DF_Placebo_UW_raw[DF_Placebo_UW_raw$Time < "2008-07-01", "TimeIndicator"])
Length_UW <- length(Range_UW)

# Create a empty matrix
Mat_UW <- matrix(data = NA, nrow=nrow(DF_Placebo_UW_raw), ncol=Length_UW*2)

# Create counterfactual treated time dummies 
for(i in 1:Length_UW){
  Mat_UW[,i] <- assign(paste("PT", i, sep = "."), as.numeric(DF_Placebo_UW_raw$TimeIndicator >= i))
  Mat_UW[,i+Length_UW] <- assign(paste("TE", i, sep = "."), Mat_UW[,i]*DF_Placebo_UW_raw$FE_NTC)   
}

# Create column names
colnames_UW <- vector(mode = "logical", length = Length_UW*2)
for(i in 1:Length_UW){
  colnames_UW[i] <- paste("PT", i, sep = ".")
  colnames_UW[i+Length_UW] <- paste("TE", i, sep = ".")
}

# Convert the matrix to a dataframe
Mat_UW <- data.frame(Mat_UW)
colnames(Mat_UW) <- colnames_UW

# Drop the first and last periods to avoid multicollinearity
del_list_UW <- c("PT.1", paste("PT", Length_UW, sep = "."), "TE.1", paste("TE", Length_UW, sep = "."))
Mat_UW <- Mat_UW[,!(colnames(Mat_UW) %in% del_list_UW)]

# Combine the placebo dataframe with original data
DF_Placebo_UW <- cbind.data.frame(DF_Placebo_UW_raw, Mat_UW)

# 1.2 Biowaste Placebo test (200301 - 200806, 8 municipalities) ----------------------------
# Creat subset of data before 2008 July
DF_Placebo_BW_raw <- DF_Biowaste[DF_Biowaste$Time < "2008-07-01", ]

# Extract a range of TimeIndicator
Range_BW <- unique(DF_Placebo_BW_raw[DF_Placebo_BW_raw$Time < "2008-07-01", "TimeIndicator"])
Length_BW <- length(Range_BW)

# Create a empty matrix
Mat_BW <- matrix(data = NA, nrow=nrow(DF_Placebo_BW_raw), ncol=Length_BW*2)

# Create counterfactual treated time dummies 
for(i in 1:Length_BW){
  Mat_BW[,i] <- assign(paste("PT", i, sep = "."), as.numeric(DF_Placebo_BW_raw$TimeIndicator >= i))
  Mat_BW[,i+Length_BW] <- assign(paste("TE", i, sep = "."), Mat_BW[,i]*DF_Placebo_BW_raw$FE_NTC)   
}

# Create column names
colnames_BW <- vector(mode = "logical", length = Length_BW*2)
for(i in 1:Length_BW){
  colnames_BW[i] <- paste("PT", i, sep = ".")
  colnames_BW[i+Length_BW] <- paste("TE", i, sep = ".")
}

# Convert the matrix to a dataframe
Mat_BW <- data.frame(Mat_BW)
colnames(Mat_BW) <- colnames_BW

# Drop the first and last periods to avoid multicollinearity
del_list_BW <- c("PT.1", paste("PT", Length_BW, sep = "."), "TE.1", paste("TE", Length_BW, sep = "."))
Mat_BW <- Mat_BW[,!(colnames(Mat_BW) %in% del_list_BW)]

# Combine the placebo dataframe with original data
DF_Placebo_BW <- cbind.data.frame(DF_Placebo_BW_raw, Mat_BW)

# 1.3 Recycling Placebo test (200001 - 200806, 8 municipalities) ----------------------------
# Creat subset of data before 2008 July
DF_Placebo_RW_raw <- DF_Recycling[DF_Recycling$Time < "2008-07-01", ]

# Extract a range of TimeIndicator
Range_RW <- unique(DF_Placebo_RW_raw[DF_Placebo_RW_raw$Time < "2008-07-01", "TimeIndicator"])
Length_RW <- length(Range_RW)

# Create a empty matrix
Mat_RW <- matrix(data = NA, nrow=nrow(DF_Placebo_RW_raw), ncol=Length_RW*2)

# Create counterfactual treated time dummies 
for(i in 1:Length_RW){
  Mat_RW[,i] <- assign(paste("PT", i, sep = "."), as.numeric(DF_Placebo_RW_raw$TimeIndicator >= i))
  Mat_RW[,i+Length_RW] <- assign(paste("TE", i, sep = "."), Mat_RW[,i]*DF_Placebo_RW_raw$FE_NTC)   
}

# Create column names
colnames_RW <- vector(mode = "logical", length = Length_RW*2)
for(i in 1:Length_RW){
  colnames_RW[i] <- paste("PT", i, sep = ".")
  colnames_RW[i+Length_RW] <- paste("TE", i, sep = ".")
}

# Convert the matrix to a dataframe
Mat_RW <- data.frame(Mat_RW)
colnames(Mat_RW) <- colnames_RW

# Drop the first and last periods to avoid multicollinearity
del_list_RW <- c("PT.1", paste("PT", Length_RW, sep = "."), "TE.1", paste("TE", Length_RW, sep = "."))
Mat_RW <- Mat_RW[,!(colnames(Mat_RW) %in% del_list_RW)]

# Combine the placebo dataframe with original data
DF_Placebo_RW <- cbind.data.frame(DF_Placebo_RW_raw, Mat_RW)

# 1.4 Illegal dumping fine Placebo test (199801 - 200806, 8 municipalities) ----------------------------
# Creat subset of data before 2008 July
DF_Placebo_ID_raw <- DF_ID[DF_ID$Time < "2008-07-01", ]

# Extract a range of TimeIndicator
Range_ID <- unique(DF_Placebo_ID_raw[DF_Placebo_ID_raw$Time < "2008-07-01", "TimeIndicator"])
Length_ID <- length(Range_ID)

# Create a empty matrix
Mat_ID <- matrix(data = NA, nrow=nrow(DF_Placebo_ID_raw), ncol=Length_ID*2)

# Create counterfactual treated time dummies 
for(i in 1:Length_ID){
  Mat_ID[,i] <- assign(paste("PT", i, sep = "."), as.numeric(DF_Placebo_ID_raw$TimeIndicator >= i))
  Mat_ID[,i+Length_ID] <- assign(paste("TE", i, sep = "."), Mat_ID[,i]*DF_Placebo_ID_raw$FE_NTC)   
}

# Create column names
colnames_ID <- vector(mode = "logical", length = Length_ID*2)
for(i in 1:Length_ID){
  colnames_ID[i] <- paste("PT", i, sep = ".")
  colnames_ID[i+Length_ID] <- paste("TE", i, sep = ".")
}

# Convert the matrix to a dataframe
Mat_ID <- data.frame(Mat_ID)
colnames(Mat_ID) <- colnames_ID

# Drop the first and last periods to avoid multicollinearity
del_list_ID <- c("PT.1", paste("PT", Length_ID, sep = "."), "TE.1", paste("TE", Length_ID, sep = "."))
Mat_ID <- Mat_ID[,!(colnames(Mat_ID) %in% del_list_ID)]

# Combine the placebo dataframe with original data
DF_Placebo_ID <- cbind.data.frame(DF_Placebo_ID_raw, Mat_ID)

# 2. Placebo tests ------------------------------------------
# 2.1 Unsorted waste ------------------------------------------
# All samples 
All_obs_UW <- lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
                 + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
                 + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
                 + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Unsorted)
True.ATE.UW <- All_obs_UW$coefficients["TE.NTC"]

Length_UW <- length(unique(DF_Placebo_UW$TimeIndicator))

lm_UW <- lapply(2:(Length_UW-1), function(x) lm(Unsorted_per_ca ~ Baby + Household_Size + Income_thous
                                            + FE_TPC + DF_Placebo_UW[,sprintf("PT.%1.0f", x)] + FE_NTC 
                                            + DF_Placebo_UW[,sprintf("TE.%1.0f", x)] + MRSP + TimeIndicator 
                                            + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan 
                                            + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Placebo_UW))

# Convert the list obj to a matrix form
Coef.list.UW <- sapply(lm_UW, '[[', 1)

# Extract Placebo coefficient values
Coef.vec.UW <- round(Coef.list.UW[8,],4)

# Extract p-values
pvalue.list.UW <- lapply(1:length(lm_UW), function(x)summary(lm_UW[[x]])$coefficients[8,4])
pvalue.list.UW <- round(sapply(pvalue.list.UW, '[[', 1),4)

# 2.2 Biowaste ------------------------------------------
# All samples 
All_obs_BW <- lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
                 + FE_TPC + TT_NTC + FE_NTC + TE.NTC + MRSP + I(UBP*MRSP) + TimeIndicator 
                 + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
                 + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Biowaste)
True.ATE.BW <- All_obs_BW$coefficients["TE.NTC"]

Length_BW <- length(unique(DF_Placebo_BW$TimeIndicator))

lm_BW <- lapply(2:(Length_BW-1), function(x) lm(Biowaste_per_ca ~ Baby + Household_Size + Income_thous
                                                + FE_TPC + DF_Placebo_BW[,sprintf("PT.%1.0f", x)] + FE_NTC 
                                                + DF_Placebo_BW[,sprintf("TE.%1.0f", x)] + MRSP + TimeIndicator 
                                                + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan 
                                                + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Placebo_BW))

# Convert the list obj to a matrix form
Coef.list.BW <- sapply(lm_BW, '[[', 1)

# Extract Placebo coefficient values
Coef.vec.BW <- round(Coef.list.BW[8,],4)

# Extract p-values
pvalue.list.BW <- lapply(1:length(lm_BW), function(x)summary(lm_BW[[x]])$coefficients[8,4])
pvalue.list.BW <- round(sapply(pvalue.list.BW, '[[', 1),4)

# 2.3 Recycling ------------------------------------------
# All samples 
All_obs_RW <- lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
                 + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
                 + MRSP + I(UBP*MRSP) + TimeIndicator 
                 + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan
                 + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Recycling)
True.ATE.RW <- All_obs_RW$coefficients["TE.NTC"]

# 200001 - 200806
Length_RW <- length(unique(DF_Placebo_RW$TimeIndicator))
lm_RW <- lapply(2:(Length_RW-1), function(x) lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
                                                + TT_TPC + FE_TPC + TE.TPC + DF_Placebo_RW[,sprintf("PT.%1.0f", x)] + FE_NTC 
                                                + DF_Placebo_RW[,sprintf("TE.%1.0f", x)] + MRSP + TimeIndicator 
                                                + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan 
                                                + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Placebo_RW))

# Convert the list obj to a matrix form
Coef.list.RW <- sapply(lm_RW, '[[', 1)

# Extract Placebo coefficient values
Coef.vec.RW <- round(Coef.list.RW[10,],4)

# Extract p-values
pvalue.list.RW <- lapply(1:length(lm_RW), function(x)summary(lm_RW[[x]])$coefficients[10,4])
pvalue.list.RW <- round(sapply(pvalue.list.RW, '[[', 1),4)

# 200601 - 200806
DF_Placebo_RW$Time <- as.Date(as.yearmon(paste(DF_Placebo_RW$Year, DF_Placebo_RW$Month), "%Y %m"))
DF_Placebo_RW_post <- DF_Placebo_RW[DF_Placebo_RW$Time>="2006-01-01",]
Range_post <- unique(DF_Placebo_RW_post$TimeIndicator)
Length_RW1 <- length(unique(DF_Placebo_RW_post$TimeIndicator))
lm_RW1 <- lapply(Range_post[-c(1,length(Range_post))], function(x) lm(Recycling_per_ca ~ Baby + Household_Size + Income_thous
                                                + TT_TPC + FE_TPC + TE.TPC + DF_Placebo_RW_post[,sprintf("PT.%1.0f", x)] + FE_NTC 
                                                + DF_Placebo_RW_post[,sprintf("TE.%1.0f", x)] + TimeIndicator 
                                                + FE_TYC + FE_TCC + FE_TNC + FE_KSC + FE_Yilan 
                                                + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Placebo_RW_post))

# Convert the list obj to a matrix form
Coef.list.RW1 <- sapply(lm_RW1, '[[', 1)

# Extract Placebo coefficient values
Coef.vec.RW1 <- round(Coef.list.RW1[10,],4)

# Extract p-values
pvalue.list.RW1 <- lapply(1:length(lm_RW1), function(x)summary(lm_RW1[[x]])$coefficients[10,4])
pvalue.list.RW1 <- round(sapply(pvalue.list.RW1, '[[', 1),4)

# 2.4 Illegal dumping ------------------------------------------
# All samples 
All_obs_ID <- lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
                 + TT_TPC + FE_TPC + TE.TPC + TT_NTC + FE_NTC + TE.NTC 
                 + MRSP + I(UBP*MRSP) + TimeIndicator 
                 + FE_TYC + FE_TCC + FE_TNC
                 + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_ID)
True.ATE.ID <- All_obs_ID$coefficients["TE.NTC"]

Length_ID <- length(unique(DF_Placebo_ID$TimeIndicator))

lm_ID <- lapply(2:(Length_ID-1), function(x) lm(Violation_per_ca ~ Baby + Household_Size + Income_thous
                                                + TT_TPC + FE_TPC + TE.TPC + FE_TPC + DF_Placebo_ID[,sprintf("PT.%1.0f", x)] + FE_NTC 
                                                + DF_Placebo_ID[,sprintf("TE.%1.0f", x)] + MRSP + TimeIndicator 
                                                + FE_TYC + FE_TCC + FE_TNC
                                                + m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11, data = DF_Placebo_ID))

# Convert the list obj to a matrix form
Coef.list.ID <- sapply(lm_ID, '[[', 1)

# Extract Placebo coefficient values
Coef.vec.ID <- round(Coef.list.ID[10,],4)

# Extract p-values
pvalue.list.ID <- lapply(1:length(lm_ID), function(x)summary(lm_ID[[x]])$coefficients[10,4])
pvalue.list.ID <- round(sapply(pvalue.list.ID, '[[', 1),4)

# 2.5 Plot the PDF histogram ------------------------------------------
# Create dataframe
DF_UW_PT <- data.frame(UW = Coef.vec.UW)
DF_BW_PT <- data.frame(BW = Coef.vec.BW)
DF_RW_PT <- data.frame(RW = Coef.vec.RW)
DF_RW1_PT <- data.frame(RW=Coef.vec.RW1)
DF_ID_PT <- data.frame(ID = Coef.vec.ID)

# Unsorted
theme_set(theme_bw())
Hist_UW <- ggplot(data = DF_UW_PT, aes(x=UW)) +
  geom_histogram(binwidth=0.15) +
  geom_vline(xintercept=True.ATE.UW, color="gray50", linetype="dashed", size=1) +
  geom_density(size=0.9) +
  xlim(-6, 3) +
  labs(x = "ATU (kg/per capita)",
       y = "Count", title = "Panel A: Unsorted waste") +
  theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5))

# Biodegradable
theme_set(theme_bw())
Hist_BW <- ggplot(data = DF_BW_PT, aes(x=BW)) +
  geom_histogram(binwidth=0.025) +
  geom_vline(xintercept=True.ATE.BW, color="gray50", linetype="dashed", size=1) +
  geom_density(size=0.9) +
  xlim(-1, 1) +
  labs(x = "ATU (kg/per capita)",
       y = "Count", title = "Panel B: Biodegradable waste") +
  theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5))

# Recycling (200001-201612)
theme_set(theme_bw())
Hist_RW <- ggplot(data = DF_RW_PT, aes(x=RW)) +
  geom_histogram(binwidth=0.05) +
  geom_vline(xintercept=True.ATE.RW, color="gray50", linetype="dashed", size=1) +
  geom_density(size=0.9) +
  xlim(-2, 2) +
  labs(x = "ATU (kg/per capita)",
       y = "Count", title = "Panel C: Recycling") +
  theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5))

# Recycling (Post-2006)
theme_set(theme_bw())
Hist_RW_Post <- ggplot(data = DF_RW1_PT, aes(x=RW)) +
  geom_histogram(binwidth=0.05) +
  geom_vline(xintercept=True.ATE.RW, color="gray50", linetype="dashed", size=1) +
  geom_density(size=0.9) +
  xlim(-2, 2) +
  labs(x = " ATU (kg/per capita)",
       y = "Count", title = "Panel C: Recycling") +
  theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5))

# Illegal dumping: tickets
theme_set(theme_bw())
Hist_ID <- ggplot(data = DF_ID_PT, aes(x=ID)) +
  geom_histogram(binwidth=0.01) +
  geom_vline(xintercept=True.ATE.ID, color="gray50", linetype="dashed", size=1) +
  geom_density(size=0.9) +
  xlim(-0.4, 0.4) +
  labs(x = "ATU (Num. of tickets/1,000 people)",
       y = "Count", title = "Panel D: Illegal dumping tickets") +
  theme(text = element_text(size=14), plot.title = element_text(hjust = 0.5))

## Show all plots
grid.arrange(Hist_UW, Hist_BW, Hist_RW, Hist_ID, nrow = 2)

# 3. Summary statistic of counterfactual treatment effects ------------------------------------------
True.ATE.UW
True.ATE.BW
True.ATE.RW
True.ATE.ID

# two-tailed t test
t.test(Coef.vec.UW, mu=True.ATE.UW)
t.test(Coef.vec.BW, mu=True.ATE.BW)
t.test(Coef.vec.RW, mu=True.ATE.RW)
t.test(Coef.vec.ID, mu=True.ATE.ID)

# 4. t-tests for the growth rate prior to the treatment ------------------------------------------
# 4.1 Unsorted waste ------------------------------------------
# Conduct a t-test for confirming the parallel assumption for collected waste data (Jan 2001 - June 2008)

# Select desired variables
DF_UW1 <- DF_Unsorted %>% select("Time", "City", "Unsorted_per_ca") 
DF_UW1 <- DF_UW1 %>% filter(Time < "2008-07-01")

# Rearrange from a long DF to a wide DF
DF_UW1 <- dcast(data = DF_UW1, Time ~ City, value.var = "Unsorted_per_ca")

# Rearrange columns
DF_UW1 <- DF_UW1[,c("Time", "NewTaipeiCity", "TaipeiCity", "TaoyuanCity", "TaichungCity", "TainanCity", "KaohsiungCity","YilanCounty", "KeelungCity")]

# Create an empty vector
DF_GR_UW <- matrix(nrow = nrow(DF_UW1)-1, ncol=ncol(DF_UW1)-1)

# Create an empty vector
GR_UW <- matrix(nrow = nrow(DF_UW1)-1, ncol=ncol(DF_UW1)-1)

# Calculate the growth rate of collected garbage for each city

## Loop for time
for(i in 1:nrow(DF_UW1)-1){
  
  ## Loop for cities  
  for(j in 1:ncol(DF_UW1)-1){
    GR_UW[i,j] <- (DF_UW1[i+1,j+1] - DF_UW1[i,j+1])/DF_UW1[i,j+1]    
  }
}

# Add a time variable and rename columnes' names 
CollectedWaste_Growth_Rate <- cbind.data.frame(DF_UW1$Time[-1],GR_UW)
colnames(CollectedWaste_Growth_Rate) <- c("Time","NTC","TPC","TYC","TCC","TNC","KSC","YLC","KLC")

# T-test between NTC and TPC
GR_UW_TPC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$TPC, alternative = "less", mu=0)$p.value

# T-test between NTC and KLC
GR_UW_KLC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$KLC, alternative = "less", mu=0)$p.value

# T-test between NTC and YLC
GR_UW_YLC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$YLC, alternative = "less", mu=0)$p.value

# T-test between NTC and TYC
GR_UW_TYC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$TYC, alternative = "less", mu=0)$p.value

# T-test between NTC and TCC
GR_UW_TCC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$TCC, alternative = "less", mu=0)$p.value

# T-test between NTC and TNC
GR_UW_TNC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$TNC, alternative = "less", mu=0)$p.value

# T-test between NTC and KSC
GR_UW_KSC <- t.test(CollectedWaste_Growth_Rate$NTC, CollectedWaste_Growth_Rate$KSC, alternative = "less", mu=0)$p.value

# 4.2 Biodegradable waste ------------------------------------------
# Conduct a t-test for confirming the parallel assumption for biodegradable waste data (Jan 2003 - June 2008)

# Select desired variables
DF_BW1 <- DF_Biowaste %>% select("Time", "City", "Biowaste_per_ca") 
DF_BW1 <- DF_BW1 %>% filter(Time < "2008-07-01")

# Rearrange from a long DF to a wide DF
DF_BW1 <- dcast(data = DF_BW1, Time ~ City, value.var = "Biowaste_per_ca")

# Rearrange columns
DF_BW1 <- DF_BW1[,c("Time", "NewTaipeiCity", "TaipeiCity", "TaoyuanCity", "TaichungCity", "TainanCity", "KaohsiungCity","YilanCounty", "KeelungCity")]

# Create an empty vector
DF_GR_BW <- matrix(nrow = nrow(DF_BW1)-1, ncol=ncol(DF_BW1)-1)

# Calculate the growth rate of bio waste for each city

## Loop for time
for(i in 1:nrow(DF_BW1)-1){
  
  ## Loop for cities
  for(j in 1:ncol(DF_BW1)-1){
    DF_GR_BW[i,j] <- (DF_BW1[i+1,j+1] - DF_BW1[i,j+1])/DF_BW1[i,j+1]    
  }
}

# Add a time variable and rename columnes' names 
BioWaste_Growth_Rate <- cbind.data.frame(DF_BW1$Time[-1],DF_GR_BW)
colnames(BioWaste_Growth_Rate) <- c("Time","NTC","TPC","TYC","TCC","TNC","KSC","YLC","KLC")

# T-test between NTC and TPC
GR_BW_TPC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$TPC, alternative = "greater", mu=0)$p.value

# T-test between NTC and KLC
GR_BW_KLC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$KLC, alternative = "greater", mu=0)$p.value

# T-test between NTC and YLC
GR_BW_YLC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$YLC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TYC
GR_BW_TYC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$TYC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TCC
GR_BW_TCC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$TCC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TNC
GR_BW_TNC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$TNC, alternative = "greater", mu=0)$p.value

# T-test between NTC and KSC
GR_BW_KSC <- t.test(BioWaste_Growth_Rate$NTC, BioWaste_Growth_Rate$KSC, alternative = "greater", mu=0)$p.value

# 4.3 Recycling ------------------------------------------
# Conduct a t-test for confirming the parallel assumption for recycling data (from Jan 1998 to June 2008 (NTC)(TPC))
# (Drop 2000.06 observation, outliner in TPC)
# Select desired variables
DF_RW1 <- DF_Recycling %>% select("Time", "City", "Recycling_per_ca") 
DF_RW1 <- DF_RW1 %>% filter(Time < "2008-07-01")
DF_RW1 <- DF_RW1 %>% filter(Time != "2000-06-01")

# Rearrange from a long DF to a wide DF
DF_RW1 <- dcast(data = DF_RW1, Time ~ City, value.var = "Recycling_per_ca")

# Rearrange columns
DF_RW1 <- DF_RW1[,c("Time", "NewTaipeiCity", "TaipeiCity", "TaoyuanCity", "TaichungCity", "TainanCity", "KaohsiungCity","YilanCounty", "KeelungCity")]

# Create an empty vector
DF_GR_RW <- matrix(nrow = nrow(DF_RW1)-1, ncol=ncol(DF_RW1)-1)

# Calculate the growth rate of recycling for each city

## Loop for time
for(i in 1:nrow(DF_RW1)-1){
  
  ## Loop for cities  
  for(j in 1:ncol(DF_RW1)-1){
    DF_GR_RW[i,j] <- (DF_RW1[i+1,j+1] - DF_RW1[i,j+1])/DF_RW1[i,j+1]    
  }
}

# Add a time variable and rename columnes' names 
Recycling_Growth_Rate <- cbind.data.frame(DF_RW1$Time[-1],DF_GR_RW)
colnames(Recycling_Growth_Rate) <- c("Time","NTC","TPC","TYC","TCC","TNC","KSC","YLC","KLC")

# T-test between NTC and TPC
GR_RW_TPC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$TPC, alternative = "greater", mu=0)$p.value

# T-test between NTC and KLC
GR_RW_KLC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$KLC, alternative = "greater", mu=0)$p.value

# T-test between NTC and YLC
GR_RW_YLC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$YLC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TYC
GR_RW_TYC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$TYC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TCC
GR_RW_TCC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$TCC, alternative = "greater", mu=0)$p.value

# T-test between NTC and TNC
GR_RW_TNC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$TNC, alternative = "greater", mu=0)$p.value

# T-test between NTC and KSC
GR_RW_KSC <- t.test(Recycling_Growth_Rate$NTC, Recycling_Growth_Rate$KSC, alternative = "greater", mu=0)$p.value


# 4.4 Illegal dumping ------------------------------------------
# Conduct a t-test for confirming the parallel assumption for recycling data (from Jan 1998 to June 2008 (NTC)(TPC))
# Select desired variables
DF_ID1 <- DF_ID %>% select("Time", "City", "Violation_per_ca") 
DF_ID1 <- DF_ID1 %>% filter(Time < "2008-07-01")

# Rearrange from a long DF to a wide DF
DF_ID1 <- dcast(data = DF_ID1, Time ~ City, value.var = "Violation_per_ca")

# Rearrange columns
DF_ID1 <- DF_ID1[,c("Time", "NewTaipeiCity", "TaipeiCity", "TaoyuanCity", "TaichungCity", "TainanCity", "KaohsiungCity")]

# Create an empty vector
DF_GR_ID <- matrix(nrow = nrow(DF_ID1)-1, ncol=ncol(DF_ID1)-1)

# Calculate the growth rate of collected garbage for each city

## Loop for time
for(i in 1:(nrow(DF_ID1)-1)){
  
  ## Loop for cities  
  for(j in 1:(ncol(DF_ID1)-1)){
    DF_GR_ID[i,j] <- (DF_ID1[i+1,j+1] - DF_ID1[i,j+1])/DF_ID1[i,j+1]    
  }
}

# Add a time variable and rename columnes' names 
ID_Growth_Rate <- cbind.data.frame(DF_ID1$Time[-1],DF_GR_ID)
colnames(ID_Growth_Rate) <- c("Time","NTC","TPC","TYC","TCC","TNC","KSC")

# T-test between NTC and TPC
GR_ID_TPC <- t.test(ID_Growth_Rate$NTC, ID_Growth_Rate$TPC, alternative = "less", mu=0)$p.value

# T-test between NTC and TYC
GR_ID_TYC <- t.test(ID_Growth_Rate$NTC, ID_Growth_Rate$TYC, alternative = "less", mu=0)$p.value

# T-test between NTC and TCC
GR_ID_TCC <- t.test(ID_Growth_Rate$NTC, ID_Growth_Rate$TCC, alternative = "less", mu=0)$p.value

# T-test between NTC and TNC
GR_ID_TNC <- t.test(ID_Growth_Rate$NTC, ID_Growth_Rate$TNC, alternative = "less", mu=0)$p.value

# T-test between NTC and KSC
GR_ID_KSC <- t.test(ID_Growth_Rate$NTC, ID_Growth_Rate$KSC, alternative = "less", mu=0)$p.value

GR_ID_TPC <- round(GR_ID_TPC,4)
GR_ID_TYC <- round(GR_ID_TYC,4) 
GR_ID_TCC <- round(GR_ID_TCC,4) 
GR_ID_TNC <- round(GR_ID_TNC,4) 
GR_ID_KSC <- round(GR_ID_KSC,4) 

# 4.5 summary ------------------------------------------
DF_T_test <- data.frame(
  Unsorted = round(c(GR_UW_TPC, GR_UW_KLC, GR_UW_YLC, GR_UW_TYC, GR_UW_TCC, GR_UW_TNC, GR_UW_KSC),4),
  Biodegradable = round(c(GR_BW_TPC, GR_BW_KLC, GR_BW_YLC, GR_BW_TYC, GR_BW_TCC, GR_BW_TNC, GR_BW_KSC),4),
  Recycling = round(c(GR_RW_TPC, GR_RW_KLC, GR_RW_YLC, GR_RW_TYC, GR_RW_TCC, GR_RW_TNC, GR_RW_KSC),4),
  Illegal_dumping = c(GR_ID_TPC, "", "", GR_ID_TYC, GR_ID_TCC, GR_ID_TNC, GR_ID_KSC)
)

# Edit row names
rownames(DF_T_test) <- c("NTC=TPC","NTC=KLC","NTC=YLC","NTC=TYC","NTC=TCC","NTC=TNC","NTC=KSC")

# Summary
stargazer(DF_T_test, header = FALSE, summary=FALSE, type = "text", out = "t-Test.txt")