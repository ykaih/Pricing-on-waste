# Time-series model to examine the spillover effect in TYC, Yilan, and  Keelung
# Study duration for collected garbage is 2001Jan - 2016Dec
# Study duration for biodegradable waste, and rate of biodegradable waste is 2003Jan - 2014Dec
# Study duration for recyclcing is 2000Jan - 2016Dec

library(ggplot2)  # ggplot
library(stargazer) #  produces LaTeX code, HTML code and ASCII text for well-formatted tables
library(dplyr) # check for the content of a string with dplyr::filter command
library(zoo)  # as.Date
library(scales) # rescale
library(dynlm)  # dynlm()
library(lmtest) # coeftest
library(car) # use the function dwt()
library(nlme) # use the function gls()
library(sandwich) # vcovHAC
library(MuMIn)  # model.sel

# Clean all objects in a Specified Environment
rm(list = ls())

# Input data ------------------------------------------
DF_Unsorted_raw <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Unsorted.csv"))

# 1. Arrange data ------------------------------------------------------------------
# Select needed variables
DF_Unsorted <- DF_Unsorted_raw %>% select("Time","Year","Month","City","Unsorted_per_ca","Income_thous","Baby","Household_Size",
                                          "TimeIndicator","TE.NTC","MRSP",
                                          "m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11",
                                          "y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010",
                                          "y2011","y2012","y2013","y2014","y2015","y2016")

# Set up the time variable
DF_Unsorted$Time <- as.Date(as.yearmon(paste(DF_Unsorted$Year, DF_Unsorted$Month), "%Y %m"))

# Rescale the time trend variable 
UW_cutpoint = DF_Unsorted[DF_Unsorted$Time=="2008-07-01","TimeIndicator"][1]

# Create a rescaled time
DF_Unsorted$Rescale_Time <- rescale(DF_Unsorted$TimeIndicator, 
                                    to = c(min(DF_Unsorted$TimeIndicator)-UW_cutpoint, max(DF_Unsorted$TimeIndicator)-UW_cutpoint))

# Remove outliners caused by Nali Typhoon
DF_Unsorted <- DF_Unsorted[DF_Unsorted$Time!="2001-09-01",]
DF_Unsorted <- DF_Unsorted[DF_Unsorted$Time!="2001-10-01",]
DF_Unsorted$TE.NTC <- ifelse(DF_Unsorted$Rescale_Time>=0,1,0)
DF_Unsorted <- DF_Unsorted %>% filter(City %in% c("NewTaipeiCity","TaipeiCity","TaoyuanCity","YilanCounty","KeelungCity"))
DF_Unsorted$City <- factor(DF_Unsorted$City, levels = c("TaoyuanCity","KeelungCity","YilanCounty","TaipeiCity","NewTaipeiCity"),
                                             labels = c("Taoyuan~City","Keelung~City","Yilan~County","Taipei~City","New~Taipei~City"))

# Separate dataset by municipality
DF_Unsorted_NTC <- DF_Unsorted %>% filter(City %in% "New~Taipei~City")
DF_Unsorted_TPC <- DF_Unsorted %>% filter(City %in% "Taipei~City")
DF_Unsorted_TYC <- DF_Unsorted %>% filter(City %in% "Taoyuan~City")
DF_Unsorted_KLC <- DF_Unsorted %>% filter(City %in% "Keelung~City")
DF_Unsorted_YLC <- DF_Unsorted %>% filter(City %in% "Yilan~County")

# 2. Time series analysis  ------------------------------------------------------------------
# Model selction function
Fn_Model_Sel <- function(DF){
  
  model.sel(# GLS with AR(1) Correlation Structure
            gls(Unsorted_per_ca ~ Income_thous + Baby + Household_Size + TE.NTC + MRSP + Rescale_Time,
                correlation = corAR1(form=~1), data = DF),
            
            # GLS with AR(1) Correlation Structure (seasonality)
            gls(Unsorted_per_ca ~ Income_thous + Baby + Household_Size + TE.NTC + MRSP + Rescale_Time +
                  m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11,
                correlation = corAR1(form=~1), data = DF),
            
            # Regular time series model
            dynlm(Unsorted_per_ca ~ Income_thous + Baby + Household_Size + TE.NTC + MRSP + Rescale_Time, data = DF),
            
            # Regular time series model (seasonality)
            dynlm(Unsorted_per_ca ~ Income_thous + Baby + Household_Size + TE.NTC + MRSP + Rescale_Time +
                    m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11, data = DF))
}

Fn_Model_Sel(DF_Unsorted_NTC)
Fn_Model_Sel(DF_Unsorted_TPC)
Fn_Model_Sel(DF_Unsorted_TYC)
Fn_Model_Sel(DF_Unsorted_KLC)
Fn_Model_Sel(DF_Unsorted_YLC)


# Create a time-series model function
Fn_TS_UW <- function(DF){
  
  dynlm_UW <- dynlm(Unsorted_per_ca ~ Income_thous + Baby + Household_Size + TE.NTC + MRSP + Rescale_Time +
                      m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11, data = DF)
  
  gls_UW <- gls(Unsorted_per_ca ~ Income_thous + Baby + Household_Size +  TE.NTC + MRSP + Rescale_Time +
                  m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11, correlation = corAR1(form=~1), data = DF)
  
  # Save outcomes
  List_outcome <- list(DW.Statistic=dwt(dynlm_UW),
                       Regular.Result=dynlm_UW,
                       HAC.Result=coeftest(dynlm_UW, vcov. = vcovHAC),
                       GLSAR.Result = gls_UW)
}

# Run the time series model for each municipality
List_UW_NTC <- Fn_TS_UW(DF_Unsorted_NTC)
List_UW_TPC <- Fn_TS_UW(DF_Unsorted_TPC)
List_UW_TYC <- Fn_TS_UW(DF_Unsorted_TYC)
List_UW_KLC <- Fn_TS_UW(DF_Unsorted_KLC)
List_UW_YLC <- Fn_TS_UW(DF_Unsorted_YLC)

# Results: GLS with AR(1) Correlation Structure
stargazer(List_UW_TPC$GLSAR.Result, List_UW_TYC$GLSAR.Result, List_UW_KLC$GLSAR.Result, List_UW_YLC$GLSAR.Result, 
          no.space=TRUE, type="text", out = "Spillovers.txt",
          column.labels=c("Taipei","Taoyuan","Keelung","Yilan"),
          dep.var.labels=c("Unsorted waste"),
          covariate.labels=c("Income (NTD 1,000)","Percentage of babies","Household size","UBP in NTC","Mandatory recycling","Time trend","Constant"),
          omit = c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11"))
