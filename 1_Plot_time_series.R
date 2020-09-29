# Make time trend plots

# Study duration: Jan/2001 to Dec/2016 for Unsorted waste
# Study duration: Jan/2003 to Dec/2014 for Biodegradable Waste
# Study duration: Jan/2000 to Dec/2016 for Recycling (Except outlier June 2000)
# Study duration: Jan/1998 to Dec/2015 for Illegal dumping

# Load packages
library(dplyr)
library(ggplot2)
library(zoo)
library(scales)
library(gridExtra)

# Clean all objects in a Specified Environment
rm(list = ls())

# Input data
DF_Unsorted <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Unsorted.csv"))
DF_Biowaste <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Biowaste.csv"))
DF_Recycling <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Recycling.csv"))
DF_ID <- read.csv(url("https://raw.githubusercontent.com//ykaih//Pricing-on-waste//master//DF_Illegal_dumping.csv"))

# Convert the time variable as a Date object
DF_Unsorted$Time <- as.Date(as.yearmon(paste(DF_Unsorted$Year, DF_Unsorted$Month), "%Y %m"))
DF_Biowaste$Time <- as.Date(as.yearmon(paste(DF_Biowaste$Year, DF_Biowaste$Month), "%Y %m"))
DF_Recycling$Time <- as.Date(as.yearmon(paste(DF_Recycling$Year, DF_Recycling$Month), "%Y %m"))
DF_ID$Time <- as.Date(as.yearmon(paste(DF_ID$Year, DF_ID$Month), "%Y %m"))

# 1. Unsorted waste per capita (2001/Jan - 2016/Dec) --------------------------------------------
# Exclude outliers
DF_Unsorted <- DF_Unsorted[DF_Unsorted$Time!="2001-09-01" & DF_Unsorted$Time!="2001-10-01",]

# Plot
theme_set(theme_bw())
Plot_UW <- ggplot(DF_Unsorted, aes(x=Time, y=Unsorted_per_ca, color=as.factor(City_code),linetype=as.factor(City_code), size=as.factor(City_code))) + 
  geom_line() + 
  geom_vline(xintercept=as.numeric(DF_Unsorted$Time[c(59,89,118)]),linetype=c(rep("longdash",time=3)), size=0.6, color="gray25") + 
  labs(y = "Quantity of Unsorted Waste (kg/capita)", color = "City", linetype = "City", size = "City") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(limits=c(min(DF_Unsorted$Unsorted_per_ca), max(DF_Unsorted$Unsorted_per_ca))) + 
  scale_size_manual(breaks=c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"), 
                    values=c(2,2,rep(0.8,time=6)))  + 
  scale_linetype_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                        values = c(rep(c("solid", "dashed", "twodash", "F1"), each=2))) + 
  scale_color_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                     values = c(rep(c("gray0","gray55","gray20","grey66"),time=2))) + 
  labs(title = "Panel A: Unsorted") + 
  theme(legend.position = 'bottom', text = element_text(size=16), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust=1)) + 
  guides(color = guide_legend(keywidth = 3), linetype = guide_legend(keywidth = 3), size = guide_legend(keywidth = 3))
Plot_UW 

# 2. Biodegradable waste per capita (2003/Jan - 2016/Dec) --------------------------------------------
# Plot 
theme_set(theme_bw())
Plot_BW <- ggplot(DF_Biowaste, aes(x=Time, y=Biowaste_per_ca, color=as.factor(City_code), linetype=as.factor(City_code), size=as.factor(City_code))) + 
  geom_line() + 
  geom_vline(xintercept=c(as.numeric(DF_Biowaste$Time[c(37,67,96)])), linetype=c(rep("longdash",time=3)), size=0.6, color="gray25") + 
  labs(y = "Quantity of Biodeg. Waste (kg/capita)", color = "City", linetype = "City", size = "City") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(limits=c(min(DF_Biowaste$Biowaste_per_ca), max(DF_Biowaste$Biowaste_per_ca))) + 
  scale_size_manual(breaks=c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"), 
                    values=c(2,2,rep(0.8,time=6)))  + 
  scale_linetype_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                        values = c(rep(c("solid", "dashed", "twodash", "F1"), each=2))) + 
  scale_color_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                     values = c(rep(c("gray0","gray55","gray20","grey66"),time=2))) + 
  labs(title = "Panel B: Biodegradable") + 
  theme(legend.position = 'bottom', text = element_text(size=16), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust=1)) + 
  guides(color = guide_legend(keywidth = 3), linetype = guide_legend(keywidth = 3), size = guide_legend(keywidth = 3))
Plot_BW

# 3. Recycling (2000/Jan - 2016/Dec) --------------------------------------------
# Plot
theme_set(theme_bw())
Plot_RW <- ggplot(DF_Recycling, aes(x=Time, y=Recycling_per_ca, color=as.factor(City_code), linetype=as.factor(City_code), size=as.factor(City_code))) + 
  geom_line() + 
  geom_vline(xintercept=c(as.numeric(DF_Recycling$Time[c(7,73,103,132)])), linetype=c(rep("longdash",time=4)), size=0.6, color="gray25") + 
  labs(y = "Quantity of Recycling (kg/capita)", color = "City", linetype = "City", size = "City") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(limits=c(min(DF_Recycling$Recycling_per_ca),max(DF_Recycling$Recycling_per_ca))) + 
  scale_size_manual(breaks=c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"), 
                    values=c(2,2,rep(0.8,time=6)))  + 
  scale_linetype_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                        values = c(rep(c("solid", "dashed", "twodash", "F1"), each=2))) + 
  scale_color_manual(labels = c("NewTaipeiCity","TaipeiCity","TaoyuanCity","TaichungCity","TainanCity","KaohsiungCity","YilanCounty","KeelungCity"),
                     values = c(rep(c("gray0","gray55","gray20","grey66"),time=2))) + 
  labs(title = "Panel C: Recyclable") + 
  theme(legend.position = 'bottom', text = element_text(size=16), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust=1)) + 
  guides(color = guide_legend(keywidth = 3), linetype = guide_legend(keywidth = 3), size = guide_legend(keywidth = 3))
Plot_RW

# 4. Illegal citation (1998/Jan - 2015/Dec) --------------------------------------------
# Calculate yearly values
Yr_DF_Violation <- round(aggregate(Violation_per_ca ~ Year + City_code, DF_ID, sum),2)

# Yearly plot 
theme_set(theme_bw())
Plot_ID <- ggplot(Yr_DF_Violation, aes(x=Year, y=Violation_per_ca, color=as.factor(City_code), linetype=as.factor(City_code), size=as.factor(City_code))) +
  geom_line() +
  geom_vline(xintercept=c(2000.5,2006, 2008.5,2010.92), linetype=c(rep("longdash",time=4)), size=0.6) +
  labs(y = "Num. of tickets/1,000 people", color = "City", linetype = "City", size="City") +
  scale_x_continuous(breaks = seq(1998, 2016, 1)) +
  scale_y_continuous(limits=c(min(Yr_DF_Violation$Violation_per_ca), max(Yr_DF_Violation$Violation_per_ca))) +
  scale_size_manual(breaks=c("New Taipei City", "Taipei City", "Taoyuan City","Taichung City", "Tainan City", "Kaohsiung City"), 
                    values=c(2,2,rep(0.8,time=4)))  + 
  scale_linetype_manual(labels = c("New Taipei City", "Taipei City", "Taoyuan City","Taichung City", "Tainan City", "Kaohsiung City"),
                        values = c(rep(c("solid", "dashed", "twodash", "F1"), each=2))) + 
  scale_color_manual(labels = c("New Taipei City", "Taipei City", "Taoyuan City","Taichung City", "Tainan City", "Kaohsiung City"),
                     values = c(rep(c("gray0","gray55","gray20","grey66"),time=2))) + 
  labs(title = "Panel D: Illegal dumping") + 
  theme(legend.position = 'bottom', text = element_text(size=16), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust=1)) +
  guides(color = guide_legend(keywidth = 3), linetype = guide_legend(keywidth = 3), size = guide_legend(keywidth = 3))
Plot_ID

# 5. Show all plots --------------------------------------------
grid.arrange(Plot_UW, Plot_BW, Plot_RW, Plot_ID, nrow=2)
