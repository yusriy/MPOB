##### MPOB MET DATA ################################################################################# 
# MALAYSIAN PALM OIL BOARD (MPOB) MET DATA ANALYSES
# This script is written to analyze met data data from MPOB Teluk Intan towers
# 
# Measurement from sensors include:
# 1. Two (2) soil heat flux HFT3 (W/m2), 8 cm below the soil (Campbell Sci)
# 2. One (1) soil heat flux HFP01 (W/m2), 8 cm below the soil (Campbell Sci)
# 3. Short-wave net radiometer (W/m2) pyranometer (Kipp-Zonen)
# 4. Long-wave net radiometer (W/m2) pyrgeometer (Kipp-Zonen)
# 5. Photosynthetically Active Radiation (PAR) quantum sensor flux density (umol/s.m2) (Campbell Sci)
# 6. PAR quantum sensor total flux (mmol/m2) (Campbell Sci)
# 
# Note:
# Before using this script, you need to change original Excel data file to '.csv' format and also 
# change the 'time' column in Excel to 'time' and 'hh.mm.ss' 12-hour to 'hh:mm:ss' 24-hour basis so that R can 
# import it correctly.
# 
# Author: Yusri Yusup, PhD
# Date created: 2014-09-29
# Date modified: 2014-09-29
# Version: 1.0

##### 0. Preliminaries #################################################################################
## Initial analyses setup

# Installing packages when needed
# install.packages('Hmisc')

# Loading packages
library(Hmisc)

# Sourcing custom functions
source('R/convert_magic.R')
source('R/convert_magic_num.R')

##### 2. Data import and cleanup #########################################

## A. Importing the data into R
# Change name of file for other files
data_5min <- read.csv('data/CR5000_MetData_1.1.13-8.1.13.csv')
# Removing the first 3 rows of the data
data_5min <- data_5min[c(-1,-2,-3,-4),]
# Removing row.names
row.names(data_5min) <- NULL
# Adding column names
colnames(data_5min) <- c('timestamp','time','record_no',
                         'panel_temp_avg','batt_volt_avg',
                         'hft1_avg','hft2_avg',
                         'hfp01_1_avg',
                         'netrad_sw','netrad_lw',
                         'netsw_min','time_netsw_min',
                         'netsw_max','time_netsw_max','netsw_avg',
                         'netlw_min','time_netlw_min',
                         'netlw_max','time_netlw_max','netlw_avg',
                         'par_q_avg','par_tot')
# Converting from 'factors' to 'character' and from 'character' to 'numeric'
data_5min <- convert_magic(data_5min,c('character','character','character',
                                     'character','character','character',
                                     'character','character','character',
                                     'character','character','character',
                                     'character','character','character',
                                     'character','character','character',
                                     'character','character','character','character'))
data_5min <- convert_magic_num(data_5min,c('character','character',
                                           'numeric','numeric','numeric','numeric',
                                           'numeric','numeric','numeric','numeric',
                                           'numeric','character','numeric','character',
                                           'numeric','numeric','character','numeric',
                                           'character','numeric','numeric','numeric'))
                                         
## B. Time manipulation

# Save date for future use with time_netsw_min, etc.
timestamp <- data_5min$timestamp
# i. Combining 'date' and 'time
timestamp_temp <- paste(data_5min$timestamp,data_5min$time)
# Converting combined characters to POSIXlt/POSIXct format
timestamp_temp <- strptime(timestamp_temp,"%m/%d/%y %H:%M")
# Replacing date with timestamp to data_5min
data_5min <- within(data_5min,timestamp<-timestamp_temp)
# Deleting the temporary timestamp_temp
rm(timestamp_temp)
# Removing the 'time' column
data_5min <- data_5min[,-2]

# ii. Changing the time format for 'time_netsw_min', time_netsw_max', time_netlw_min', and 'time_netlw_max'

data_5min$time_netsw_min <- strptime(paste(timestamp,data_5min$time_netsw_min),"%m/%d/%y %H:%M:%S")
data_5min$time_netsw_max <- strptime(paste(timestamp,data_5min$time_netsw_max),"%m/%d/%y %H:%M:%S")
data_5min$time_netlw_min <- strptime(paste(timestamp,data_5min$time_netlw_min),"%m/%d/%y %H:%M:%S")
data_5min$time_netlw_max <- strptime(paste(timestamp,data_5min$time_netlw_max),"%m/%d/%y %H:%M:%S")

# Delete temporary 'timestamp'
rm(timestamp)

##### 3. Analyzing the data ####################################################################################

##### 4. Plots #################################################################################################

old.par <- par()
# Time series plots

# 1. Time series of soil heat fluxes (1 and 2) HFT1 and HFT2

path_fig <- "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/hft1and2_hfp01_ws.jpg"

jpeg(path_fig,height=800,width=600,res=100)
plot.new()

# Create a 3 by 1 panels plot
par(mfrow=c(3,1),mar=c(4.1,4.5,1.1,1.1))
plot(data_5min$timestamp,data_5min$hft1_avg,xlab='Date and time',ylab=expression(paste('Soil heat flux (W/m'^2,')')),type='l',main='HFT1',ylim=c(-100,400))
#legend("topright",legend=c('u','v','w'),lty=c(1,1,1),col=c('black','blue','red'))
minor.tick(nx=10,ny=10)
plot(data_5min$timestamp,data_5min$hft2_avg,xlab='Date and time',ylab=expression(paste('Soil heat flux (W/m'^2,')')),type='l',main='HFT2',ylim=c(-100,400))
minor.tick(nx=10,ny=10)
plot(data_5min$timestamp,data_5min$hfp01_1_avg,xlab='Date and time',ylab=expression(paste('Soil heat flux (W/m'^2,')')),type='l',main='HFP01',ylim=c(-100,400))
minor.tick(nx=10,ny=10)
dev.off()

# 2. Time series of net radiation short-wave and long-wave

path_fig <- "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/net_swAndlw.jpg"

jpeg(path_fig,height=800,width=600,res=100)
plot.new()

# Create a 2 by 1 panels plot
par(mfrow=c(2,1),mar=c(4.1,4.5,1.1,1.1))
plot(data_5min$timestamp,data_5min$netrad_sw,xlab='Date and time',ylab=expression(paste('Short-wave net rad. (W/m'^2,')')),type='l')#,ylim=c(-100,1000))
#legend("topright",legend=c('u','v','w'),lty=c(1,1,1),col=c('black','blue','red'))
minor.tick(nx=10,ny=10)
plot(data_5min$timestamp,data_5min$netrad_lw,xlab='Date and time',ylab=expression(paste('Long-wave net rad. (W/m'^2,')')),type='l')#,ylim=c(-100,1000))
minor.tick(nx=10,ny=10)
dev.off()


# 3. Time series of PAR quantum sensor

path_fig <- "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/par_flux_tot.jpg"

jpeg(path_fig,height=800,width=600,res=100)
plot.new()

# Create a 2 by 1 panels plot
par(mfrow=c(2,1),mar=c(4.1,4.5,1.1,1.1))
plot(data_5min$timestamp,data_5min$par_q_avg,xlab='Date and time',ylab=expression(paste('PAR quantum sensor flux density (',mu,'mol/m'^2,'s)')),type='l')#,ylim=c(-100,1000))
#legend("topright",legend=c('u','v','w'),lty=c(1,1,1),col=c('black','blue','red'))
minor.tick(nx=10,ny=10)
plot(data_5min$timestamp,data_5min$par_tot,xlab='Date and time',ylab=expression(paste('PAR quantum sensor total flux (mmol/m'^2,')')),type='l')#,ylim=c(-100,1000))
minor.tick(nx=10,ny=10)

dev.off()

# Cleaning up temporary variables
rm(path_fig,old.par)
