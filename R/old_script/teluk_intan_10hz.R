##### MPOB FLUX DATA ANALYSIS ################################################################################# 
# MALAYSIAN PALM OIL BOARD (MPOB) 10HZ FLUX DATA ANALYSES USING EDDY COVARIANCE SYSTEM
# This script is written to analyze 10 Hz eddy covariance data from MPOB towers
# 
# Parameters:
# u = longitudinal wind speed component (m/s)
# v = lateral wind speed component (m/s)
# w = vertical wind speed component (m/s)
# Ts = sonic temperature (deg C)
# co2 = carbon dioxide concentration (mmol/m3)
# h2o = water vapor concentration (mmol/m3)
#
# Note:
# Before using this script, you need to change original Excel data file to '.csv' format and also 
# change the 'time' column in Excel to 'custom' and 'mm:ss.0' to 'hh:mm:ss.0' so that R can 
# import it correctly.
# 
# Author: Yusri Yusup
# Date created: 2014-09-26
# Date modified: 2014-09-29
# Version: 1.1

##### 0. Preliminaries #################################################################################
## Initial analyses setup
# Installing packages

# Loading packages
# install.packages('Hmisc')

# Sourcing custom functions
library(Hmisc)
source('R/convert_magic.R')
source('R/convert_magic_num.R')

##### 1. Data import and cleanup ########################################################################
#Change name of file for other files
data_10hz <- read.csv('data/CR5000_FluxData_8.1.13.csv')
# Removing the first 3 rows of the data
data_10hz <- data_10hz[c(-1,-2,-3),]
# Removing row.names
row.names(data_10hz) <- NULL

# Adding column names
## u = longitudinal wind speed (m/s)
## v = lateral wind speed (m/s)
## w = vertical wind speed (m/s)
## co2 = carbon dioxide concentration (mmol/m3)
## h2o = water vapor concentration (mmol/m3)
## Ts = sonic temperature (deg C)
colnames(data_10hz) <- c('timestamp','time','record_no','u','v',
                        'w','co2','h2o','Ts')

# Converting from 'factors' to 'character' and from 'character' to 'numeric'
data_10hz <- convert_magic(data_10hz,c('character','character','character',
                                     'character','character','character',
                                     'character','character','character'))
data_10hz <- convert_magic_num(data_10hz,c('character','character','numeric',
                                         'numeric','numeric','numeric',
                                         'numeric','numeric','numeric'))
# Combining 'date' and 'time
# options(digits.secs=2) NOT USED HERE
timestamp_temp <- paste(data_10hz$timestamp,data_10hz$time)
# Converting combined characters to POSIXlt/POSIXct format
timestamp_temp <- strptime(timestamp_temp,"%m/%d/%y %H:%M:%OS")
# ...to display the correct millisecond
timestamp_temp <- timestamp_temp + 0.05
# Replacing date with timestamp to data_10hz
data_10hz <- within(data_10hz,timestamp<-timestamp_temp)
# Deleting the temporary timestamp_temp
rm(timestamp_temp)
# Removing the 'time' column
data_10hz <- data_10hz[,-2]

#### 3. Data analysis ###################################################

#### 4. Plots ###########################################################
# Saving old settings
par.old <- par()

# Time series plots

# 1. Time series of wind components

path_fig <- "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/time_series_ws.jpg"

jpeg(path_fig,height=800,width=600,res=100)
plot.new()

# Create a 3 by 1 panels plot
par(mfrow=c(3,1),mar=c(4.1,4.1,1.1,1.1))
plot(data_10hz$timestamp,data_10hz$u,xlab='Date and time',ylab='u (m/s)',type='l',ylim=c(-5,6))
#legend("topright",legend=c('u','v','w'),lty=c(1,1,1),col=c('black','blue','red'))
minor.tick(nx=10,ny=10)
plot(data_10hz$timestamp,data_10hz$v,xlab='Date and time',ylab='v (m/s)',type='l',ylim=c(-5,6),col='blue')
minor.tick(nx=10,ny=10)
plot(data_10hz$timestamp,data_10hz$w,xlab='Date and time',ylab='w (m/s)',type='l',ylim=c(-5,6),col='red')
minor.tick(nx=10,ny=10)
dev.off()

# 2. Time series of CO2 and H2O

path_fig <- "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/time_series_co2h2o.jpg"

jpeg(path_fig,height=800,width=600,res=100)
plot.new()

# Create a 2 by 1 panels plot
par(mfrow=c(1,1),mar=c(4.1,4.5,1.1,1.1))
plot(data_10hz$timestamp,data_10hz$co2,xlab='Date and time',ylab=expression(paste('CO'[2],' (mmol/m'^3,')')), type='l')#,ylim=c(-5,6))
minor.tick(nx=10,ny=10)
#plot(data_10hz$timestamp,data_10hz$h2o,xlab='Date and time',ylab=expression(paste('H'[2],'O',' (mmol/m'^3,')')),type='l',col='blue')#,ylim=c(-5,6))
#minor.tick(nx=10,ny=10)
dev.off()

# Cleaning up temporary variables
rm(par.old,path_fig)
