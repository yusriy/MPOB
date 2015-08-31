##### MPOB FULL OUTPUT DATA ################################################################################# 
# MALAYSIAN PALM OIL BOARD (MPOB) MET DATA ANALYSES
# This script is written to analyze met data data from MPOB Keratong towers
# 
# Note: For 'full output' results only
# 
# Author: Yusri Yusup, PhD
# Date created: 2014-11-23
# Date modified: 2014-11-23
# Version: 1.0

##### 0. Preliminaries #################################################################################
## Initial analyses setup

# Installing packages when needed
# install.packages('Hmisc')

# Loading packages
library(Hmisc)
library(plyr)
# Sourcing custom functions
source('R/convert_magic.R')
source('R/convert_magic_num.R')
source('R/charactersNumeric.R')
source('R/asNumeric.R')
source('R/conv_umol_Mg.R')

##### 1. Data import #####################################################
# Note: Data must be placed in the 'data' folder
# For each new dataset, customize the below settings
# Type of data = 'full output'
# Here I added all the available data from Sept 2013 to Sept 2014 into different
# dataframes
year <- c('2013','2014')
month1 <- c('SEP','OCT','NOV','DIS')
month2 <- c('JAN','FEB','MAR','APR','MAY','JUNE','JUL','AUG','SEP')
date1 <- c('2013-12-10T110736.csv','2013-12-10T151209.csv','2013-12-10T164319.csv','2014-03-25T103000.csv')
date2 <- c('2014-03-24T111934.csv','2014-03-24T150740.csv','2014-04-23T104313.csv','2014-05-21T114702.csv',
          '2014-08-12T172645.csv','2014-08-17T180620.csv','2014-08-17T192744.csv','2014-09-17T230017.csv',
          '2014-10-31T191758.csv')

# Initialize the path
path1 <- 0
path2 <- 0
df_name <- 0

## Import the data
# Year 2013
for (i in 1:length(month1)){
  path1[i] <- paste('data/Result_EDDYPRO/',year[1],'/Result_',month1[i],'_',year[1],
                '/eddypro_EC_Keratong_full_output_',date1[i],sep='')
  df_name[i] <- as.character(paste('df','_',year[1],'_',month1[i],sep=''))
  assign(df_name[i],read.csv(path1[i],skip=1))
}

# Year 2014
for (i in 1:length(month2)){
  path2[i] <- paste('data/Result_EDDYPRO/',year[2],'/Result_',month2[i],'_',year[2],
                    '/eddypro_EC_Keratong_full_output_',date2[i],sep='')
  df_name[i + length(month1)] <- as.character(paste('df','_',year[2],'_',month2[i],sep=''))
  assign(df_name[i + length(month1)],read.csv(path2[i],skip=1))
}

# Some questions to ask.
# 1. What is 'ET'? Answer: Evapotranspiration
# 2. Why is some column appendded with 'hf', 'sf', and some with '1'?
# Answer: hf = hard-flagged; sf = soft-flagged,

##### Cleaning up ########################################
# Remove temporary variables
rm(date1,date2,df_name,i,month1,month2,path1,path2,year)

