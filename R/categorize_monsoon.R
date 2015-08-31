## This script is to separate the data into four Monsoon seasons

# Load required libraries
library(openair)

## Note: Northeast monsoon: Dec-Jan-Feb-Mar (4 months) NEM
## Spring transitional inter-monsoon: Apr-May (2 months) STM
## Southwest monsoon: Jun-Jul-Aug-Sep (4 months) SWM
## Fall transitional inter-monsoon: Oct-Nov (2 months) FTM

# Rename time_stamp column to date so openair is usable
df_EC2_temp <- df_EC2
names(df_EC2_temp)[1] <- 'date'

df_EC2_month <- cutData(df_EC2_temp,type='month')

rm(df_EC2_temp)

# To classify data based on months of the monsoon season
monsoon <- ''
for (i in 1:nrow(df_EC2_month)){
  if (is.na(df_EC2_month$month[i])) {
    monsoon[i] <- NA
  }
  else if (df_EC2_month$month[i] == 'December' | df_EC2_month$month[i] == 'January' |
           df_EC2_month$month[i] == 'February' | df_EC2_month$month[i] == 'March') {
    monsoon[i] <- 'NEM'
  } else if (df_EC2_month$month[i] == 'April' | df_EC2_month$month[i] == 'May') {
    monsoon[i] <- 'STM'
  } else if (df_EC2_month$month[i] == 'June' | df_EC2_month$month[i] == 'July' |
             df_EC2_month$month[i] == 'August' | df_EC2_month$month[i] == 'September'){
    monsoon[i] <- 'SWM'
  } else {
    monsoon[i] <- 'FTM'
  }
}
monsoon<-factor(monsoon,levels=c('NEM','STM','SWM','FTM'),ordered=TRUE)
df_EC2_monsoon <- cbind(df_EC2_month,monsoon)
rm(monsoon,df_EC2_month)
