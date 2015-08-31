# Script to import data from the Biomet datalogger

df_biomet_2013_09 <- read.csv('data/Result_EDDYPRO/2013/Result_SEP_2013/eddypro_EC_KERATONG_biomet_2013-12-10T110736.csv')
time_stamp <- paste(df_biomet_2013_09$date,df_biomet_2013_09$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2013_09 <- df_biomet_2013_09[,c(-1,-2)]
df_biomet_2013_09 <- cbind(time_stamp, df_biomet_2013_09)
df_biomet_2013_09 <- na.omit(df_biomet_2013_09)

df_biomet_2013_10 <- read.csv('data/Result_EDDYPRO/2013/Result_OCT_2013/eddypro_EC_KERATONG_biomet_2013-12-10T151209.csv')
time_stamp <- paste(df_biomet_2013_10$date,df_biomet_2013_10$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2013_10 <- df_biomet_2013_10[,c(-1,-2)]
df_biomet_2013_10 <- cbind(time_stamp, df_biomet_2013_10)

df_biomet_2013_11 <- read.csv('data/Result_EDDYPRO/2013/Result_NOV_2013/eddypro_EC_KERATONG_biomet_2013-12-10T164319.csv')
# Get a sample header names from the previous import
header_biomet <- names(df_biomet_2013_11)
time_stamp <- paste(df_biomet_2013_11$date,df_biomet_2013_11$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2013_11 <- df_biomet_2013_11[,c(-1,-2)]
df_biomet_2013_11 <- cbind(time_stamp, df_biomet_2013_11)

df_biomet_2013_12 <- read.csv('data/Result_EDDYPRO/2013/Result_DIS_2013/eddypro_KERATONG_EC_biomet_2015-03-30T151034.csv',skip=1)
names(df_biomet_2013_12) <- header_biomet
time_stamp <- paste(df_biomet_2013_12$date,df_biomet_2013_12$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2013_12 <- df_biomet_2013_12[,c(-1,-2)]
df_biomet_2013_12 <- cbind(time_stamp, df_biomet_2013_12)

df_biomet_2014_01 <- read.csv('data/Result_EDDYPRO/2014/Result_JAN_2014/eddypro_Ec_Keratong_biomet_2014-03-24T111934.csv')
time_stamp <- paste(df_biomet_2014_01$date,df_biomet_2014_01$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_01 <- df_biomet_2014_01[,c(-1,-2)]
df_biomet_2014_01 <- cbind(time_stamp, df_biomet_2014_01)

df_biomet_2014_02 <- read.csv('data/Result_EDDYPRO/2014/Result_FEB_2014/eddypro_Ec_Ketarong_feb_2014_biomet_2014-03-24T150740.csv')
time_stamp <- paste(df_biomet_2014_02$date,df_biomet_2014_02$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_02 <- df_biomet_2014_02[,c(-1,-2)]
df_biomet_2014_02 <- cbind(time_stamp, df_biomet_2014_02)

df_biomet_2014_03 <- read.csv('data/Result_EDDYPRO/2014/Result_MAR_2014/eddypro_EC_Keratong_biomet_2014-04-23T104313.csv')
time_stamp <- paste(df_biomet_2014_03$date,df_biomet_2014_03$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_03 <- df_biomet_2014_03[,c(-1,-2)]
df_biomet_2014_03 <- cbind(time_stamp, df_biomet_2014_03)

df_biomet_2014_04 <- read.csv('data/Result_EDDYPRO/2014/Result_APR_2014/eddypro_keratong_ec_biomet_2014-05-21T114702.csv',skip=1)
names(df_biomet_2014_04) <- header_biomet
time_stamp <- paste(df_biomet_2014_04$date,df_biomet_2014_04$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_04 <- df_biomet_2014_04[,c(-1,-2)]
df_biomet_2014_04 <- cbind(time_stamp, df_biomet_2014_04)

df_biomet_2014_05 <- read.csv('data/Result_EDDYPRO/2014/Result_MAY_2014/eddypro_EC_KERATONG_biomet_2014-08-12T172645.csv',skip=1)
names(df_biomet_2014_05) <- header_biomet
time_stamp <- paste(df_biomet_2014_05$date,df_biomet_2014_05$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_05 <- df_biomet_2014_05[,c(-1,-2)]
df_biomet_2014_05 <- cbind(time_stamp, df_biomet_2014_05)


df_biomet_2014_06 <- read.csv('data/Result_EDDYPRO/2014/Result_JUNE_2014/eddypro_EC_KERATONG_biomet_2014-08-17T180620.csv',skip=1)
names(df_biomet_2014_06) <- header_biomet
time_stamp <- paste(df_biomet_2014_06$date,df_biomet_2014_06$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_06 <- df_biomet_2014_06[,c(-1,-2)]
df_biomet_2014_06 <- cbind(time_stamp, df_biomet_2014_06)

df_biomet_2014_07 <- read.csv('data/Result_EDDYPRO/2014/Result_JUL_2014/eddypro_EC_KERATONG_biomet_2014-08-17T192744.csv',skip=1)
names(df_biomet_2014_07) <- header_biomet
time_stamp <- paste(df_biomet_2014_07$date,df_biomet_2014_07$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_07 <- df_biomet_2014_07[,c(-1,-2)]
df_biomet_2014_07 <- cbind(time_stamp, df_biomet_2014_07)

df_biomet_2014_08 <- read.csv('data/Result_EDDYPRO/2014/Result_AUG_2014/eddypro_EC_KERATONG_biomet_2014-09-17T230017.csv',skip=1)
names(df_biomet_2014_08) <- header_biomet
time_stamp <- paste(df_biomet_2014_08$date,df_biomet_2014_08$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_08 <- df_biomet_2014_08[,c(-1,-2)]
df_biomet_2014_08 <- cbind(time_stamp, df_biomet_2014_08)

# Missing biomet data for Sep 2014
header_biomet1 <- names(df_biomet_2014_08)
df_biomet_2014_09 <- matrix(NA, ncol=38,nrow=1440)
df_biomet_2014_09 <- as.data.frame(df_biomet_2014_09)
names(df_biomet_2014_09) <- header_biomet1

df_biomet_2014_10 <- read.csv('data/Result_EDDYPRO/2014/Result_OCT_2014/RESULT_EP_OCT_2014/eddypro_EC_KERATONG_biomet_2015-03-11T161708.csv',skip=1)
names(df_biomet_2014_10) <- header_biomet
time_stamp <- paste(df_biomet_2014_10$date,df_biomet_2014_10$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_10 <- df_biomet_2014_10[,c(-1,-2)]
df_biomet_2014_10 <- cbind(time_stamp, df_biomet_2014_10)

df_biomet_2014_10 <- read.csv('data/Result_EDDYPRO/2014/Result_OCT_2014/RESULT_EP_OCT_2014/eddypro_EC_KERATONG_biomet_2015-03-11T161708.csv',skip=1)
names(df_biomet_2014_10) <- header_biomet
time_stamp <- paste(df_biomet_2014_10$date,df_biomet_2014_10$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_10 <- df_biomet_2014_10[,c(-1,-2)]
df_biomet_2014_10 <- cbind(time_stamp, df_biomet_2014_10)

df_biomet_2014_11 <- read.csv('data/Result_EDDYPRO/2014/Result_NOV_2014/RESULT_EP_NOV_2014/eddypro_EC_KERATONG_biomet_2015-03-12T120520.csv',skip=1)
names(df_biomet_2014_11) <- header_biomet
time_stamp <- paste(df_biomet_2014_11$date,df_biomet_2014_11$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_11 <- df_biomet_2014_11[,c(-1,-2)]
df_biomet_2014_11 <- cbind(time_stamp, df_biomet_2014_11)

df_biomet_2014_12 <- read.csv('data/Result_EDDYPRO/2014/Result_DEC_2014/RESULT_EP_DEC_2014/eddypro_EC_KERATONG_biomet_2015-03-12T132332.csv',skip=1)
names(df_biomet_2014_12) <- header_biomet
time_stamp <- paste(df_biomet_2014_12$date,df_biomet_2014_12$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2014_12 <- df_biomet_2014_12[,c(-1,-2)]
df_biomet_2014_12 <- cbind(time_stamp, df_biomet_2014_12)

df_biomet_2015_01 <- read.csv('data/Result_EDDYPRO/2015/Result_JAN_2015/RESULT_EP_JAN 2015/eddypro_KERATONG_EC_biomet_2015-03-22T024201.csv',skip=1)
names(df_biomet_2015_01) <- header_biomet
time_stamp <- paste(df_biomet_2015_01$date,df_biomet_2015_01$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2015_01 <- df_biomet_2015_01[,c(-1,-2)]
df_biomet_2015_01 <- cbind(time_stamp, df_biomet_2015_01)

df_biomet_2015_02 <- read.csv('data/Result_EDDYPRO/2015/Result_FEB_2015/RESULT_EP_FEB_2015/eddypro_KERATONG_EC_biomet_2015-03-23T171818.csv',skip=1)
names(df_biomet_2015_02) <- header_biomet
time_stamp <- paste(df_biomet_2015_02$date,df_biomet_2015_02$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_biomet_2015_02 <- df_biomet_2015_02[,c(-1,-2)]
df_biomet_2015_02 <- cbind(time_stamp, df_biomet_2015_02)

# Combining all Biomet dataframes
df_biomet <- rbind(df_biomet_2013_09,df_biomet_2013_10,df_biomet_2013_11,
                   df_biomet_2013_12,df_biomet_2014_01,df_biomet_2014_02,
                   df_biomet_2014_03,df_biomet_2014_04,df_biomet_2014_05,
                   df_biomet_2014_06,df_biomet_2014_07,df_biomet_2014_08,
                   df_biomet_2014_09,df_biomet_2014_10,df_biomet_2014_11,
                   df_biomet_2014_12,df_biomet_2015_01,df_biomet_2015_02)
row.names(df_biomet) <- NULL

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 2:length(df_biomet)){ # starts at index 2 to exclude time_stamp
  df_biomet[i][df_biomet[i] == '-9999' | df_biomet[i] == '-9999.0'] <- NA
}
rm(i)

rm(df_biomet_2013_09,df_biomet_2013_10,df_biomet_2013_11,
   df_biomet_2013_12,df_biomet_2014_01,df_biomet_2014_02,
   df_biomet_2014_03,df_biomet_2014_04,df_biomet_2014_05,
   df_biomet_2014_06,df_biomet_2014_07,df_biomet_2014_08,
   df_biomet_2014_09,df_biomet_2014_10,df_biomet_2014_11,
   df_biomet_2014_12,df_biomet_2015_01,df_biomet_2015_02)
rm(header_biomet,header_biomet1,time_stamp)











