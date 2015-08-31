##### Script to combine data ####
# Author: Yusri Yusup, PhD
# Date: 2014-12-16
# Version: 1.0
#
#

time_stamp <- c(df_2013_SEP$time_stamp,df_2013_OCT$time_stamp,df_2013_NOV$time_stamp,
              df_2013_DIS$time_stamp,df_2014_JAN$time_stamp,df_2014_FEB$time_stamp,
              df_2014_MAR$time_stamp,df_2014_APR$time_stamp,df_2014_MAY$time_stamp,
              df_2014_JUNE$time_stamp,df_2014_JUL$time_stamp,df_2014_AUG$time_stamp,
              df_2014_SEP$time_stamp)

h2o_flux <- c(df_2013_SEP$h2o_flux,df_2013_OCT$h2o_flux,df_2013_NOV$h2o_flux,
              df_2013_DIS$h2o_flux,df_2014_JAN$h2o_flux,df_2014_FEB$h2o_flux,
              df_2014_MAR$h2o_flux,df_2014_APR$h2o_flux,df_2014_MAY$h2o_flux,
              df_2014_JUNE$h2o_flux,df_2014_JUL$h2o_flux,df_2014_AUG$h2o_flux,
              df_2014_SEP$h2o_flux)

co2_flux <- c(df_2013_SEP$co2_flux,df_2013_OCT$co2_flux,df_2013_NOV$co2_flux,
              df_2013_DIS$co2_flux,df_2014_JAN$co2_flux,df_2014_FEB$co2_flux,
              df_2014_MAR$co2_flux,df_2014_APR$co2_flux,df_2014_MAY$co2_flux,
              df_2014_JUNE$co2_flux,df_2014_JUL$co2_flux,df_2014_AUG$co2_flux,
              df_2014_SEP$co2_flux)

LE <- c(df_2013_SEP$LE,df_2013_OCT$LE,df_2013_NOV$LE,
              df_2013_DIS$LE,df_2014_JAN$LE,df_2014_FEB$LE,
              df_2014_MAR$LE,df_2014_APR$LE,df_2014_MAY$LE,
              df_2014_JUNE$LE,df_2014_JUL$LE,df_2014_AUG$LE,
              df_2014_SEP$LE)

H <- c(df_2013_SEP$H,df_2013_OCT$H,df_2013_NOV$H,
        df_2013_DIS$H,df_2014_JAN$H,df_2014_FEB$H,
        df_2014_MAR$H,df_2014_APR$H,df_2014_MAY$H,
        df_2014_JUNE$H,df_2014_JUL$H,df_2014_AUG$H,
        df_2014_SEP$H)

wd <- c(df_2013_SEP$wind_dir,df_2013_OCT$wind_dir,df_2013_NOV$wind_dir,
       df_2013_DIS$wind_dir,df_2014_JAN$wind_dir,df_2014_FEB$wind_dir,
       df_2014_MAR$wind_dir,df_2014_APR$wind_dir,df_2014_MAY$wind_dir,
       df_2014_JUNE$wind_dir,df_2014_JUL$wind_dir,df_2014_AUG$wind_dir,
       df_2014_SEP$wind_dir)

ws <- c(df_2013_SEP$wind_speed,df_2013_OCT$wind_speed,df_2013_NOV$wind_speed,
        df_2013_DIS$wind_speed,df_2014_JAN$wind_speed,df_2014_FEB$wind_speed,
        df_2014_MAR$wind_speed,df_2014_APR$wind_speed,df_2014_MAY$wind_speed,
        df_2014_JUNE$wind_speed,df_2014_JUL$wind_speed,df_2014_AUG$wind_speed,
        df_2014_SEP$wind_speed)

zL <- c(df_2013_SEP$Z.L,df_2013_OCT$Z.L,df_2013_NOV$Z.L,
        df_2013_DIS$Z.L,df_2014_JAN$Z.L,df_2014_FEB$Z.L,
        df_2014_MAR$Z.L,df_2014_APR$Z.L,df_2014_MAY$Z.L,
        df_2014_JUNE$Z.L,df_2014_JUL$Z.L,df_2014_AUG$Z.L,
        df_2014_SEP$Z.L)