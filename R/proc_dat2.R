#### Process MPOB data ##################
#
# This script/function is to manual process MPOB data.
# Note that data frames with 118 and 119 columns have different column names.
# 
#########################################
proc_dat2 <- function(df_name,h_names){
  # Insert insert dataframe into dummy dataframe
  df <- df_name
  
  # Separating into two different data frame types
  
  if (ncol(df) == 118){
    # Removing the first column and first row
    df <- df[,-1]
    df <- df[-1,]
    rownames(df)<-NULL
    
    # Using convert_magic to convert all columns to 'character' first
    df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times = ncol(df))))
    
    # Changing all the '-9999.0' or '-9999' (missing data) to NA
    for (i in 1:length(df)){
      df[i][df[i] == '-9999' | df[i] == '-9999.0'] <- NA
    }
    rm(i)
    
    # Formatting time
    time_stamp <- paste(df$date,df$time)
    
    # Might need to change format of date 1/1/2014 or 2014-1-1
    time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
    df$time <- time_stamp
    df <- df[,c(-1)]
    colnames(df)[1] <-'time_stamp'
    # Remove 'DOY', don't know what it is for...
    df <- df[,c(-2)]
    
    # Changing all relevant columns to factors
    df$daytime <- as.factor(df$daytime)
    df$file_records <- as.factor(df$file_records)
    df$used_records <- as.factor(df$used_records)
    df$qc_Tau <- as.factor(df$qc_Tau)
    df$qc_H <- as.factor(df$qc_H)
    df$qc_LE <- as.factor(df$qc_LE)
    df$qc_co2_flux <- as.factor(df$qc_co2_flux)
    df$qc_h2o_flux <- as.factor(df$qc_h2o_flux)
    df$co2_def_timelag <- as.factor(df$co2_time_lag)
    df$h2o_def_timelag <- as.factor(df$h2o_def_timelag)
    df$spikes <- as.factor(df$spikes)
    df$amplitude_resolution <- as.factor(df$amplitude_resolution)
    df$drop_out <- as.factor(df$drop_out)
    df$absolute_limits <- as.factor(df$absolute_limits)
    df$skweness_kurtosis <- as.factor(df$skweness_kurtosis)
    df$skweness_kurtosis.1 <- as.factor(df$skweness_kurtosis.1)
    df$discontinuities <- as.factor(df$discontinuities)
    df$discontinuities.1 <- as.factor(df$discontinuities.1)
    df$timelag <- as.factor(df$timelag)
    df$timelag.1 <- as.factor(df$timelag.1)
    df$attack_angle <- as.factor(df$attack_angle)
    df$non_steady_wind <- as.factor(df$non_steady_wind)
    df$model <- as.factor(df$model)
    
    # Change all non-factors (or characters) to numeric)
    df <- charactersNumeric(df)
    
    # Change column name of (z-d)/L to Z.L
    colnames(df)[which(colnames(df) == 'X.z.d..L')] <- 'Z.L'
    
    df_name <- df
    
    rm(df,time_stamp)
    
  } else {
    # Removing the first column and first row
    df <- df[,-1]
    df <- df[-1,]
    rownames(df)<-NULL
    
    # Using convert_magic to convert all columns to 'character' first
    df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times = ncol(df))))
    
    # Changing all the '-9999.0' or '-9999' (missing data) to NA
    for (i in 1:length(df)){
      df[i][df[i] == '-9999' | df[i] == '-9999.0'] <- NA
    }
    rm(i)
    
    # Formatting time
    time_stamp <- paste(df$date,df$time)
    
    # Might need to change format of date 1/1/2014 or 2014-1-1
    time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
    df$time <- time_stamp
    df <- df[,c(-1)]
    colnames(df)[1] <-'time_stamp'
    # Remove 'DOY', don't know what it is for...
    df <- df[,c(-2)]
    
    # Changing all relevant columns to factors
    df$daytime <- as.factor(df$daytime)
    df$file_records <- as.factor(df$file_records)
    df$used_records <- as.factor(df$used_records)
    df$qc_Tau <- as.factor(df$qc_Tau)
    df$qc_H <- as.factor(df$qc_H)
    df$qc_LE <- as.factor(df$qc_LE)
    df$qc_co2_flux <- as.factor(df$qc_co2_flux)
    df$qc_h2o_flux <- as.factor(df$qc_h2o_flux)
    df$co2_def_timelag <- as.factor(df$co2_time_lag)
    df$h2o_def_timelag <- as.factor(df$h2o_def_timelag)
    df$spikes_hf <- as.factor(df$spikes_hf)
    df$amplitude_resolution_hf <- as.factor(df$amplitude_resolution_hf)
    df$drop_out_hf <- as.factor(df$drop_out_hf)
    df$absolute_limits_hf <- as.factor(df$absolute_limits_hf)
    df$skewness_kurtosis_hf <- as.factor(df$skewness_kurtosis_hf)
    df$skewness_kurtosis_sf <- as.factor(df$skewness_kurtosis_sf)
    df$discontinuities_hf <- as.factor(df$discontinuities_hf)
    df$discontinuities_sf <- as.factor(df$discontinuities_sf)
    df$timelag_hf <- as.factor(df$timelag_hf)
    df$timelag_sf <- as.factor(df$timelag_sf)
    df$attack_angle_hf <- as.factor(df$attack_angle_hf)
    df$non_steady_wind_hf <- as.factor(df$non_steady_wind_hf)
    df$model <- as.factor(df$model)
    
    # Change all non-factors (or characters) to numeric)
    df <- charactersNumeric(df)
    
    # Change column name of (z-d)/L to Z.L
    colnames(df)[which(colnames(df) == 'X.z.d..L')] <- 'Z.L'
    
    df$ET <- NULL
    
    # Remove 'ET' because the only column different with other data sets
    df_name <- df
    
    
    #rm(df,time_stamp)
  }
  
  names(df_name) <- h_names
  return(df_name)
  
}

# Combine all data frames
#df <- rbind.fill(df_2013_SEP,df_2013_OCT,df_2013_NOV,df_2013_DIS,
#                 df_2014_JAN,df_2014_FEB,df_2014_MAR,df_2014_APR,
#                 df_2014_MAY,df_2014_JUNE,df_2014_JUL,df_2014_AUG,
#                 df_2014_SEP)


