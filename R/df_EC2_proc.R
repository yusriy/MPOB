#Script to create and filter df_EC2
#####Loading data###########################################################
#df_EC <- read.csv("Data/Result_EDDYPRO/EC_biomet.csv")
#remove column 'X'
#df_EC <- df_EC[,-1]
#create df_EC2
df_EC2 <- df_EC
#Convert time_stamp from "factor" to "POSIXlt"
df_EC2$time_stamp <- as.POSIXlt(df_EC2$time_stamp)

#####Removing observations with#############################################
# 1) wind direction between 224 to 244 degrees of CSAT3 sonic anemometer
# 2) -10 < Z.L < 10
# 3) qc = 2

#1)Wind direction between 224 to 244 degrees of (behind) CSAT3 sonic anemometer
df_EC2$co2_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$qc_co2_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$h2o_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$qc_h2o_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$LE[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$qc_LE[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$H[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$qc_H[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$Z.L[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_EC2$wind_dir[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA

#2)Assign values where -10 > Z.L or Z.L > 10 with NA
df_EC2$co2_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$qc_co2_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$h2o_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$qc_h2o_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$LE[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$qc_LE[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$H[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$qc_H[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_EC2$Z.L[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA

#3)assign qc = 2 with NA (relevant parameters only)
df_EC2$co2_flux[which(df_EC$qc_co2_flux == 2)] <- NA
df_EC2$h2o_flux[which(df_EC$qc_h2o_flux == 2)] <- NA
df_EC2$LE[which(df_EC$qc_LE == 2)] <- NA
df_EC2$H[which(df_EC$qc_H == 2)] <- NA

#####Create NA data frame to fill up missing time_stamp from################
#2015-01-15 16:30:00 till 2015-02-01 00:00:00
#Create dummy time_stamp column
janfeb <- matrix(seq(as.POSIXct('2015-01-15 16:30:00 MYT'),
                     as.POSIXct('2015-02-01 00:00:00 MYT'),1800)
                 ,nrow=784, ncol=1)
janfebr <- as.POSIXlt(janfeb,origin = "1970-01-01")
janfebr <- as.data.frame(janfebr)

#Create dummy variables columns filled with NA
dummy_col <- matrix(rep(NA,118384),nrow=784,ncol=151)
dummy_col <- as.data.frame(dummy_col)

#Combine date column with dummy variables
df_dummy <- cbind(janfebr,dummy_col)

#giving df_dummy header names
header_EC2 <- names(df_EC2)
names(df_dummy) <- header_EC2

#remove temporary variables
rm(janfeb,janfebr,dummy_col,header_EC2)

#####Insert df_dummy into df_EC2 at point of missing timestamp###############
#Split df_EC2 at point of missing timestamp
df_EC2_1 <- df_EC2[1:23092,1:152]
df_EC2_2 <- df_EC2[23093:24435,1:152]
row.names(df_EC2_2)<-NULL
#Combine the data frames at point of missing timestamp
df_ECnew <- rbind(df_EC2_1,df_dummy,df_EC2_2)
row.names(df_ECnew)<-NULL
#remove temporary dataframes
rm(df_EC2_1,df_EC2_2,df_dummy)
