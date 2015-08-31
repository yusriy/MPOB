#Script to filter df_EC dataframe
#####Loading data#####################################################################################
setwd("c:./CO2_eddy/")
df_EC <- read.csv("Data/Result_EDDYPRO/EC_biomet.csv")
#remove column 'X'
df_EC <- df_EC[,-1]

#####Removing observations with:#############################################################
# 1) wind direction between 224 to 244 degrees of CSAT3 sonic anemometer
# 2) -10 < Z.L < 10
# 3) qc = 2

df_EC2 <- df_EC
df_EC2$wind_dir[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
###Wind direction between 224 to 244 degrees of (behind) CSAT3 sonic anemometer###
time_stamp_wd <- df_EC$time_stamp[which(df_EC$wind_dir < 224 | 
                                             df_EC$wind_dir > 244)]
wind_dir_wd <- df_EC$wind_dir[which(df_EC$wind_dir < 224 |
                                          df_EC$wind_dir > 244)]
co2_flux_wd <- df_EC$co2_flux[which(df_EC$wind_dir < 224 |
                                           df_EC$wind_dir > 244)]
qc_co2_wd <- df_EC$qc_co2_flux[which(df_EC$wind_dir < 224 |
                                    df_EC$wind_dir > 244)]
h2o_flux_wd <- df_EC$h2o_flux[which(df_EC$wind_dir < 224 |
                                           df_EC$wind_dir > 244)]
qc_h2o_wd <- df_EC$qc_h2o_flux[which(df_EC$wind_dir < 224 |
                                    df_EC$wind_dir > 244)]
LE_wd <- df_EC$LE[which(df_EC$wind_dir < 224 |
                               df_EC$wind_dir > 244)]
qc_LE_wd <- df_EC$qc_LE[which(df_EC$wind_dir < 224 |
                                   df_EC$wind_dir > 244)]
H_wd <- df_EC$H[which(df_EC$wind_dir < 224 |
                             df_EC$wind_dir > 244)]
qc_H_wd <- df_EC$qc_H[which(df_EC$wind_dir < 224 |
                                  df_EC$wind_dir > 244)]
Z.L_wd <- df_EC$Z.L[which(df_EC$wind_dir < 224 |
                                 df_EC$wind_dir > 244)]

### -10 < Z.L < 10 ###
time_stamp_zl <- time_stamp_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
wind_dir_zl <- wind_dir_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
co2_flux_zl <- co2_flux_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
qc_co2_zl <- qc_co2_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
h2o_flux_zl <- h2o_flux_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
qc_h20_zl <- qc_h2o_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
LE_zl <- LE_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
qc_LE_zl <- LE_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
H_zl <- H_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
qc_H_zl <- H_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
Z.L_zl <- Z.L_wd[which(Z.L_wd < 10 & Z.L_wd> -10)]
#Remove previous temporary values
rm(time_stamp_wd,wind_dir_wd,co2_flux_wd,qc_co2_wd,h2o_flux_wd,qc_h2o_wd,LE_wd,qc_LE_wd,
   H_wd,qc_H_wd,Z.L_wd)

### assign qc = 2 with NA (relevant parameters only)###
#for CO2 flux
co2_flux_ec <- co2_flux_zl
co2_flux_ec[which(qc_co2_zl == 2)] <- NA
#for H2O flux
h2o_flux_ec <- h2o_flux_zl
h2o_flux_ec[which(qc_h20_zl == 2)] <- NA
#for LE
LE_ec <- LE_zl
LE_ec[which(qc_LE_zl == 2)] <- NA
#for H
H_ec <- H_zl
H_ec[which(qc_H_zl == 2)] <- NA
#Remove temporary variables
rm(co2_flux_zl,h2o_flux_zl,LE_zl,H_zl,qc_co2_zl,qc_H_zl,qc_h20_zl,qc_LE_zl)

#####Plotting#####

plot(time_stamp_zl,co2_flux_ec,type='l',ylim=c(-250,100), 
     xlim=c(as.POSIXct('2013-09-11 00:00:00 MYT'),as.POSIXct('2013-10-11 00:00:00 MYT')),
     xlab = 'Time Stamp', ylab = 'CO2 Flux', main = 'Sept 11 2013 - Oct 11 2013')
axis.POSIXct(side=1, at=time_stamp_zl, format="%m-%d-%Y")

#Error i get is as follows:
#Error: protect(): protection stack overflow
#Error: protect(): protection stack overflow
#Graphics error: Plot rendering error

plot(time_stamp_zl,co2_flux_ec,type='l')
