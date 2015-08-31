#Script to create and filter df_new
#####Loading data###########################################################
df_EC <- read.csv("Data/EC_biomet.csv")
#remove column 'X'
df_EC <- df_EC[,-1]
#create df_new
df_new <- df_EC
#Convert time_stamp from "factor" to "POSIXlt"
df_new$time_stamp <- as.POSIXlt(df_new$time_stamp)

#####Removing observations with#############################################
# 1) wind direction between 224 to 244 degrees of CSAT3 sonic anemometer
# 2) -10 < Z.L < 10
# 3) qc = 2

#1)Wind direction between 224 to 244 degrees of (behind) CSAT3 sonic anemometer
df_new$co2_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$qc_co2_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$h2o_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$qc_h2o_flux[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$LE[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$qc_LE[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$H[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$qc_H[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$Z.L[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA
df_new$wind_dir[which(df_EC$wind_dir > 224 & df_EC$wind_dir < 244)] <- NA

#2)Assign values where -10 > Z.L or Z.L > 10 with NA
df_new$co2_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$qc_co2_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$h2o_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$qc_h2o_flux[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$LE[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$qc_LE[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$H[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$qc_H[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA
df_new$Z.L[which(df_EC$Z.L > 10 | df_EC$Z.L < -10)] <- NA

#3)assign qc = 2 with NA (relevant parameters only)
df_new$co2_flux[which(df_EC$qc_co2_flux == 2)] <- NA
df_new$h2o_flux[which(df_EC$qc_h2o_flux == 2)] <- NA
df_new$LE[which(df_EC$qc_LE == 2)] <- NA
df_new$H[which(df_EC$qc_H == 2)] <- NA

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
header_new <- names(df_new)
names(df_dummy) <- header_new

#remove temporary variables
rm(janfeb,janfebr,dummy_col)

#Insert df_dummy into df_EC2 at point of missing timestamp
#Split df_EC2 at point of missing timestamp
df_new_1 <- df_new[1:23092,1:152]
df_new_2 <- df_new[23093:24435,1:152]
row.names(df_new_2)<-NULL
#Combine the data frames at point of missing timestamp
df_new <- rbind(df_new_1,df_dummy,df_new_2)
#remove temporary dataframes
rm(df_new_1,df_new_2,df_dummy)

#####Other missing time stamps###########################################################
#Missing time stamp (NA instead of timestamp) from
#2014-01-01 00:30:00 to 2014-02-01 00:00:00
df_new$time_stamp[5388:6875]<-seq(as.POSIXct('2014-01-01 00:30:00 MYT'),
                                    as.POSIXct('2014-02-01 00:00:00 MYT'),1800)

#Missing time stamp for 2013-12-01 00:30:00pl
timestamp1 <- as.POSIXct('2013-12-01 00:30:00')
timestamp1 <- as.data.frame(timestamp1)
dummy_row <- matrix(rep(NA,151),nrow=1,ncol=151)
dummy_row <- as.data.frame(dummy_row)
dummy_12_01 <- cbind(timestamp1,dummy_row)
names(dummy_12_01) <- header_new
df_new_3 <- df_new[1:3900,1:152]
df_new_4 <- df_new[3901:25219,1:152]
row.names(df_new_4)<-NULL
df_new <- rbind(df_new_3,dummy_12_01,df_new_4)
rm(df_new_3,df_new_4,dummy_12_01,timestamp1)

#Missing time stamp for 2014-05-01 00:00:00
timestamp2 <- as.POSIXct('2014-05-01 00:00:00')
timestamp2 <- as.data.frame(timestamp2)
dummy_05_01 <- cbind(timestamp2,dummy_row)
names(dummy_05_01) <- header_new
df_new_5 <- df_new[1:11148,1:152]
df_new_6 <- df_new[11149:25220,1:152]
row.names(df_new_6)<-NULL
df_new <- rbind(df_new_5,dummy_05_01,df_new_6)
rm(df_new_5,df_new_6,dummy_05_01,timestamp2,dummy_row)

#Missing time stamp from 2014-11-21 04:00:00 till 2014-12-01 23:00:00
timestamp3 <- seq(as.POSIXct('2014-11-21 04:00:00 MYT'),
                  as.POSIXct('2014-12-01 23:00:00 MYT'),1800)
timestamp3 <- as.data.frame(timestamp3)
dummy_col2 <- matrix(rep(NA,78369),nrow=519,ncol=151)
dummy_col2 <- as.data.frame(dummy_col2)
dummy_11_21 <- cbind(timestamp3,dummy_col2)
names(dummy_11_21) <- header_new
df_new_7 <- df_new[1:20948,1:152]
df_new_8 <- df_new[20949:25221,1:152]
row.names(df_new_8)<-NULL
df_new <- rbind(df_new_7,dummy_11_21,df_new_8)
rm(df_new_7,df_new_8,dummy_11_21,dummy_col2,timestamp3,header_new)

#Remove extra time stamp at 2014-04-01 00:00:00 [9708]
df_new <- df_new[-c(9708),]
row.names(df_new) <- NULL

#timecheck<-as.data.frame(seq(as.POSIXct('2013-09-10 18:30:00 MYT'),
                  #as.POSIXct('2014-09-30 23:30:00 MYT'),1800))

#####Filter data#############################################################
#Spike removal by visual inspection
df_new$co2_flux[c(1368,1847,2069,2257,2843,5568,9058,9061,9067,9069,10375,10392,11213,12000,12057,
                  12422,12423,12462,12554,12702,12915,13872,14000,14086,14162,14253,15151,15247,
                  15831,16030,16124,16176,16511,16605,17787,17994,17996,18188,18189,18230,18233,
                  18332,18336,18650,18653,18912,18913,18914,19020,19038,19351,19390,19687,19724,
                  19972,19975,20304,20305,20367,20370,20439,20440,20498,20750,20752,20777,21584,
                  22075,22162,22164,23118,23177,23185,23231,23238,23275,23282,23375,23429,23560,
                  23564,24559,24570)] <- NA

df_new$h2o_flux[c(1368,1847,2257,3161,3452,4656,5814,7757,7864,9068,9597,10944,11273,11617,
                  12057,12422,12423,12426,13194,14253,16176,16605,17660,17996,18189,18331,
                  18336,18912,18915,19022,19023,19094,19106,19687,19765,19971,20213,20304,
                  20440,20498,22075,22162,22164,22312,23177,23272,23281,23282,24559,24570)] <- NA

df_new$LE[c(1368,1847,2257,3161,3452,4656,5814,7757,7864,9068,9597,10944,11273,11617,
            12057,12422,12423,12426,13194,14253,16176,16605,17660,17996,18189,18331,
            18336,18912,18915,19022,19023,19094,19106,19687,19765,19971,20213,20304,
            20440,20498,22075,22162,22164,22312,23177,23272,23281,23282,24559,24570)] <- NA

df_new$H[c(1844,1847,9595,11228,11466,14162,14903,15533,15979,16685,18187,18914,23177)] <- NA

# Biomet data (slow response)

#Air temperature

# Ambient temperature at levels 1 to 5 (low to high)
#Change Kelvin to Celcius
df_new$Ta_1_1_1 <- df_new$Ta_1_1_1 - 273.15
df_new$Ta_2_1_1 <- df_new$Ta_2_1_1 - 273.15
df_new$Ta_3_1_1 <- df_new$Ta_3_1_1 - 273.15
df_new$Ta_4_1_1 <- df_new$Ta_4_1_1 - 273.15
df_new$Ta_5_1_1 <- df_new$Ta_5_1_1 - 273.15

#Soil temperature
#Change Kelvin to Celcius
df_new$Ts_1_1_1 <- df_new$Ts_1_1_1 - 273.15
df_new$Ts_2_1_1 <- df_new$Ts_2_1_1 - 273.15
df_new$Ts_3_1_1 <- df_new$Ts_3_1_1 - 273.15

# Rain gauge
# m (meter) to mm
df_new$Prain_1_1_1 <- df_new$Prain_1_1_1*1000

# Remove all probelmatic data and spikes after visual inspection

# Air Temperature

df_new$Ta_1_1_1[which(df_new$Ta_1_1_1 < 16)] <- NA
df_new$Ta_2_1_1[which(df_new$Ta_2_1_1 < 16)] <- NA
df_new$Ta_3_1_1[which(df_new$Ta_3_1_1 < 16)] <- NA
df_new$Ta_4_1_1[which(df_new$Ta_3_1_1 < 16)] <- NA
df_new$Ta_5_1_1[which(df_new$Ta_3_1_1 < 16)] <- NA

# RH

df_new$RH_1_1_1[which(df_new$RH_1_1_1 < 40 | df_new$RH_1_1_1 > 100)] <- NA
df_new$RH_2_1_1[which(df_new$RH_2_1_1 < 40 | df_new$RH_2_1_1 > 100)] <- NA
df_new$RH_3_1_1[which(df_new$RH_3_1_1 < 40 | df_new$RH_3_1_1 > 100)] <- NA
df_new$RH_4_1_1[which(df_new$RH_4_1_1 < 40 | df_new$RH_4_1_1 > 100)] <- NA
df_new$RH_5_1_1[which(df_new$RH_5_1_1 < 40 | df_new$RH_5_1_1 > 100)] <- NA

# Turn RH = 100% to NA because constant at 100
df_new$RH_1_1_1 [c(20427,20428,20466:20478,20520:20525,20620,20622,20854,20855,20856,
                   20857,20858,20859,20860,21482,21483,21484,21485,21486,21628,21629,21630,23163,
                   23164,23165,23166,23205,23206,23207,23208,23209,23210,23211,23212,23213,23214,
                   23215,23259,23260,23261,23262,23301,23302,23303,23304,23305,23306,23307,23308,
                   23309,23310,23311,23312,23342,23343,23344,23345,23346,23347,23348,23349,23350,
                   23351,23352,23353,23354,23355,23356,23357,23401,23402,23403,23404,23405)] <- NA

# Net radiation (Rn)
df_new$Rn_1_1_1[which(df_new$Rn_1_1_1 > 900 | df_new$Rn_1_1_1 < -100)] <- NA


# PAR (PPFD)

df_new$PPFD_1_1_1[which(df_new$PPFD_1_1_1 > 2000 | df_new$PPFD_1_1_1 < 0)] <- NA
df_new$PPFD_2_1_1[which(df_new$PPFD_2_1_1 > 2000 | df_new$PPFD_2_1_1 < 0)] <- NA
df_new$PPFD_3_1_1[which(df_new$PPFD_3_1_1 > 2000 | df_new$PPFD_3_1_1 < 0)] <- NA

# Rain

df_new$Prain_1_1_1[which(df_new$Prain_1_1_1 > 40 | df_new$Prain_1_1_1 <0)] <- NA

# Windspeed (RM Young)

df_new$WS_1_1_1[which(df_new$WS_1_1_1 > 15.4 | df_new$WS_1_1_1 <0)] <- NA

# Wind direction

df_new$WD_1_1_1[which(df_new$WD_1_1_1 <0 | df_new$WD_1_1_1 >360)] <- NA

#Soil water content

df_new$SWC_1_1_1[which(df_new$SWC_1_1_1 <0 | df_new$SWC_1_1_1 > 1)] <- NA
df_new$SWC_2_1_1[which(df_new$SWC_2_1_1 <0 | df_new$SWC_2_1_1 > 1)] <- NA
df_new$SWC_3_1_1[which(df_new$SWC_3_1_1 <0 | df_new$SWC_3_1_1 > 1)] <- NA

# Soil heat flux

df_new$SHF_1_1_1[which(df_new$SHF_1_1_1 < -200 | df_new$SHF_1_1_1 > 300)] <- NA
df_new$SHF_2_1_1[which(df_new$SHF_2_1_1 < -200 | df_new$SHF_2_1_1 > 300)] <- NA
df_new$SHF_3_1_1[which(df_new$SHF_3_1_1 < -200 | df_new$SHF_3_1_1 > 300)] <- NA

# Soil Temperature

df_new$Ts_1_1_1[which(df_new$Ts_1_1_1 <23 | df_new$Ts_1_1_1 >32 )] <- NA
df_new$Ts_2_1_1[which(df_new$Ts_2_1_1 <23 | df_new$Ts_2_1_1 >32 )] <- NA
df_new$Ts_3_1_1[which(df_new$Ts_3_1_1 <23 | df_new$Ts_3_1_1 >32 )] <- NA

######Storage calculation functions######################################################################
# Function to calculate absolute humidity (kg/m3) using the 
# Magnus-Tetens equation
# T = temperature in deg C
# RH = relative humidity in %

abs_humidity <- function(temp,RH){
  hum <- (6.112 * exp(17.67*temp/(temp+243.5)) * RH * 2.1674)/(273.15 + temp)
  ans <- hum/1000 # To change to kg/m3 from g/m3
  return(ans)
}

# Calculate rho * c_p of actual air (dry and moist air)
# temp = temperature in deg C
# rh = relative humidity in %
# P = atmospheric pressure in kPa
# Taken from Matlab script return by James from LI-COR.

rho_cp <- function(rh,temp,P){
  # Since, we will using air_pressure from EddyPro output
  # we have to change the units to kPa
  P <- P / 1000 # [kPa]
  R <- 8.31451 #[N m mol-1 K-1]
  # Need to calculate c_p of moist air
  c_p_moist <- 1859 + (0.13 * rh) + (temp*(0.193 + 0.00569*rh)) +
    ((temp^2)*(0.001 + 0.0005*rh)) # [J kg-1 C-1]
  # Calculate c_p of dry air
  c_p_dry <- 1005 + ((temp + 23.12)/3364) # [J kg-1 C-1]
  # Calculate vapor pressure of saturated air
  e_s1 <- 0.6112 * exp((17.62 * temp)/(243.12 + temp)) # kPa
  # Apply pressure correction on e_s
  e_s <- e_s1 * (1.00072 + (P * (3.2 + 0.00059 * temp^2)/100000)) #kPa
  # Calculate vapor pressure of unsaturated air
  e_a <- (rh * e_s)/100 # kPa
  # Calculate density of dry air
  rho_a <- 29.002 * (P * 1000)/(R * (temp + 273.15)) # [g m-3]
  # Calculate density of moist air
  rho_v <- 18.01 * (e_a * 1000)/(R * (temp + 273.15)) # [g m-3]
  
  # Calculate final rho * c_p
  rhocp <- ((c_p_moist*rho_v)/1000) + ((c_p_dry*rho_a)/1000) # [J m-3 C-1]
  
  return(rhocp)
  
}

# Integrating using the trapezium area rule

trapezium_intg <- function(heights,x1,x2,x3,x4,x5){
  area <- (0.5 * (heights[2] - heights[1]) * (x1 + x2)) +
    (0.5 * (heights[3] - heights[2]) * (x2 + x3)) +
    (0.5 * (heights[4] - heights[3]) * (x3 + x4)) +
    (0.5 * (heights[5] - heights[4]) * (x5 + x4))
  
  return(area)
}

#### Calculating storage H in canopy #################################

heights <- c(2,5,10,15,30.65) #Levels 1: 2 m, 2: 5 m, 3: 10 m, 4: 15 m, 5: 30.65 m
# 5 heights are used here unlike in James' (LI-COR) script which uses
# only 4 heights

# Calculating rho * cp for each level
rhocp1 <- rho_cp(df_new$RH_1_1_1,df_new$Ta_1_1_1,df_new$air_pressure)
rhocp2 <- rho_cp(df_new$RH_2_1_1,df_new$Ta_2_1_1,df_new$air_pressure)
rhocp3 <- rho_cp(df_new$RH_3_1_1,df_new$Ta_3_1_1,df_new$air_pressure)
rhocp4 <- rho_cp(df_new$RH_4_1_1,df_new$Ta_4_1_1,df_new$air_pressure)
rhocp5 <- rho_cp(df_new$RH_5_1_1,df_new$Ta_5_1_1,df_new$air_pressure)

# Calculating the difference of rho * c_p * (T2 - T1) in time
# Level 1, 2 m
rho_cp_dT1 <- numeric()
#rho_cp_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(rhocp1)){
  rho_cp_dT1[i] <- ((rhocp1[i]*df_new$Ta_1_1_1[i]) - 
                      (rhocp1[i-1]*df_new$Ta_1_1_1[i-1]))/(30 * 60)
}
# Level 2, 5 m
rho_cp_dT2 <- numeric()
#rho_cp_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(rhocp2)){
  rho_cp_dT2[i] <- ((rhocp2[i]*df_new$Ta_2_1_1[i]) - 
                      (rhocp2[i-1]*df_new$Ta_2_1_1[i-1]))/(30 * 60)
}

# Level 3, 10 m
rho_cp_dT3 <- numeric()
#rho_cp_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(rhocp3)){
  rho_cp_dT3[i] <- ((rhocp3[i]*df_new$Ta_3_1_1[i]) - 
                      (rhocp3[i-1]*df_new$Ta_3_1_1[i-1]))/(30 * 60)
}

# Level 4, 15 m
rho_cp_dT4 <- numeric()
#rho_cp_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(rhocp4)){
  rho_cp_dT4[i] <- ((rhocp4[i]*df_new$Ta_4_1_1[i]) - 
                      (rhocp4[i-1]*df_new$Ta_4_1_1[i-1]))/(30 * 60)
}

# Level 5, 30.65 or 30 m
rho_cp_dT5 <- numeric()
#rho_cp_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(rhocp5)){
  rho_cp_dT5[i] <- ((rhocp5[i]*df_new$Ta_5_1_1[i]) - 
                      (rhocp5[i-1]*df_new$Ta_5_1_1[i-1]))/(30 * 60)
}

# Integrating using the trapezium area rule
H_stor <- numeric()
for (i in 1:nrow(df_new)){
  H_stor[i] <- trapezium_intg(heights,rho_cp_dT1[i],rho_cp_dT2[i],rho_cp_dT3[i],
                              rho_cp_dT4[i],rho_cp_dT5[i])
}

# Adding to df_new
df_new <- cbind(df_new,H_stor)

rm(rho_cp_dT1,rho_cp_dT2,rho_cp_dT3,rho_cp_dT4,
   rho_cp_dT5,rhocp1,rhocp2,rhocp3,
   rhocp4,rhocp5,H_stor)

#### Calculating storage LE in canopy ####

# Calculating absolute humidity from RH
# Level 1, 2 m
hum1 <- numeric()
for (i in 1:nrow(df_new)){
  hum1[i] <- abs_humidity(df_new$Ta_1_1_1[i],df_new$RH_1_1_1[i])
}
# Level 2, 5 m
hum2 <- numeric()
for (i in 1:nrow(df_new)){
  hum2[i] <- abs_humidity(df_new$Ta_2_1_1[i],df_new$RH_2_1_1[i])
}
# Level 3, 10 m
hum3 <- numeric()
for (i in 1:nrow(df_new)){
  hum3[i] <- abs_humidity(df_new$Ta_3_1_1[i],df_new$RH_3_1_1[i])
}
# Level 4, 15 m
hum4 <- numeric()
for (i in 1:nrow(df_new)){
  hum4[i] <- abs_humidity(df_new$Ta_4_1_1[i],df_new$RH_4_1_1[i])
}
# Level 5, 30.65 m
hum5 <- numeric()
for (i in 1:nrow(df_new)){
  hum5[i] <- abs_humidity(df_new$Ta_5_1_1[i],df_new$RH_5_1_1[i])
}
# Adding to df_new
df_new <- cbind(df_new,hum1,hum2,hum3,hum4,hum5)
rm(hum1,hum2,hum3,hum4,hum5)

# Calculating storage LE in canopy
L_v = 2540000 # [J/kg]

# Level 1, 2 m
diff_hum1 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$hum1)){
  diff_hum1[i] <- L_v * (df_new$hum1[i] - df_new$hum1[i-1])/(30 * 60)
}

# Level 2, 5 m
diff_hum2 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$hum2)){
  diff_hum2[i] <- L_v * (df_new$hum2[i] - df_new$hum2[i-1])/(30 * 60)
}

# Level 3, 10 m
diff_hum3 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$hum3)){
  diff_hum3[i] <- L_v * (df_new$hum3[i] - df_new$hum3[i-1])/(30 * 60)
}

# Level 4, 15 m
diff_hum4 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$hum4)){
  diff_hum4[i] <- L_v * (df_new$hum4[i] - df_new$hum4[i-1])/(30 * 60)
}

# Level 5, 30.65 or 30 m
diff_hum5 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$hum5)){
  diff_hum5[i] <- L_v * (df_new$hum5[i] - df_new$hum5[i-1])/(30 * 60)
}

# Integrating using the trapezium area rule
LE_stor <- numeric()
for (i in 1:nrow(df_new)){
  LE_stor[i] <- trapezium_intg(heights,diff_hum1[i],diff_hum2[i],diff_hum3[i],
                               diff_hum4[i],diff_hum5[i])
}

# Adding to df_new
df_new <- cbind(df_new,LE_stor)
rm(LE_stor,L_v,diff_hum1,diff_hum2,diff_hum3,diff_hum4,diff_hum5,heights,i)

#### Calculate soil storage#######################################################
#Averaging values
#soil water content (m3/m3)
swc_avg <- rowMeans(df_new[,c(139,140,141)],na.rm=TRUE)
swc_avg <- as.data.frame(swc_avg)

#soil heat flux(W/m2)
shf_avg <- rowMeans(df_new[,c(142,143,144)],na.rm=TRUE)
shf_avg <- as.data.frame(shf_avg)

#soil temperature(c)
stemp_avg <- rowMeans(df_new[,c(145,146,147)],na.rm=TRUE)
stemp_avg <- as.data.frame(stemp_avg)

#combine data frames
df_new <- cbind(df_new,swc_avg,shf_avg,stemp_avg)
#remove temporary data
rm(swc_avg,shf_avg,stemp_avg)

#Heat storage from depth 0 to heat flux sensor depth
#Cs = (rho)b*Cd + (Theta)v*(rho)w*Cw
#where Cs = heat capacity of moist soil (J m-3 C-1)
# (rho)b = soil bulk density (for rengam series or typic paleudult; around 1400 kg m-3)
#(rho)w = density of water at 26.11C (average soil temperature, 996.76 kg m-3)
# cd = heat capacity of dry mineral soil (890 J kg-1 C-1; from Oliphant et al., 2004)
#(theta)v,tv = soil water content (m3 water /m-3 soil)
# Cw = heat capacity of water (4181.3 J kg-1 C-1)

# First term
# dry soil capacity = 1400*890 = 1246000(J m-3 C-1) assuming values are constant throughout
firstterm <- matrix(rep(1246000,25739),nrow=25739,ncol=1)

#second term
tv <- df_new$swc_avg 
secondterm <- tv*996.76*4181.3 #(J m-3 C-1)

#adding both terms
Cs <- firstterm + secondterm
rm(firstterm,tv,secondterm)

#Calculating soil temperature difference across each 30min
soil_dT <- numeric()
#soil_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$stemp_avg)){
  soil_dT[i] <- (df_new$stemp_avg[i] - df_new$stemp_avg[i-1])
}

#Calculating soil storage
#soil_stor = (soil_dT*Cs*d)/t (W m-2)
#where d = depth of the soil heat flux sensors (0.05m)
# t = time period (30min)
soil_stor <- (soil_dT*Cs*0.05)/(30*60)
#combine to main data frame
df_new <- cbind(df_new,soil_stor)
rm(soil_dT,Cs,soil_stor)
rm(df_EC)
