
# Loading libraries
library(openair)

# Separating the data for footprint analysis (unstable and stable)
angle_unstable<-data.frame(df_EC2$LE[which(df_EC2$Z.L < -0.05)],
                           df_EC2$x_90.[which(df_EC2$Z.L < -0.05)],
                           df_EC2$wind_dir[which(df_EC2$Z.L < -0.05)],
                           df_EC2$x_peak[which(df_EC2$Z.L < -0.05)],
                           df_EC2$co2_flux[which(df_EC2$Z.L < -0.05)],
                           df_EC2$H[which(df_EC2$Z.L < -0.05)],
                           df_EC2$Z.L[which(df_EC2$Z.L < -0.05)],
                           df_EC2$wind_speed[which(df_EC2$Z.L < -0.05)])
angle_stable<-data.frame(df_EC2$LE[which(df_EC2$Z.L > 0.05)],
                         df_EC2$x_90.[which(df_EC2$Z.L > 0.05)],
                         df_EC2$wind_dir[which(df_EC2$Z.L > 0.05)],
                         df_EC2$x_peak[which(df_EC2$Z.L > 0.05)],
                         df_EC2$co2_flux[which(df_EC2$Z.L > 0.05)],
                         df_EC2$H[which(df_EC2$Z.L > 0.05)],
                         df_EC2$Z.L[which(df_EC2$Z.L > 0.05)],
                         df_EC2$wind_speed[which(df_EC2$Z.L > 0.05)])
angle_neutral<-data.frame(df_EC2$LE[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$x_90.[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$wind_dir[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$x_peak[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$co2_flux[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$H[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$Z.L[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)],
                          df_EC2$wind_speed[which(df_EC2$Z.L <= 0.05 & df_EC2$Z.L >= -0.05)])

names(angle_unstable)<-c('LE','Distance','wd','Peak','co2','H','Z.L','ws')
names(angle_stable)<-c('LE','Distance','wd','Peak','co2','H','Z.L','ws')
names(angle_neutral)<-c('LE','Distance','wd','Peak','co2','H','Z.L','ws')

## Results of maximum 90% footprint
# Unstable under 2000 m
unst <- angle_unstable$Distance[which(angle_unstable$Distance < 2000.0)]
angle_unstable[which(angle_unstable$Distance == unst[which.max(unst)]),]
rm(unst)

# Neutral under 2000 m
angle_neutral[which.max(which(angle_neutral$Distance < 2000.0)),]
# Stable under 2000 m
angle_stable[which.max(which(angle_stable$Distance < 2000.0)),]

# Polar plot
# Unstable 90% flux
polarPlot(angle_unstable,pollutant='co2',x='Distance',wd='WD',
          par.settings = list(axis.line = list(col = 0)),statistic='frequency',
          scales=list(x=list(at=NULL)),key=TRUE,smooth=TRUE,
          key.position='left',resolution='fine',
          key.footer='90% FLUX',upper=10000,
          key.header='FREQUENCY',
          exclude.missing = TRUE)

# Stable 90% flux
polarPlot(angle_stable,pollutant='co2',x='Distance',wd='WD',
          par.settings = list(axis.line = list(col = 0)),statistic='frequency',
          scales=list(x=list(at=NULL)),key=TRUE,smooth=FALSE,
          key.position='left',resolution='fine',
          key.footer='90% FLUX',upper=10000,
          key.header='FREQUENCY',
          exclude.missing = TRUE)
