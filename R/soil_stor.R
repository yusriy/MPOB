#Script to calculate soil heat storage
#####Averaging values#################################
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

#####Heat storage from depth 0 to heat flux sensor depth##################################
#Cs = (rho)b*Cd + (Theta)v*(rho)b*Cw
#where Cs = heat capacity of moist soil (J m-3 C-1)
# (rho)b = soil bulk density (for rengam series or typic paleudult; around 1400 kg m-3)
# cd = heat capacity of dry mineral soil (890 J kg-1 C-1; from Oliphant et al., 2004)
#(theta)v,tv = soil water content (m3/m-3)
# Cw = heat capacity of water (4181.3 J kg-1 C-1)

# First term
# dry soil capacity = 1400*890 = 1176000(J m-3 C-1) assuming values are constant throughout
firstterm <- matrix(rep(1246000,18491),nrow=25739,ncol=1)

#second term
tv <- df_new$swc_avg 
secondterm <- tv*1400*4181.3 #(J m-3 C-1)

#adding both terms
Cs <- firstterm + secondterm
rm(firstterm,tv,secondterm)

#Calculating soil temperature difference across each 30min
soil_dT <- numeric()
#soil_dT[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_new$stemp_avg)){
  soil_dT[i] <- (df_new$stemp_avg[i] - df_plot$stemp_avg[i-1])
}

#Calculating soil storage
#soil_stor = (soil_dT*Cs*d)/t (W m-2)
#where d = depth of the soil heat flux sensors (0.05m)
# t = time period (30min)
soil_stor <- (soil_dT*Cs*0.05)/(30*60)
#combine to main data frame
df_new <- cbind(df_new,soil_stor)
rm(soil_dT,Cs,soil_stor)
