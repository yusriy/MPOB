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