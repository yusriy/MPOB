# Function to calculate absolute humidity (kg/m3) using the 
# Magnus-Tetens equation
# T = temperature in deg C
# RH = relative humidity in %

abs_humidity <- function(temp,RH){
  hum <- (6.112 * exp(17.67*temp/(temp+243.5)) * RH * 2.1674)/(273.15 + temp)
  ans <- hum/1000 # To change to kg/m3 from g/m3
  return(ans)
}