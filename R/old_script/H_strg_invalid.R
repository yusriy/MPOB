# Calculate H storage from temperature profile
# Equation G_H = rho * Cp * summation of each level temperature difference
# at heights 2, 5, 10, 15, 30 m at T1, T2, T3, T4, and T5 respectively.
# Only levels 1 - 4 are used because at level 5 it is above the canopy.
# summation term = [(T1 - T2) * (z1 - z2)/(60 * 30)] add for higher levels
# rho = air density, kg/m3
# Cp = heat capacity or air, 1005 J/kg K at 20 C, might want to change to higher
# temperatures
# H_strg = rho * Cp * (summation term)
# Integration (or summation) is done by trapezoidal rule as recommended
# by LI-COR
H_strg <- function(rho,T1,T2,T3,T4){
  H <- rho * 1005 * ((0.5 * (T4 - T3)*(15 + 10)) + (0.5 * (T3 - T2)*(10 + 5)) + 
          (0.5 * (T2 - T1)*(5 + 2)))/(60 * 30)
  return(H)
}