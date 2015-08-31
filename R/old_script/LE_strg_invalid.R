# Calculate LE storage from absolute humidity profile
# Equation G_LE = L * summation of each level abs. hum. difference
# at heights 2, 5, 10, 15, 30 m at hum1, hum2, hum3, hum4, and hum5 respectively.
# Only levels 1 - 4 are used because at level 5 it is above the canopy.
# summation term = [(hum1 - hum2) * (z1 - z2)/(60 * 30)] add for higher levels
# L = 2,540,000 J/kg = latent heat of vaporization assumed as constant
# LE_strg = L * (summation term)
# Integration (or summation) is done by trapezoidal rule as recommended
# by LI-COR
LE_strg <- function(hum1,hum2,hum3,hum4){
  LE <- 2540000 * ((0.5 * (hum4 - hum3)*(15 + 10)) + (0.5 * (hum3 - hum2)*(10 + 5)) + 
                    (0.5 * (hum2 - hum1)*(5 + 2)))/(60 * 30)
  return(LE)
}