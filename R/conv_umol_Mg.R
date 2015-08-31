# Function to convert flux (umol/m2.s) to Mg/ha.year (conv_flux)
# MW of CO2 = 44.01 g/gmol
# 1 year = 365 days x 24 hours x 60 min x 60 s = 31536000 s
# 1 ha = 10000 m2
conv_umol_Mg <- function(flux){
  conv_flux <- flux * 31536000 * 10000 * (1/1000000) *
    44.01 * (1/1000000)
}