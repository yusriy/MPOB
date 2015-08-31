# Integrating using the trapezium area rule

trapezium_intg <- function(heights,x1,x2,x3,x4,x5){
  area <- (0.5 * (heights[2] - heights[1]) * (x1 + x2)) +
    (0.5 * (heights[3] - heights[2]) * (x2 + x3)) +
    (0.5 * (heights[4] - heights[3]) * (x3 + x4)) +
    (0.5 * (heights[5] - heights[4]) * (x5 + x4))
    
  return(area)
}