#### To calculate soil CO2 efflux ####
# Have to do some light editing on original CSV raw data file
# by removing other headers in the dataframe
d_soil <- read.csv('data/soil_efflux_sample_data.csv')

## Rename some of the headers
names(d_soil)[7] <- 'co2_ref' # Unit is in [ppm]
names(d_soil)[14] <- 'dt' # Unit is in [s]

#### To count the number of data sets #####
index <- numeric()

count <- 0
count1 <- 1

for (i in 1:nrow(d_soil)){
  if (d_soil$dt[i] == 0) {
    count <- count1
    index[i] <- count
    count1 <- count + 1
  } else {
    index[i] <- count 
  }
}

# Cleaning up
rm(count,count1)

# Adding to the d_soil dataframe
d_soil <- cbind(d_soil,index)
rm(index)

## Factor to convert from [ppm s-1] to [umol m-2 s-1]
F <- 6.3918

# Cleaning up
rm(F)