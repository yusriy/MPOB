# Script to look into data for only between 08:00 and 15:00 
library(openair)
library(Hmisc)
# Select only data from 08:00 and 15:00
sample_data <- df_EC2
names(sample_data)[1] <- 'date'
sample_data$date <- as.POSIXct(sample_data$date)

sample <- selectByDate(sample_data,start=sample_data$date[1],
                       end=sample_data$date[nrow(sample_data)],
                       hour=7:15)

rm(sample_data)

## Plot the figure

jpeg(file="figs/vpd_co2.jpg",width=8,height=8,res=360, units='cm',quality=100)
par(mar=c(3,3.5,1,1))
plot(sample$VPD,sample$co2_flux,pch=19,cex=0.5,
     ylab='',
     xlab='')
minor.tick(nx=2,ny=2)
mtext(expression(paste('CO'['2'],' Flux (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2)
mtext('VPD (Pa)',side=1,line=2)
dev.off()
