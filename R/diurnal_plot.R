#####Diurnal plots#####################################################################
#install.packages("dplyr")
#install.packages("Hmisc)
library(dplyr)
library(Hmisc)
df_EC <- df_new %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m'),
         hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),LE=mean(LE,na.rm=TRUE), 
            H=mean(H,na.rm=TRUE), Z.L=mean(Z.L,na.rm=TRUE))

plot(df_EC$hour[which(df_EC$month=='01')],df_EC$co2_flux[which(df_EC$month=='01')],
     type='l',ylim=c(-40,20),xlab="Hour (local time)",
     ylab= expression(paste('CO'['2'],' flux (',mu,'mol ','m'^{'-2'},'s'^{'-1'},')')),lwd=2,xaxt='n')#cex.axis=2)
text(0,20,"a)")
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24))#cex.axis=2)