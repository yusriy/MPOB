#### Run this once 'keratong_eddypro2.R' has been run
#### to group the data and obtain the plots

#### 1. Preliminaries ##################################

#install.packages("dplyr")
#install.packages("Hmisc)
library(dplyr)
library(plyr)
library(Hmisc)
# Saving old plotting parameters
old.par <- par()

#### 2. Grouping the data #########################################
# All data: from 2013-09-10 to 2015-02-28

# Grouping into hours for all data
# Note: since the update have to create separate groups for mean and sd.
df_group <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            LE=mean(LE,na.rm=TRUE),
            H=mean(H,na.rm=TRUE))

df_group_sd <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(sd_co2=sd(co2_flux,na.rm=TRUE),
            sdLE=sd(LE,na.rm=TRUE),
            sdH=sd(H,na.rm=TRUE))

#### 3. Diurnal plots ################

# To calculate the shaded region of the plot from standard deviations
possdco2 <- c(df_group$co2_flux + df_group_sd$sd_co2)
possdco2 <- as.data.frame(possdco2)
negsdco2 <- c(df_group$co2_flux - df_group_sd$sd_co2)
negsdco2 <- as.data.frame(negsdco2)
possdLE <- c(df_group$LE + df_group_sd$sdLE)
possdLE <- as.data.frame(possdLE)
negsdLE <- c(df_group$LE - df_group_sd$sdLE)
negsdLE <- as.data.frame(negsdLE)
possdH <- c(df_group$H + df_group_sd$sdH)
possdH <- as.data.frame(possdH)
negsdH <- c(df_group$H - df_group_sd$sdH)
negsdH <- as.data.frame(negsdH)
df_group <- cbind(df_group,df_group_sd[,-1],possdco2,negsdco2,possdLE,negsdLE,possdH,negsdH)
rm(negsdco2,possdco2,possdLE,negsdLE,possdH,negsdH,df_group_sd)

##### Fig. 6 CO2 flux, H and LE barplots #######################################

jpeg(file="figs/CO2_H_LE.jpg",width=3600,height=3060,res=360, quality=100)
mat4 <- matrix(c(1,2,3),nrow=3, ncol=1)
layout(mat4, heights = rep.int(8,nrow(mat4)), respect = FALSE)
par(family="serif", mar=c(3,2.4,0,0), tcl=-0.6, mgp=c(1,0.8,0),omi=c(0.35,0.35,0.1,0.35),lwd=1.7)

s <- barplot(data_group_a$gco2_flux,lwd=1.5,ylim=c(-8,0),ylab =NA, xlab=NA,cex.axis=1.8,xaxt='n',col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=s,labels=FALSE)
text(-0.2,-0.8,"a)",cex=2.2)
mtext(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2.5,outer= F,at=-8.2,adj=0,family="serif",cex=1.5)
box()

t <- barplot(data_group_a$gLE,lwd=1.5, ylim=c(0,150),ylab =NA,xlab= NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=t,labels=FALSE)
text(-0.2,130,"b)",cex=2.2)
mtext(expression(paste('LE',' (','W',' m'^{'-2'},')'))
      ,side=2,line=2.6,outer= F,at=38,adj=0,family="serif",cex=1.5)
box()

u <- barplot(data_group_a$gH,lwd=1.5,ylim=c(0,40),ylab =NA, xlab=NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=u,labels=c('Sept','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb'),cex.axis=2.1,las=2)
text(-0.2,35.5,"c)",cex=2.2)
mtext(expression(paste('H',' (','W',' m'^{'-2'},')'))
      ,side=2,line=2.5,outer= F,at=11,adj=0,family="serif",cex=1.5)
mtext("Month",side=1,line=4.7,outer= F,at=9,adj=0,family="serif",cex=1.6)
box()

dev.off()
rm(mat4,s,t,u)

#### Fig. 5 CO2, LE, H diurnal plots ####

jpeg(file="figs/fig1_co2_LE_H.jpg",width=1800,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3),nrow=3, ncol=1)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.4,0.35,0.1,0.1))

# a) CO2 flux diurnal plot for all data 
plot(df_group$hour,df_group$co2_flux,lwd=2,
     type='l',ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.6)
polygon(c(df_group$hour,rev(df_group$hour)), 
        c(df_group$possdco2,rev(df_group$negsdco2)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)
axis(side=1,at=NULL,labels=FALSE)
#axis(side=2,at=c(-30,-10,10,30),cex.axis=1.6)
text(0,38,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
mtext(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2.5,outer= F,at=-40,adj=0,family="serif",cex=1.4)

# b) LE diurnal plot for all data
plot(df_group$hour,df_group$LE,lwd=2,
     type='l',ylim=c(-100,500),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(df_group$hour,rev(df_group$hour)), 
        c(df_group$possdLE,rev(df_group$negsdLE)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=c(-100,0,100,200,300,400,500),cex.axis=1.6)
axis(side=2,at=c(200,400),cex.axis=1.6)
text(0,480,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
mtext(expression(paste('LE',' (','W',' m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=60,adj=0,family="serif",cex=1.4)

# c) H diurnal plot for all data
plot(df_group$hour,df_group$H,lwd=2,
     type='l',ylim=c(-50,200),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.6)
polygon(c(df_group$hour,rev(df_group$hour)), 
        c(df_group$possdH,rev(df_group$negsdH)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,190,"c)",cex=2.2)
axis(side=2,at=c(150,250),cex.axis=1.6)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.9)
mtext(expression(paste('H',' (','W',' m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=30,adj=0,family="serif",cex=1.4)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=6,adj=0,family="serif",cex=1.4)

rm(mat)
dev.off()

#### Fig. 4 Time series plots for CO2, LE and H ####
jpeg(file="figs/fig2_times_series.jpg",width=1800,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3),nrow=3, ncol=1)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.4,0.35,0.1,0.1))

# To label the x-axis with dates
time_stamp <- as.POSIXct(c('2013-09-15 12:00:00','2013-10-15 12:00:00','2013-11-15 12:00:00',
                           '2013-12-15 12:00:00','2014-01-15 12:00:00','2014-02-15 12:00:00',
                           '2014-03-15 12:00:00','2014-04-15 12:00:00','2014-05-15 12:00:00',
                           '2014-06-15 12:00:00','2014-07-15 12:00:00','2014-08-15 12:00:00',
                           '2014-09-15 12:00:00','2014-10-15 12:00:00','2014-11-15 12:00:00',
                           '2014-12-15 12:00:00','2015-01-15 12:00:00','2015-02-15 12:00:00'))

# a) CO2 flux
plot(df_EC2$time_stamp,df_EC2$co2_flux,type='l',xlab='',ylab='',xaxt='n',yaxt='n',ylim=c(-150,150),col='darkgray')
axis(side=2,at=c(-150,-100,-50,0,50,100,150),cex.axis=1.3)
axis(side=1,at=time_stamp,labels=c('','','','','','','','','','','','','','','','','',''))
text(as.POSIXct('2013-09-15 12:00:00'),145,"a)",cex=2.2)
mtext(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')),
      side=2.3,line=2.2,outer= F,at=-120,adj=0,family="serif",cex=1.2)

# b) LE
plot(df_EC2$time_stamp,df_EC2$LE,type='l',xlab='',ylab='',xaxt='n',yaxt='n',ylim=c(-400,800),col='darkgray')
axis(side=2,at=c(-400,-200,0,200,400,600,800),cex.axis=1.3)
axis(side=1,at=time_stamp,labels=c('','','','','','','','','','','','','','','','','',''))
text(as.POSIXct('2013-09-15 12:00:00'),780,"b)",cex=2.2)
mtext(expression(paste('LE',' (','W',' m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=-10,adj=0,family="serif",cex=1.2)

# c) H
plot(df_EC2$time_stamp,df_EC2$H,type='l',xlab='',ylab='',xaxt='n',yaxt='n',ylim=c(-400,800),col='darkgray')
mtext(expression(paste('H',' (','W',' m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=10,adj=0,family="serif",cex=1.2)
text(as.POSIXct('2013-09-15 12:00:00'),780,"c)",cex=2.2)
axis(side=2,at=c(-400,-200,0,200,400,600,800),cex.axis=1.3)
axis.POSIXct(side=1,at=time_stamp,labels=c('Sept','Oct','Nov','Dec','Jan 2014','Feb','Mar',
                                           'Apr','May','June','July','Aug','Sept','Oct',
                                           'Nov','Dec','Jan 2015','Feb'),cex.axis=1.1)
dev.off()

#### Energy balance closure ###########################

# Remove outliers from df_group1

#df_group1[c(121,123,132,133,140,143,253,325,332,333),] <- NA
#df_group1[c(which(df_group1$year=='20'))]
#df_group1[c(which(df_group1$month == '05' & df_group1$day == '01' & df_group1$year == '2014'),
#            which(df_group1$month == '05' & df_group1$day == '12' & df_group1$year == '2014'),
#            which(df_group1$month == '05' & df_group1$day == '13' & df_group1$year == '2014'),
#            which(df_group1$month == '05' & df_group1$day == '23' & df_group1$year == '2014'),
#            which(df_group1$month == '05' & df_group1$day == '03' & df_group1$year == '2014'),
#            which(df_group1$month == '05' & df_group1$day == '20' & df_group1$year == '2014'),
#            which(df_group1$month == '09' & df_group1$day == '10' & df_group1$year == '2013'), #| df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '21' & df_group1$year == '2013'), # | df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '28' & df_group1$year == '2013'), # | df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '29' & df_group1$year == '2013'), # | df_group1$year == '2013'),
#            which(df_group1$month == '09' & df_group1$day == '10' & df_group1$year == '2014'), #| df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '21' & df_group1$year == '2014'), # | df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '28' & df_group1$year == '2014'), # | df_group1$year == '2013'),
#            which(df_group1$month == '11' & df_group1$day == '29' & df_group1$year == '2014')),] <- NA # | df_group1$year == '2013')

######## Daily energy balance closure ##########
x <- df_group1$Rn - df_group1$SHF - df_group1$H_strg - df_group1$LE_strg

y <- df_group1$LE + df_group1$H

# To remove outliers on the left of fig
x[c(which(x < 0 & y > 50))] <- NA
y[c(which(x < 0 & y > 50))] <- NA

# Another set of outliers on the right of fig
x[c(which(x > 220 & y > 100))] <- NA
y[c(which(x > 220 & y > 100))] <- NA

# Linear regression results force to fit y = x
lm_daily <- lm(y ~ x)
summary(lm_daily)

plot(x,y,ylim=c(-50,400),xlim=c(-50,400),col='black',xlab='Rn - G - S',ylab='LE + H',pch=19,cex=0.7,
     main='Daily energy balance closure')
minor.tick()
abline(0,1,lwd=2,lty=2)
abline(lm_daily,lwd=2)
#text(300,120,'Best fit line, red')
#text(300,350,'Line 1:1, black')
text(300,50,labels = paste('slope = ',round(lm_daily$coefficients[2],digits=2)))
text(300,0,labels = paste('intercept = ',round(lm_daily$coefficients[1],digits=2)))
text(300,100,labels=paste('R^2 = ' , round(summary(lm_daily)$r.squared,digits=2)))


##### Monthly energy balance closure #########
x1 <- df_group2$Rn - df_group2$SHF - df_group2$H_strg - df_group2$LE_strg

y1 <- df_group2$LE + df_group2$H

plot(x1,y1,ylim=c(-50,400),xlim=c(-50,400),col='black',xlab='Rn - G - S',ylab='LE + H',pch=19,
     main='Monthly energy balance closure')
#abline(0,1,lwd=2)
# Linear regression results
lm_monthly <- lm(y1~x1-1)
summary(lm_monthly)
abline(lm2,lwd=2)

#### Half-hourly energy balance closure #################################

x1 <- df_EC2$Rn_1_1_1 - df_EC2$meanSHF - df_EC2$H_stor - df_EC2$LE_stor - df_EC2$soil_stor

y1 <- df_EC2$LE + df_EC2$H

# Need to remove LE + H < -200 due to rain events Prain_1_1_1 > 0
# before plotting by Jong

index <- which(df_EC2$Prain_1_1_1>0 & (df_EC2$LE + df_EC2$H) < -200)
index_others <- c(4065) # Outlier after rain event 2013-12-04 10:30:00 identified by Jong

# Plot without rain events
plot(x1[-index],y1[-index],ylim=c(-400,1000),xlim=c(-400,1000),col='black',xlab='Rn - G - S',
     ylab='LE + H',pch=19,cex=0.5)

lm_half <- lm(y1 ~ x1)
summary(lm_half)

abline(lm_half,lwd=3)
abline(0,1,lwd=3,lty=2)


########### Cleaning up ##############
rm(x,y,x1,y1)