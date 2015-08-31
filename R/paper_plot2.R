### Biomet plots ###
# Author: Uzaier
# Date: 2015-07-01

##### 1 Preliminaries #####
#install.packages('ggplot2')
#install.packages("dplyr")
#install.packages("Hmisc")
library('ggplot2')
library('dplyr')
library('Hmisc')

###### 2 Create groups #######

### Diurnal group
data_group1 <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp))%>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'))%>%
  
  summarise(vpd=mean(VPD,na.rm=TRUE),
            vpd_up=mean(VPD,na.rm=TRUE)+sd(VPD,na.rm=TRUE),
            vpd_down=mean(VPD,na.rm=TRUE)-sd(VPD,na.rm=TRUE),
            ta_3=mean(Ta_3_1_1,na.rm=TRUE),
            ta3_up=mean(Ta_3_1_1,na.rm=TRUE)+sd(Ta_3_1_1,na.rm=TRUE),
            ta3_down=mean(Ta_3_1_1,na.rm=TRUE)-sd(Ta_3_1_1,na.rm=TRUE),
            ta_5=mean(Ta_5_1_1,na.rm=TRUE), 
            ta5_up=mean(Ta_5_1_1,na.rm=TRUE)+sd(Ta_5_1_1,na.rm=TRUE),
            ta5_down=mean(Ta_5_1_1,na.rm=TRUE)-sd(Ta_5_1_1,na.rm=TRUE),
            rn=mean(Rn_1_1_1,na.rm=TRUE),
            rn_up=mean(Rn_1_1_1,na.rm=TRUE)+sd(Rn_1_1_1,na.rm=TRUE),
            rn_down=mean(Rn_1_1_1,na.rm=TRUE)-sd(Rn_1_1_1,na.rm=TRUE),
            ws=mean(WS_1_1_1,na.rm=TRUE),
            ws_up=mean(WS_1_1_1,na.rm=TRUE)+ sd(WS_1_1_1,na.rm=TRUE),
            ws_down=mean(WS_1_1_1,na.rm=TRUE)- sd(WS_1_1_1,na.rm=TRUE))
## Rain group ###    
data_group2 <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp))%>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')), '%Y'),
          month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%b'))%>%
  
  summarise(rain=sum(Prain_1_1_1,na.rm=TRUE))

## Create date column for data_group2
data_group2$month <- as.character(data_group2$month)
data_group2$year <- as.character(data_group2$year)
day <- matrix(rep(1,18),nrow=18,ncol=1)
day <- as.data.frame (day)
colnames(day) <- "day"

data_group2 <- cbind(data_group2,day)
date <- paste(data_group2$year,data_group2$month,data_group2$day)

data_group2$year <- date
data_group2 <- data_group2[,c(-2,-4)]
colnames(data_group2)[1] <- 'date'

date <- as.POSIXct(data_group2$date,format = " %Y %b %d",tz ="GMT")
data_group2$date <- date
rm(date,day)

###### Fig. 3 a) Diurnal plots #####

jpeg(filename = "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/diurnal_p.jpeg", width=3600, height= 3060,res=360,family='serif')
par(family = 'serif',mfrow = c(3,2),mar=c(4.5, 5.3, 0.5, 0.5))

### a) Plot 1 (air temp canopy) 
plot(data_group1$hour,data_group1$ta_3,type='l',ylim=c(20,36),
     xlab='',ylab=expression(paste('T'['canopy'],' (',degree,'C)')),
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7,cex.lab = 2.4,mgp = c(3, 1, 0))

x <- data_group1$hour
y_down <- data_group1$ta3_down
y_up <- data_group1$ta3_up

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)

text(0,35.8,"a)",cex = 2)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

### d) Plot 2 (air temp surface layer, SL)
plot(data_group1$hour,data_group1$ta_5,type='l',ylim=c(20,36),xlab="",
     ylab=expression(paste('T'['SL'],' (',degree,'C)')),
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7,cex.lab = 2.4,mgp = c(3, 1, 0))

x <- data_group1$hour
y_down <- data_group1$ta5_down
y_up <- data_group1$ta5_up

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)

text(0,35.8,"d)",cex = 2)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#### b) Plot 3 (wind speed)
plot(data_group1$hour, data_group1$ws,type='l',ylim=c(0,5),xlab='',
     ylab= (expression(paste('U (m s', ' '^{'-1'}, ')'))),
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7,cex.lab = 2.4,mgp = c(3, 1, 0))

x <- data_group1$hour
y_down <- data_group1$ws_down
y_up <- data_group1$ws_up

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)

text(0,5,"b)",cex = 2)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,1,2,3,4,5),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

### e) Plot 4 (Vapor pressure deficit, VPD) 
plot(data_group1$hour, data_group1$vpd,type='l',ylim=c(-100,2300),xlab='Hour (local time)',
     ylab='VPD (Pa)',lwd=2,xaxt='n',yaxt='n',cex.axis=1.7,cex.lab = 2.4,mgp = c(3, 1, 0)) 

x <- data_group1$hour
y_down <- data_group1$vpd_down
y_up <- data_group1$vpd_up

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)

text(0,2290,"e)",cex = 2)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,1000,2000),cex.axis=1.7)

axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)

### c) Plot 5 (Net radiation)
plot(data_group1$hour,data_group1$rn,type='l',ylim=c(-100,800),xlab="Hour (local time)",
     ylab= (expression(paste('Rn', ' (W m'^{'-2'}, ')'))),
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7,cex.lab = 2.4,mgp = c(3, 1, 0))

x <- data_group1$hour
y_down <- data_group1$rn_down
y_up <- data_group1$rn_up

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=adjustcolor("grey",alpha.f=0.5), border = NA)

text(0,795,"c)",cex = 2)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(-100,200,400,600,800),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)

dev.off()

###clean global env.
rm(x,y_down,y_up)

##### Fig. 2 Precipitation bar plot ######

jpeg(filename = "/Users/Yusri/Documents/Work/Data analysis/MPOB/figs/Rainfall_p.jpeg", 
     width=7200, height= 2500,res=360,family='serif')
par(family = 'serif',mar=c(2,2,1,1),omi=c(0.9,0.9,0.5,0),mai = c(0.4,0.3, 0.1, 0.1),lwd=1.5)

barplot(data_group2$rain,cex.axis=1.6,col='white',ylim=c(0,280),
        names.arg=c('Sept','Oct','Nov','Dec','Jan 2014','Feb','Mar',
                    'Apr','May','June','July','Aug','Sept','Oct',
                    'Nov','Dec','Jan 2015','Feb'),cex.names=1.7)
lines(x=c(1.3,1.3),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(3.7,3.7),y=c(-10,300),lty=2,lwd=2) # FTM
lines(x=c(8.5,8.5),y=c(-10,300),lty=2,lwd=2) # NEM
lines(x=c(10.9,10.9),y=c(-10,300),lty=2,lwd=2) # STM
lines(x=c(15.7,15.7),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(18.1,18.1),y=c(-10,300),lty=2,lwd=2) # FTM
minor.tick(ny=2,nx=1,tick.ratio=0.5)
box()
# Annotate on top of plot the monsoons
mtext(side=3,'SWM',line=0.5, adj = 0.025,cex=2)
mtext(side=3,'FTM',line=0.5,adj = 0.1,cex=2)
mtext(side=3,'NEM',line=0.5, adj=0.26,cex=2)
mtext(side=3,'STM',line=0.5,adj = 0.44,cex=2)
mtext(side=3,'SWM',line=0.5, adj = 0.6,cex=2)
mtext(side=3,'FTM',line=0.5, adj = 0.78,cex=2)
mtext(side=3,'NEM',line=0.5, adj = 0.95,cex=2)

mtext("Month", side=1, line = 2, outer=TRUE, at=0.5,cex = 1.7)
mtext("Monthly cummulative precipitation (mm)", side=2, line = 2,outer=TRUE, 
      at=0.5,cex = 1.7)

dev.off()

#### Cleanup global env ####
rm(timestamp)
