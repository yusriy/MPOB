#Script to group and plot trends required by Dr. Hanif
#load libraries
library(dplyr)
library(Hmisc)

#####Grouping data###############################################################
#monthly group
data_group_a <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(gco2_flux=mean(co2_flux,na.rm=TRUE),gLE=mean(LE,na.rm=TRUE), gH=mean(H,na.rm=TRUE),
            gairtemp10=mean(Ta_3_1_1,na.rm=TRUE),gairtemp30=mean(Ta_5_1_1, na.rm=TRUE),
            sdco2=sd(co2_flux,na.rm=TRUE), sdT10=sd(Ta_3_1_1,na.rm=TRUE), sdT30=sd(Ta_5_1_1,na.rm=TRUE))
possdco2 <- c(data_group_a$gco2_flux + data_group_a$sdco2)
possdco2 <- as.data.frame(possdco2)
negsdco2 <- c(data_group_a$gco2_flux - data_group_a$sdco2)
negsdco2 <- as.data.frame(negsdco2)
possdT10 <- c(data_group_a$gairtemp10 + data_group_a$sdT10)
possdT10 <- as.data.frame(possdT10)
negsdT10 <- c(data_group_a$gairtemp10 - data_group_a$sdT10)
negsdT10 <- as.data.frame(negsdT10)
possdT30 <- c(data_group_a$gairtemp30 + data_group_a$sdT30)
possdT30 <- as.data.frame(possdT30)
negsdT30 <- c(data_group_a$gairtemp30 - data_group_a$sdT30)
negsdT30 <- as.data.frame(negsdT30)
month1 <- matrix(seq(1,18,1),nrow=18,ncol=1)
data_group_a <- cbind(data_group_a,possdco2,negsdco2,possdT10,negsdT10 ,possdT30,negsdT30,month1)
rm(possdco2,negsdco2,possdT10,negsdT10,possdT30,negsdT30,month1)

#diurnal group
data_group_b <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(gco2_flux=mean(co2_flux,na.rm=TRUE),gLE=mean(LE,na.rm=TRUE), gH=mean(H,na.rm=TRUE),
            gairtemp10=mean(Ta_3_1_1,na.rm=TRUE),gairtemp30=mean(Ta_5_1_1, na.rm=TRUE),
            sdco2=sd(co2_flux,na.rm=TRUE),sdLE=sd(LE,na.rm=TRUE))
possdco2 <- c(data_group_b$gco2_flux + data_group_b$sdco2)
possdco2 <- as.data.frame(possdco2)
negsdco2 <- c(data_group_b$gco2_flux - data_group_b$sdco2)
negsdco2 <- as.data.frame(negsdco2)
possdLE <- c(data_group_b$gLE + data_group_b$sdLE)
possdLE <- as.data.frame(possdLE)
negsdLE <- c(data_group_b$gLE - data_group_b$sdLE)
negsdLE <- as.data.frame(negsdLE)
data_group_b <- cbind(data_group_b,possdco2,negsdco2,possdLE,negsdLE)
rm(possdco2,negsdco2,possdLE,negsdLE)

#####Air temperature barplots#################################
#T_canopy = Air temperature at 10 m (In canopy)
#T_SL = Air temperature at 30.65 m (Surface layer)
#Barplot

jpeg(file="figs/Tabar.jpg",width=3600,height=3060,res=360, quality=100)
mat3 <- matrix(c(1,2),nrow=2, ncol=1)
layout(mat3, heights = rep.int(8,nrow(mat3)), respect = FALSE)
par(family="serif", mar=c(3.5,2.6,0.3,0), tcl=-0.6, mgp=c(1,0.8,0),omi=c(0.35,0.35,0.1,0.35),lwd=1.7)

x <- barplot(data_group_a$gairtemp30,lwd=1.5, ylim=c(23,28),ylab =NA,xlab= NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=x,labels=FALSE)
text(0,27.5,"a)",cex=2)
mtext(expression(paste('T'['SL'],' (',degree,'C)')),
      side=2,line=2.6,outer= F,at=24.7,adj=0,family="serif",cex=1.8)
box()

y <- barplot(data_group_a$gairtemp10,lwd=1.5, ylim=c(23,28),ylab =NA, xlab=NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=y,labels=c('Sept','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb'),cex.axis=1.8,las=2)
text(0,27.5,"b)",cex=2)
mtext(expression(paste('T'['canopy'],' (',degree,'C)')),
      side=2,line=2.5,outer= F,at=24.4,adj=0,family="serif",cex=1.8)
mtext("Month",side=1,line=4.2,outer= F,at=9,adj=0,family="serif",cex=1.8)
box()

dev.off()
rm(mat3,x,y)

#####CO2 flux, H and LE barplots########################################################
jpeg(file="figs/CO2_H_LE.jpg",width=3600,height=3060,res=360, quality=100)
mat4 <- matrix(c(1,2,3),nrow=3, ncol=1)
layout(mat4, heights = rep.int(8,nrow(mat4)), respect = FALSE)
par(family="serif", mar=c(3,2.4,0,0), tcl=-0.6, mgp=c(1,0.8,0),omi=c(0.4,0.35,0.35,0.35),lwd=1.7)

s <- barplot(data_group_a$gco2_flux,lwd=1.5,ylim=c(-8,0),ylab =NA, xlab=NA,cex.axis=1.8,xaxt='n',col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=s,labels=FALSE)
text(-0.2,-0.8,"a)",cex=2.2)
mtext(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2.5,outer= F,at=-8.2,adj=0,family="serif",cex=1.5)
box()
lines(x=c(1.3,1.3),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(3.7,3.7),y=c(-10,300),lty=2,lwd=2) # FTM
lines(x=c(8.5,8.5),y=c(-10,300),lty=2,lwd=2) # NEM
lines(x=c(10.9,10.9),y=c(-10,300),lty=2,lwd=2) # STM
lines(x=c(15.7,15.7),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(18.1,18.1),y=c(-10,300),lty=2,lwd=2) # FTM
mtext(side=3,'SWM',line=0.5, adj = 0.002,cex=2)
mtext(side=3,'FTM',line=0.5,adj = 0.11,cex=2)
mtext(side=3,'NEM',line=0.5, adj=0.26,cex=2)
mtext(side=3,'STM',line=0.5,adj = 0.44,cex=2)
mtext(side=3,'SWM',line=0.5, adj = 0.6,cex=2)
mtext(side=3,'FTM',line=0.5, adj = 0.78,cex=2)
mtext(side=3,'NEM',line=0.5, adj = 0.95,cex=2)

t <- barplot(data_group_a$gLE,lwd=1.5, ylim=c(0,150),ylab =NA,xlab= NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=t,labels=FALSE)
text(-0.2,130,"b)",cex=2.2)
mtext(expression(paste('LE',' (','W',' m'^{'-2'},')'))
      ,side=2,line=2.6,outer= F,at=38,adj=0,family="serif",cex=1.5)
box()
lines(x=c(1.3,1.3),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(3.7,3.7),y=c(-10,300),lty=2,lwd=2) # FTM
lines(x=c(8.5,8.5),y=c(-10,300),lty=2,lwd=2) # NEM
lines(x=c(10.9,10.9),y=c(-10,300),lty=2,lwd=2) # STM
lines(x=c(15.7,15.7),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(18.1,18.1),y=c(-10,300),lty=2,lwd=2) # FTM
u <- barplot(data_group_a$gH,lwd=1.5,ylim=c(0,40),ylab =NA, xlab=NA,cex.axis=1.8,col=adjustcolor("black",alpha.f=0),xpd=FALSE)
axis(side=1,at=u,labels=c('Sept','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb'),cex.axis=2.1,las=2)
text(-0.2,35.5,"c)",cex=2.2)
mtext(expression(paste('H',' (','W',' m'^{'-2'},')'))
      ,side=2,line=2.5,outer= F,at=11,adj=0,family="serif",cex=1.5)
mtext("Month",side=1,line=4.7,outer= F,at=9,adj=0,family="serif",cex=1.6)
box()
lines(x=c(1.3,1.3),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(3.7,3.7),y=c(-10,300),lty=2,lwd=2) # FTM
lines(x=c(8.5,8.5),y=c(-10,300),lty=2,lwd=2) # NEM
lines(x=c(10.9,10.9),y=c(-10,300),lty=2,lwd=2) # STM
lines(x=c(15.7,15.7),y=c(-10,300),lty=2,lwd=2) # SWM
lines(x=c(18.1,18.1),y=c(-10,300),lty=2,lwd=2) # FTM

dev.off()
rm(mat4,s,t,u)

#####Diurnal CO2 flux and LE#######################################

jpeg(file="figs/CO2&LE.jpg",width=3600,height=3060,res=360, quality=100)
mat4 <- matrix(c(1,2),nrow=2, ncol=1)
layout(mat4, heights = rep.int(10,nrow(mat4)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1,1,0),omi=c(0.35,0.35,0.1,0.35))

plot(data_group_b$hour,data_group_b$gco2_flux,lwd=1.5, ylim=c(-40,40),
     type='l',ylab =NA, xlab=NA,yaxt='n',xaxt='n',cex.axis=2.0)
polygon(c(data_group_b$hour, rev(data_group_b$hour)), 
        c(data_group_b$possdco2,rev(data_group_b$negsdco2)), 
        col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=c(-40,-20,0,20,40),cex.axis=1.8)
text(0,33,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
mtext(expression(paste('CO'['2'],' Flux (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')),
      side=2.3,line=2.2,outer= F,at=-30,adj=0,family="serif",cex=1.6)

plot(data_group_b$hour,data_group_b$gLE,lwd=1.5, type='l', ylim=c(-50,500),
     ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.8)
polygon(c(data_group_b$hour, rev(data_group_b$hour)), 
        c(data_group_b$possdLE,rev(data_group_b$negsdLE)), 
        col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.8)
axis(side=2,at=c(0,100,200,300,400,500),cex.axis=1.8)
text(0,440,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
mtext(expression(paste('LE',' (','W','m'^{'-2'},')')),
      side=2.3,line=2.2,outer= F,at=130,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1.5,line=2.4,outer= F,at=8.5,adj=0,family="serif",cex=1.8)

dev.off()
rm(mat4)
