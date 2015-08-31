#####Defining variables##############################################################
#install.packages("mblm")
#loading packages
library(mblm)
library(dplyr)
library(Hmisc)
#filter data
df_plot_eb2 <- df_plot

# Averaging soil heat flux
shf_avg <- rowMeans(df_plot_eb2[,c(142,143,144)],na.rm=TRUE)
#shf_avg <- c(df_plot_eb2$SHF_1_1_1 + df_plot_eb2$SHF_2_1_1 + df_plot_eb2$SHF_3_1_1 )
shf_avg <- as.data.frame(shf_avg)

# Accounting for energy storage due to conversion of PAR to biomass by oil palm
# 1 Wm^-2 = 4.6 mumolm^-2s^-1
# Conversion efficiency of C3 plants is 12.27% of PAR reaching them
par_biomass <- df_plot_eb2$PPFD_3_1_1
par_biomass <- par_biomass/4.6
par_biomass <- par_biomass*0.1227
par_biomass <- as.data.frame(par_biomass)
#Combining columns
df_plot_eb2 <- cbind(df_plot_eb2,shf_avg,par_biomass)
rm(shf_avg,par_biomass)
#df_plot$Prain_1_1_1 <- df_plot$Prain_1_1_1*1000
#df_plot$Prain_1_1_1[which(df_plot$Prain_1_1_1 > 10 | df_plot$Prain_1_1_1 <0)] <- NA
#group and summarise data
data_group_day2 <- df_plot_eb2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')
           ,day=format(as.POSIXlt(cut(time_stamp,breaks='day')),'%d')) %>%
  summarise(gco2_flux=mean(co2_flux,na.rm=TRUE),gLE=mean(LE,na.rm=TRUE), 
            gH=mean(H,na.rm=TRUE), gZ.L=mean(Z.L,na.rm=TRUE),gairtemp=mean(air_temperature,na.rm=TRUE),gPrain_1_1_1=mean(Prain_1_1_1,na.rm=TRUE),
            gu.=mean(u.,na.rm=TRUE),gRn=mean(Rn_1_1_1,na.rm=TRUE),gG=mean(shf_avg,na.rm=TRUE),gHstor=mean(H_stor,na.rm=TRUE),
            gLEstor=mean(LE_stor,na.rm=TRUE),gparbiomass=mean(par_biomass,na.rm=TRUE))

#Creating column of variables
rng <- c(data_group_day2$gRn - data_group_day2$gG)
rng <- as.data.frame(rng)
hle <- c(data_group_day2$gH + data_group_day2$gLE)
hle <- as.data.frame(hle)
rngs <- c(data_group_day2$gRn - data_group_day2$gG - data_group_day2$gHstor - data_group_day2$gLEstor)
rngs <- as.data.frame(rngs)
rngsp <- c(data_group_day2$gRn - data_group_day2$gG - data_group_day2$gHstor - data_group_day2$gLEstor - data_group_day2$gparbiomass)
rngsp <- as.data.frame(rngsp)
data_group_day2 <- cbind(data_group_day2,hle,rng,rngs,rngsp)
rm(hle,rng,rngs,rngsp)

#Remove outliers
data_group_day2$rng[c(121,123,132,133,140,143,253,318,325,332,333,109,221,326,329)] <- NA
data_group_day2$hle[c(121,123,132,133,140,143,253,318,325,332,333,109,221,326,329)] <- NA #121,123,132,133,140,143,253,325,332,333,109,221,318,326,329,214
data_group_day2$rngs[c(121,123,132,133,140,143,253,318,325,332,333,109,221,326,329)] <- NA
data_group_day2$rngsp[c(121,123,132,133,140,143,253,318,325,332,333,109,221,326,329)] <- NA

#####Plotting######################################################################
jpeg(file="C:/Users/user/Documents/CO2_eddy/energy_balance_daily_plot11.jpg",height=1600, width=1600, res=360,quality=100)
par(family="serif", mar=c(1.5,1.5,0.5,1), tcl=-0.3, mgp=c(1,0.3,0),omi=c(0.12,0.12,0.12,0.12),lwd=0.7)
lm_plot <- lm(hle~rngs+0,data=data_group_day2)
plot(data_group_day2$rngs, data_group_day2$hle,cex.axis=0.7, pch=16, cex=0.7,
     ylab= NA,xlab = NA,xaxt='n',yaxt='n',ylim=c(0,300),xlim=c(0,350))
axis(side=1,at=c(0,50,100,150,200,250,300,350),cex.axis=0.7,lwd=0.7)
axis(side=2,at=c(0,50,100,150,200,250,300),cex.axis=0.7,lwd=0.7)
mtext("LE + H",side=2,line=1.1,outer= F,at=125,adj=0,family="serif",cex=0.9)
mtext("Rn - G - S",side=1,line=1,outer= F,at=146,adj=0,family="serif",cex=0.9)
abline(lm_plot,lwd=1)
#abline(0,0.8117581,from=0,to=365,lwd=1,lty=2)#lower tail
#abline(0,0.8672479,from=0,to=365,lwd=1,lty=2)#upper tail
u <- par('usr') 
c1 <- curve(0.8117581*x, from=u[1], to=u[2],add=TRUE,lty=2)
c2 <- curve(0.8672479*x, from=u[1], to=u[2],add=TRUE,lty=2)
polygon(c(c1$x,rev(c2$x)), c(c1$y, rev(c2$y)),col=adjustcolor("grey",alpha.f=0.20), border=NA)
dev.off()


jpeg(file="C:/Users/user/Documents/CO2_eddy/energy_balance_daily_plot8.jpg",height=1500, width=1500, res=360,quality=100)
par(family="serif", mar=c(1.5,1.5,0.5,1), tcl=-0.3, mgp=c(1,0.3,0),omi=c(0.12,0.12,0.12,0.12),lwd=0.7)
lm_plot <- lm(hle~rngs+0,data=data_group_day2)
plot(data_group_day2$rngs, data_group_day2$hle,cex.axis=0.7, pch=16, cex=0.7,
     ylab= NA,xlab = NA,xaxt='n',yaxt='n',ylim=c(0,300),xlim=c(0,350))
axis(side=1,at=c(0,50,100,150,200,250,300,350),cex.axis=0.7,lwd=0.7)
axis(side=2,at=c(0,50,100,150,200,250,300),cex.axis=0.7,lwd=0.7)
mtext("LE + H",side=2,line=1.1,outer= F,at=130,adj=0,family="serif",cex=0.7)
mtext("Rn - G - S",side=1,line=1,outer= F,at=120,adj=0,family="serif",cex=0.7)
abline(lm_plot,lwd=1)
newx <- seq(1,365)
prd<-predict(lm_plot,newdata=data.frame(rngs=newx),interval = c("confidence"), 
             level = 0.95,type="response",na.action=na.pass)
lines(newx,prd[,2],lty=2)
lines(newx,prd[,3],lty=2)
polygon(c(data_group_day2$rngs,rev(data_group_day2$rngs)), c(prd[,2],rev(prd[,3])), col=adjustcolor("grey",alpha.f=0.25), border = NA)
dev.off()

#abline(0,0.8117581,lwd=1,lty=2)#lower tail
#abline(0,0.8672479,lwd=1,lty=2)#upper tail
#text(250,90,round(summary(lm_plot)$r.squared,digits=2),cex=0.7)
#text(250,75,round(summary(lm_plot)$coefficient,digits=2),cex=0.7)
#polygon(c(data_group_day2$rngs,rev(data_group_day2$rngs)), c(prd[,2],rev(prd[,3])), col=adjustcolor("grey",alpha.f=0.25), border = NA)
rm(lm_plot)
