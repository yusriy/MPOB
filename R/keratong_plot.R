#####Description and loading data#########################################################
#This script is used to plot the following:
# 1. Site map for Keratong eddy covariance tower
# 2. Energy balance linear regression from Sept 2013 to Sept 2014
# 3. diurnal trends of (z-d)/L, CO2 flux, LE and H at monthly and seasonal timescales from Sept 2013 to Sept 2014

#internet connection is required to obtain map data
#**Please select and run the "Generate maps" section manually in order to plot the maps (not sure of the reason behind)

#EBR is the energy balance ratio calculated for the whole year

#sourcing script to load and filter data
source('R/df_new_proc_keratong.R')

#Cut the data frame df_EC2 to obtain one year data from Sept 10th 2013 to Sept 30th 2014
df_EC2 <- df_new[1:18491,1:163]
rm(df_new,i)

#####Preliminaries######################################################################
#setwd("yourworkingdirectory")

#Install packages when needed:
#install.packages("ggmap")
#install.packages("gridExtra")
#install.packages("maptools")
#install.packages("rgdal")
#install.packages("raster")

#loading libraries
library(ggmap)
library(mapproj)
library(ggplot2)
library(gridExtra)
library(maptools)
library(rgdal)
library(raster)
library(dplyr)
library(Hmisc)


# Sourcing custom functions
source('R/map_functions.R') # function to create scalebar in the map

#####Grab map data####################################################################

# 1. Zoomed in site map for the tower location in Keratong

sitemap <- get_googlemap(center = c(lon = 102.9327469,lat =2.788916588), sensor=TRUE,
                         size = c(640,640), scale = 2,zoom=15,maptype="terrain")

plot1 <- ggmap(sitemap) +
  geom_point(aes_string(x = "102.9327469",y = "2.788916588"),size = 15,shape=16,colour="black")+
  geom_text(aes_string(x="102.9208",y="2.7995"),label="(b)",colour="black",size=13.2,fontface="bold",hjust=0,vjust=-1.00,family="serif")+ xlab("") + ylab("") +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 25, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())+
  scaleBar(lon = 102.929, lat = 2.7762, distanceLon = 0.5, distanceLat = 0.05, distanceLegend = 0.11, dist.unit = "km", orientation = FALSE,legend.size = 9)

# 2. Map of Peninsular Malaysia with tower location marked by filled triangle

map <- ggmap(get_googlemap(center=c(102.9327469, 2.788916588),zoom=9,maptype='terrain',size = c(640,640),scale = 2,color='color',
                           extent='panel',darken=0))

map1= map + 
  geom_point (aes_string(x = "102.9327469",y = "2.788916588"), shape = 24, colour = "black", fill="black", size = 12) +  
  geom_text(aes_string(x = "102.178",y = "3.555"),label = "(a)",colour="black",size=13.2,fontface="bold",family="serif")+
  labs(x = "Longitude", y = "Latitude") +
  theme ( legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
          legend.justification = c(0, 0),
          legend.background = element_rect(colour = F, fill = "white"),
          legend.key = element_rect (fill = F, colour = F),
          axis.title=element_text(size=39,face="bold",colour="grey19",family="serif"),
          axis.text.x=element_text(size=28,face="bold",colour="grey19",family="serif"),
          axis.text.y=element_text(size=28,face="bold",colour="grey19",family="serif"),
          panel.border = element_rect(colour = "grey19",fill=F,size=1.2))

#Inset

mys0 <-getData("GADM", country="MYS", level=0) # download MYS level 0 map for ucdavis site

oc <- readOGR(dsn="Data/countries_shp", layer="countries")

pol<-data.frame(xmin=101.7,xmax=104.0 ,ymin=1.9 ,ymax=3.6)

p2<- ggplot()+geom_polygon(data=oc,aes(long,lat,group=group),fill="grey60")+
  geom_polygon(data=mys0, aes(long,lat,group=group),colour="grey10",fill="grey90",size=0.2)+
  theme_bw()+labs(x=NULL,y=NULL)+
  annotate("text", x = 102.1, y = 4.61, label = "PENINSULAR\nMALAYSIA",size=8,fontface="bold",family="serif")+
  annotate("text", x = 99.0, y = 1.90, label = "SUMATERA,\nINDONESIA",size=8,fontface="bold",family="serif")+
  ggtitle("LOCALITY MAP\n")+ coord_equal(xlim=c(96, 107), ylim=c(0.5, 7))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(),panel.border = element_rect(colour="white",fill=FALSE),
        plot.title = element_text(lineheight=0.1, face="bold",family="serif",size =26, colour = "grey20"),
        plot.background = element_rect(colour="black",fill="white",size=1),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.margin=unit(c(0.0,1,0.1,0.1),"mm")) 

fullMap <- map1 + inset(grob = ggplotGrob(p2+theme( 
  legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
  legend.justification = c(0, 0),
  legend.background = element_rect(colour = F, fill = "white"),
  legend.key = element_rect (fill = F, colour = F),
  panel.grid.major = element_blank (), # remove major grid
  panel.grid.minor = element_blank (),  # remove minor grid
  axis.text = element_blank (), 
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  axis.ticks = element_blank (),
  panel.border = element_rect(colour = "grey19",fill=F,size=1.2),
  panel.background = element_rect(fill = "white",colour = "black"))), 
  xmin = 101.85,  xmax = 103.05, ymin = 1.99, ymax = 2.49) +
  theme(plot.title = element_text(face = "bold",size = 14,colour="grey19"))

fullMapx <- fullMap + scaleBar(lon = 102.90, lat = 2.04, distanceLon = 30, distanceLat = 3, distanceLegend = 8, dist.unit = "km", orientation = FALSE,legend.size = 9)

#remove temporary values
rm(fullMap,map,map1,mys0,oc,p2,sitemap,pol)

#####Generate maps##############################################################
#Site map
png(filename = "Fig/Keratong Site.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
plot1
dev.off()

#fullmap
png(filename = "Fig/fullmap.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
fullMapx
dev.off()

#####30 min Energy Balance linear regression line################################################
hle <- rowSums(df_EC2[,c(7,9)]) # Adding up H and LE (measured surface energy flux)
hle <- as.data.frame(hle)
rn <- df_EC2$Rn_1_1_1 # Net radiation (Rn)
shf <- df_EC2$shf_avg # Soil heat flux (G)
hstor <- df_EC2$H_stor # Sensible heat storage
lestor <- df_EC2$LE_stor #Latent heat storage
sstor <- df_EC2$soil_stor # Soil heat storage
rngss <- c(rn - shf - hstor - lestor - sstor) #Net radiation minus soil heat flux and storage terms (Rn - G - S)
df_EC2 <- cbind(df_EC2,hle,rngss)
rm(rn,shf,hstor,lestor,hle,rngss,sstor)

# Remove outliers due to rain
df_EC2$rngss[4065] <- NA
df_EC2$hle[c(887,2840,3501,3905,3928,3950,3962,3979,4766,5946,8954,8963,8992,8995,
              11482,12000,12003,12668,16030,18003)] <- NA

#Plotting
jpeg(file="Fig/Energy_balance_30min.jpg",height=1600, width=1600, res=360,quality=100)
par(family="serif", mar=c(1.5,1.5,0.5,1), tcl=-0.3, mgp=c(1,0.3,0),omi=c(0.12,0.12,0.12,0.12),lwd=0.7)
lm_plot2 <- lm(hle~rngss,data=df_EC2)
plot(df_EC2$rngss, df_EC2$hle,cex.axis=1.0, pch=16, cex=0.7,
     ylab= NA,xlab = NA,xaxt='n',yaxt='n',ylim=c(-300,1000),xlim=c(-100,1000),col=grey(level=0.38,alpha=1))
axis(side=1,at=c(0,200,400,600,800,1000),cex.axis=0.85,lwd=0.8)
axis(side=2,at=c(-200,0,200,400,600,800,1000),cex.axis=0.85,lwd=0.8)
mtext("LE + H",side=2,line=1.1,outer= F,at=275,adj=0,family="serif",cex=0.9)
mtext(expression(paste('Rn ',"\u2013",' G ',"\u2013",' S')),side=1,line=1,outer= F,at=350,adj=0,family="serif",cex=0.9)
abline(lm_plot2,lwd=1.4,lty=5)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
#abline(0,1,lwd=1)
dev.off()

#Caculating energy balance ratio (EBR)
# where EBR = sum of all (H + LE)/ sum of all (Rn - G - S)
EBR <- sum(df_EC2$hle,na.rm=TRUE)/sum(df_EC2$rngss,na.rm=TRUE)
EBR

#remove temporary values
rm(lm_plot2)

#####Diurnal plots at monthly timescale (grouping data)######################################################
#Group and summarise data for monthly timscales
data_group <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(gco2_flux=mean(co2_flux,na.rm=TRUE),gLE=mean(LE,na.rm=TRUE), 
            gH=mean(H,na.rm=TRUE), gZ.L=mean(Z.L,na.rm=TRUE), gairtemp=mean(air_temperature,na.rm=TRUE),
            gPrain_1_1_1=mean(Prain_1_1_1,na.rm=TRUE),gwindspeed=mean(wind_speed,na.rm=TRUE),
            sdco2=sd(co2_flux,na.rm=TRUE),sdLE=sd(LE,na.rm=TRUE), 
            sdH=sd(H,na.rm=TRUE), sdZ.L=sd(Z.L,na.rm=TRUE),
            sdPrain_1_1_1=sd(Prain_1_1_1,na.rm=TRUE),sdwindspeed=sd(wind_speed,na.rm=TRUE))

possdco2 <- c(data_group$gco2_flux + data_group$sdco2)
possdco2 <- as.data.frame(possdco2)
negsdco2 <- c(data_group$gco2_flux - data_group$sdco2)
negsdco2 <- as.data.frame(negsdco2)
possdLE <- c(data_group$gLE + data_group$sdLE)
possdLE <- as.data.frame(possdLE)
negsdLE <- c(data_group$gLE - data_group$sdLE)
negsdLE <- as.data.frame(negsdLE)
possdH <- c(data_group$gH + data_group$sdH)
possdH <- as.data.frame(possdH)
negsdH <- c(data_group$gH - data_group$sdH)
negsdH <- as.data.frame(negsdH)
possdZ.L <- c(data_group$gZ.L + data_group$sdZ.L)
possdZ.L <- as.data.frame(possdZ.L)
negsdZ.L <- c(data_group$gZ.L - data_group$sdZ.L)
negsdZ.L <- as.data.frame(negsdZ.L)
possdPrain_1_1_1 <- c(data_group$gPrain_1_1_1 + data_group$sdPrain_1_1_1)
possdPrain_1_1_1 <- as.data.frame(possdPrain_1_1_1)
negsdPrain_1_1_1 <- c(data_group$gPrain_1_1_1 - data_group$sdPrain_1_1_1)
negsdPrain_1_1_1 <- as.data.frame(negsdPrain_1_1_1)
data_group <- cbind(data_group,possdco2,negsdco2,possdLE,negsdLE,
                    possdH,negsdH,possdZ.L,negsdZ.L,possdPrain_1_1_1,negsdPrain_1_1_1)
rm(negsdco2,possdco2,possdLE,negsdLE,possdH,negsdH,possdZ.L,negsdZ.L,
   possdPrain_1_1_1,negsdPrain_1_1_1)

#####CO2 flux monthly########################################################################################
jpeg(file="Fig/co2_monthlysd.jpg",width=3600,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3,4,5,6,0,0,0,0,0,0,7,8,9,10,11,12),nrow=6, ncol=3)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.1,0.35))

plot(data_group$hour[which(data_group$month=='01')],data_group$gco2_flux[which(data_group$month=='01')],lwd=1.5,
     type='l',ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='01')], 
          rev(data_group$hour[which(data_group$month=='01')])), 
        c(data_group$possdco2[which(data_group$month=='01')],
          rev(data_group$negsdco2[which(data_group$month=='01')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,30,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='02')],data_group$gco2_flux[which(data_group$month=='02')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='02')], 
          rev(data_group$hour[which(data_group$month=='02')])), 
        c(data_group$possdco2[which(data_group$month=='02')],
          rev(data_group$negsdco2[which(data_group$month=='02')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,30,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='03')],data_group$gco2_flux[which(data_group$month=='03')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='03')], 
          rev(data_group$hour[which(data_group$month=='03')])), 
        c(data_group$possdco2[which(data_group$month=='03')],
          rev(data_group$negsdco2[which(data_group$month=='03')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,30,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='04')],data_group$gco2_flux[which(data_group$month=='04')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='04')], 
          rev(data_group$hour[which(data_group$month=='04')])), 
        c(data_group$possdco2[which(data_group$month=='04')],
          rev(data_group$negsdco2[which(data_group$month=='04')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,30,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='05')],data_group$gco2_flux[which(data_group$month=='05')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='05')], 
          rev(data_group$hour[which(data_group$month=='05')])), 
        c(data_group$possdco2[which(data_group$month=='05')],
          rev(data_group$negsdco2[which(data_group$month=='05')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,30,"e)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='06')],data_group$gco2_flux[which(data_group$month=='06')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='06')], 
          rev(data_group$hour[which(data_group$month=='06')])), 
        c(data_group$possdco2[which(data_group$month=='06')],
          rev(data_group$negsdco2[which(data_group$month=='06')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0,30,"f)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('CO'['2'],' Flux (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2.5,outer= F,at=142,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group$hour[which(data_group$month=='07')],data_group$gco2_flux[which(data_group$month=='07')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='07')], 
          rev(data_group$hour[which(data_group$month=='07')])), 
        c(data_group$possdco2[which(data_group$month=='07')],
          rev(data_group$negsdco2[which(data_group$month=='07')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"g)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='08')],data_group$gco2_flux[which(data_group$month=='08')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='08')], 
          rev(data_group$hour[which(data_group$month=='08')])), 
        c(data_group$possdco2[which(data_group$month=='08')],
          rev(data_group$negsdco2[which(data_group$month=='08')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"h)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='09')],data_group$gco2_flux[which(data_group$month=='09')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='09')], 
          rev(data_group$hour[which(data_group$month=='09')])), 
        c(data_group$possdco2[which(data_group$month=='09')],
          rev(data_group$negsdco2[which(data_group$month=='09')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"i)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='10')],data_group$gco2_flux[which(data_group$month=='10')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='10')], 
          rev(data_group$hour[which(data_group$month=='10')])), 
        c(data_group$possdco2[which(data_group$month=='10')],
          rev(data_group$negsdco2[which(data_group$month=='10')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"j)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='11')],data_group$gco2_flux[which(data_group$month=='11')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='11')], 
          rev(data_group$hour[which(data_group$month=='11')])), 
        c(data_group$possdco2[which(data_group$month=='11')],
          rev(data_group$negsdco2[which(data_group$month=='11')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"k)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='12')],data_group$gco2_flux[which(data_group$month=='12')],lwd=1.5, type='l',
     ylim=c(-40,40),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='12')], 
          rev(data_group$hour[which(data_group$month=='12')])), 
        c(data_group$possdco2[which(data_group$month=='12')],
          rev(data_group$negsdco2[which(data_group$month=='12')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL,labels=FALSE)
text(0,30,"l)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()

#####LE monthly##############################################################################
jpeg(file="Fig/LE_monthlysd.jpg",width=3600,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3,4,5,6,0,0,0,0,0,0,7,8,9,10,11,12),nrow=6, ncol=3)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.1,0.35))

plot(data_group$hour[which(data_group$month=='01')],data_group$gLE[which(data_group$month=='01')],lwd=1.5,
     type='l',ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='01')], 
          rev(data_group$hour[which(data_group$month=='01')])), 
        c(data_group$possdLE[which(data_group$month=='01')],
          rev(data_group$negsdLE[which(data_group$month=='01')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,450,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='02')],data_group$gLE[which(data_group$month=='02')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='02')], 
          rev(data_group$hour[which(data_group$month=='02')])), 
        c(data_group$possdLE[which(data_group$month=='02')],
          rev(data_group$negsdLE[which(data_group$month=='02')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,450,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='03')],data_group$gLE[which(data_group$month=='03')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='03')], 
          rev(data_group$hour[which(data_group$month=='03')])), 
        c(data_group$possdLE[which(data_group$month=='03')],
          rev(data_group$negsdLE[which(data_group$month=='03')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,450,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='04')],data_group$gLE[which(data_group$month=='04')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='04')], 
          rev(data_group$hour[which(data_group$month=='04')])), 
        c(data_group$possdLE[which(data_group$month=='04')],
          rev(data_group$negsdLE[which(data_group$month=='04')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,450,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='05')],data_group$gLE[which(data_group$month=='05')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='05')], 
          rev(data_group$hour[which(data_group$month=='05')])), 
        c(data_group$possdLE[which(data_group$month=='05')],
          rev(data_group$negsdLE[which(data_group$month=='05')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,450,"e)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='06')],data_group$gLE[which(data_group$month=='06')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='06')], 
          rev(data_group$hour[which(data_group$month=='06')])), 
        c(data_group$possdLE[which(data_group$month=='06')],
          rev(data_group$negsdLE[which(data_group$month=='06')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0,450,"f)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('LE',' (','W','m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=1845,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group$hour[which(data_group$month=='07')],data_group$gLE[which(data_group$month=='07')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='07')], 
          rev(data_group$hour[which(data_group$month=='07')])), 
        c(data_group$possdLE[which(data_group$month=='07')],
          rev(data_group$negsdLE[which(data_group$month=='07')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"g)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='08')],data_group$gLE[which(data_group$month=='08')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='08')], 
          rev(data_group$hour[which(data_group$month=='08')])), 
        c(data_group$possdLE[which(data_group$month=='08')],
          rev(data_group$negsdLE[which(data_group$month=='08')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"h)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='09')],data_group$gLE[which(data_group$month=='09')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='09')], 
          rev(data_group$hour[which(data_group$month=='09')])), 
        c(data_group$possdLE[which(data_group$month=='09')],
          rev(data_group$negsdLE[which(data_group$month=='09')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"i)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='10')],data_group$gLE[which(data_group$month=='10')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='10')], 
          rev(data_group$hour[which(data_group$month=='10')])), 
        c(data_group$possdLE[which(data_group$month=='10')],
          rev(data_group$negsdLE[which(data_group$month=='10')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"j)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='11')],data_group$gLE[which(data_group$month=='11')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='11')], 
          rev(data_group$hour[which(data_group$month=='11')])), 
        c(data_group$possdLE[which(data_group$month=='11')],
          rev(data_group$negsdLE[which(data_group$month=='11')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"k)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='12')],data_group$gLE[which(data_group$month=='12')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='12')], 
          rev(data_group$hour[which(data_group$month=='12')])), 
        c(data_group$possdLE[which(data_group$month=='12')],
          rev(data_group$negsdLE[which(data_group$month=='12')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL,labels=FALSE)
text(0,450,"l)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()

#####H monthly#############################################################################
jpeg(file="Fig/H_monthlysd.jpg",width=3600,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3,4,5,6,0,0,0,0,0,0,7,8,9,10,11,12),nrow=6, ncol=3)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.15,0.35))

plot(data_group$hour[which(data_group$month=='01')],data_group$gH[which(data_group$month=='01')],lwd=1.5,
     type='l',ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='01')], 
          rev(data_group$hour[which(data_group$month=='01')])), 
        c(data_group$possdH[which(data_group$month=='01')],
          rev(data_group$negsdH[which(data_group$month=='01')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,200,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='02')],data_group$gH[which(data_group$month=='02')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='02')], 
          rev(data_group$hour[which(data_group$month=='02')])), 
        c(data_group$possdH[which(data_group$month=='02')],
          rev(data_group$negsdH[which(data_group$month=='02')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,200,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='03')],data_group$gH[which(data_group$month=='03')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='03')], 
          rev(data_group$hour[which(data_group$month=='03')])), 
        c(data_group$possdH[which(data_group$month=='03')],
          rev(data_group$negsdH[which(data_group$month=='03')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,200,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='04')],data_group$gH[which(data_group$month=='04')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='04')], 
          rev(data_group$hour[which(data_group$month=='04')])), 
        c(data_group$possdH[which(data_group$month=='04')],
          rev(data_group$negsdH[which(data_group$month=='04')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,200,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='05')],data_group$gH[which(data_group$month=='05')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='05')], 
          rev(data_group$hour[which(data_group$month=='05')])), 
        c(data_group$possdH[which(data_group$month=='05')],
          rev(data_group$negsdH[which(data_group$month=='05')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,200,"e)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='06')],data_group$gH[which(data_group$month=='06')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='06')], 
          rev(data_group$hour[which(data_group$month=='06')])), 
        c(data_group$possdH[which(data_group$month=='06')],
          rev(data_group$negsdH[which(data_group$month=='06')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0,200,"f)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('H',' (','W','m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=870,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group$hour[which(data_group$month=='07')],data_group$gH[which(data_group$month=='07')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='07')], 
          rev(data_group$hour[which(data_group$month=='07')])), 
        c(data_group$possdH[which(data_group$month=='07')],
          rev(data_group$negsdH[which(data_group$month=='07')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"g)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='08')],data_group$gH[which(data_group$month=='08')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='08')], 
          rev(data_group$hour[which(data_group$month=='08')])), 
        c(data_group$possdH[which(data_group$month=='08')],
          rev(data_group$negsdH[which(data_group$month=='08')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"h)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='09')],data_group$gH[which(data_group$month=='09')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='09')], 
          rev(data_group$hour[which(data_group$month=='09')])), 
        c(data_group$possdH[which(data_group$month=='09')],
          rev(data_group$negsdH[which(data_group$month=='09')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"i)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='10')],data_group$gH[which(data_group$month=='10')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='10')], 
          rev(data_group$hour[which(data_group$month=='10')])), 
        c(data_group$possdH[which(data_group$month=='10')],
          rev(data_group$negsdH[which(data_group$month=='10')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"j)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='11')],data_group$gH[which(data_group$month=='11')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='11')], 
          rev(data_group$hour[which(data_group$month=='11')])), 
        c(data_group$possdH[which(data_group$month=='11')],
          rev(data_group$negsdH[which(data_group$month=='11')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"k)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='12')],data_group$gH[which(data_group$month=='12')],lwd=1.5, type='l',
     ylim=c(-50,250),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=1.9)
polygon(c(data_group$hour[which(data_group$month=='12')], 
          rev(data_group$hour[which(data_group$month=='12')])), 
        c(data_group$possdH[which(data_group$month=='12')],
          rev(data_group$negsdH[which(data_group$month=='12')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL,labels=FALSE)
text(0,200,"l)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.9)

dev.off()

#####Z.L monthly##########################################################################
jpeg(file="Fig/zl_monthlysd.jpg",width=3600,height=3060,res=360, quality=100)
mat <- matrix(c(1,2,3,4,5,6,0,0,0,0,0,0,7,8,9,10,11,12),nrow=6, ncol=3)
layout(mat, widths = c(20,0.3,20), heights = rep.int(5,nrow(mat)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.15,0.35))

plot(data_group$hour[which(data_group$month=='01')],data_group$gZ.L[which(data_group$month=='01')],lwd=1.5,
     type='l',ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='01')], 
          rev(data_group$hour[which(data_group$month=='01')])), 
        c(data_group$possdZ.L[which(data_group$month=='01')],
          rev(data_group$negsdZ.L[which(data_group$month=='01')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,4,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='02')],data_group$gZ.L[which(data_group$month=='02')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='02')], 
          rev(data_group$hour[which(data_group$month=='02')])), 
        c(data_group$possdZ.L[which(data_group$month=='02')],
          rev(data_group$negsdZ.L[which(data_group$month=='02')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,4,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='03')],data_group$gZ.L[which(data_group$month=='03')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='03')], 
          rev(data_group$hour[which(data_group$month=='03')])), 
        c(data_group$possdZ.L[which(data_group$month=='03')],
          rev(data_group$negsdZ.L[which(data_group$month=='03')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,4,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='04')],data_group$gZ.L[which(data_group$month=='04')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='04')], 
          rev(data_group$hour[which(data_group$month=='04')])), 
        c(data_group$possdZ.L[which(data_group$month=='04')],
          rev(data_group$negsdZ.L[which(data_group$month=='04')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,4,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='05')],data_group$gZ.L[which(data_group$month=='05')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='05')], 
          rev(data_group$hour[which(data_group$month=='05')])), 
        c(data_group$possdZ.L[which(data_group$month=='05')],
          rev(data_group$negsdZ.L[which(data_group$month=='05')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0,4,"e)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='06')],data_group$gZ.L[which(data_group$month=='06')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='06')], 
          rev(data_group$hour[which(data_group$month=='06')])), 
        c(data_group$possdZ.L[which(data_group$month=='06')],
          rev(data_group$negsdZ.L[which(data_group$month=='06')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0,4,"f)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste(zeta)),side=2,line=2.5,outer= F,at=30,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group$hour[which(data_group$month=='07')],data_group$gZ.L[which(data_group$month=='07')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='07')], 
          rev(data_group$hour[which(data_group$month=='07')])), 
        c(data_group$possdZ.L[which(data_group$month=='07')],
          rev(data_group$negsdZ.L[which(data_group$month=='07')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"g)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='08')],data_group$gZ.L[which(data_group$month=='08')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='08')], 
          rev(data_group$hour[which(data_group$month=='08')])), 
        c(data_group$possdZ.L[which(data_group$month=='08')],
          rev(data_group$negsdZ.L[which(data_group$month=='08')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"h)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='09')],data_group$gZ.L[which(data_group$month=='09')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='09')], 
          rev(data_group$hour[which(data_group$month=='09')])), 
        c(data_group$possdZ.L[which(data_group$month=='09')],
          rev(data_group$negsdZ.L[which(data_group$month=='09')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"i)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='10')],data_group$gZ.L[which(data_group$month=='10')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='10')], 
          rev(data_group$hour[which(data_group$month=='10')])), 
        c(data_group$possdZ.L[which(data_group$month=='10')],
          rev(data_group$negsdZ.L[which(data_group$month=='10')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"j)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='11')],data_group$gZ.L[which(data_group$month=='11')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='11')], 
          rev(data_group$hour[which(data_group$month=='11')])), 
        c(data_group$possdZ.L[which(data_group$month=='11')],
          rev(data_group$negsdZ.L[which(data_group$month=='11')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"k)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group$hour[which(data_group$month=='12')],data_group$gZ.L[which(data_group$month=='12')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n',cex.axis=2.0)
polygon(c(data_group$hour[which(data_group$month=='12')], 
          rev(data_group$hour[which(data_group$month=='12')])), 
        c(data_group$possdZ.L[which(data_group$month=='12')],
          rev(data_group$negsdZ.L[which(data_group$month=='12')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL,labels=FALSE)
text(0,4,"l)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()
rm(mat)

#####Diurnal plots at seasonal timescale (grouping data)#################################################
#Create and add column for the seasons
#Let: northeast monsoon = 1 (Dec-Mar), inter-monsoon season 1 = 2(Apr-May), 
#Let: southwest monsoon = 3 (June-Sept), inter-monsoon season 2 = 4 (Oct-Nov)
timestamp <- as.data.frame(df_EC2$time_stamp)
colnames(timestamp) <- "timestamp"
season_col <- matrix(rep(3,18491),nrow=18491,ncol=1)
season_col <- as.data.frame(season_col)
colnames(season_col) <- "season"
season_stamp <- cbind(timestamp,season_col)
rm(timestamp,season_col)
season_stamp$season[which(season_stamp$timestamp > as.POSIXct('2013-09-30 23:56:00') & 
                            season_stamp$timestamp < as.POSIXct('2013-12-01 00:00:00'))] <- 4
season_stamp$season[which(season_stamp$timestamp > as.POSIXct('2013-11-30 23:30:00') & 
                            season_stamp$timestamp < as.POSIXct('2014-04-01 00:00:00'))] <- 1
season_stamp$season[which(season_stamp$timestamp > as.POSIXct('2014-03-31 23:30:00') & 
                            season_stamp$timestamp < as.POSIXct('2014-06-01 00:00:00'))] <- 2
season <- as.data.frame(season_stamp$season)
colnames(season) <- "season"
df_EC2 <- cbind(df_EC2,season)
rm(season_stamp,season)

#Group and summarise data for seasonal timescales
data_group_season <- df_EC2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(season,hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(gco2_flux=mean(co2_flux,na.rm=TRUE),gLE=mean(LE,na.rm=TRUE), 
            gH=mean(H,na.rm=TRUE), gZ.L=mean(Z.L,na.rm=TRUE), gu=mean(u.,na.rm=TRUE),
            gPrain_1_1_1=mean(Prain_1_1_1,na.rm=TRUE),gRn=mean(Rn_1_1_1,na.rm=TRUE),
            gPAR=mean(PPFD_3_1_1,na.rm=TRUE),
            sdco2=sd(co2_flux,na.rm=TRUE),sdLE=sd(LE,na.rm=TRUE), 
            sdH=sd(H,na.rm=TRUE), sdZ.L=sd(Z.L,na.rm=TRUE),
            sdPrain_1_1_1=sd(Prain_1_1_1,na.rm=TRUE))

#Create standard deviation columns
possdco2 <- c(data_group_season$gco2_flux + data_group_season$sdco2)
possdco2 <- as.data.frame(possdco2)
negsdco2 <- c(data_group_season$gco2_flux - data_group_season$sdco2)
negsdco2 <- as.data.frame(negsdco2)
possdLE <- c(data_group_season$gLE + data_group_season$sdLE)
possdLE <- as.data.frame(possdLE)
negsdLE <- c(data_group_season$gLE - data_group_season$sdLE)
negsdLE <- as.data.frame(negsdLE)
possdH <- c(data_group_season$gH + data_group_season$sdH)
possdH <- as.data.frame(possdH)
negsdH <- c(data_group_season$gH - data_group_season$sdH)
negsdH <- as.data.frame(negsdH)
possdZ.L <- c(data_group_season$gZ.L + data_group_season$sdZ.L)
possdZ.L <- as.data.frame(possdZ.L)
negsdZ.L <- c(data_group_season$gZ.L - data_group_season$sdZ.L)
negsdZ.L <- as.data.frame(negsdZ.L)
possdPrain_1_1_1 <- c(data_group_season$gPrain_1_1_1 + data_group_season$sdPrain_1_1_1)
possdPrain_1_1_1 <- as.data.frame(possdPrain_1_1_1)
negsdPrain_1_1_1 <- c(data_group_season$gPrain_1_1_1 - data_group_season$sdPrain_1_1_1)
negsdPrain_1_1_1 <- as.data.frame(negsdPrain_1_1_1)
data_group_season <- cbind(data_group_season,possdco2,negsdco2,possdLE,negsdLE,
                           possdH,negsdH,possdZ.L,negsdZ.L,possdPrain_1_1_1,negsdPrain_1_1_1)
rm(negsdco2,possdco2,possdLE,negsdLE,possdH,negsdH,possdZ.L,negsdZ.L,
   possdPrain_1_1_1,negsdPrain_1_1_1)

#####CO2 flux seasonal################################################################################
jpeg(file="Fig/co2_seasonsd.jpg",width=3600,height=3060,res=360, quality=100)
mat2 <- matrix(c(1,2,0,0,3,4),nrow=2, ncol=3)
layout(mat2, widths = c(20,0.1,20), heights = rep.int(5,nrow(mat2)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.2,0.35))

plot(data_group_season$hour[which(data_group_season$season=='1')],data_group_season$gco2_flux[which(data_group_season$season=='1')],lwd=1.5,
     type='l',ylim=c(-40,30),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='1')], 
          rev(data_group_season$hour[which(data_group_season$season=='1')])), 
        c(data_group_season$possdco2[which(data_group_season$season=='1')],
          rev(data_group_season$negsdco2[which(data_group_season$season=='1')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0.5,28,"a)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='2')],data_group_season$gco2_flux[which(data_group_season$season=='2')],lwd=1.5, type='l',
     ylim=c(-40,30),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='2')], 
          rev(data_group_season$hour[which(data_group_season$season=='2')])), 
        c(data_group_season$possdco2[which(data_group_season$season=='2')],
          rev(data_group_season$negsdco2[which(data_group_season$season=='2')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0.5,28,"b)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('CO'['2'],' Flux (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')),
      side=2,line=2.5,outer= F,at=5,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group_season$hour[which(data_group_season$season=='3')],data_group_season$gco2_flux[which(data_group_season$season=='3')],lwd=1.5, type='l',
     ylim=c(-40,30),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='3')], 
          rev(data_group_season$hour[which(data_group_season$season=='3')])), 
        c(data_group_season$possdco2[which(data_group_season$season=='3')],
          rev(data_group_season$negsdco2[which(data_group_season$season=='3')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0.5,28,"c)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='4')],data_group_season$gco2_flux[which(data_group_season$season=='4')],lwd=1.5, type='l',
     ylim=c(-40,30),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='4')], 
          rev(data_group_season$hour[which(data_group_season$season=='4')])), 
        c(data_group_season$possdco2[which(data_group_season$season=='4')],
          rev(data_group_season$negsdco2[which(data_group_season$season=='4')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL, labels=FALSE)
text(0.5,28,"d)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()

#####LE seasonal##############################################################################
jpeg(file="Fig/LE_seasonsd.jpg",width=3600,height=3060,res=360, quality=100)
mat2 <- matrix(c(1,2,0,0,3,4),nrow=2, ncol=3)
layout(mat2, widths = c(20,0.1,20), heights = rep.int(5,nrow(mat2)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.2,0.35))

plot(data_group_season$hour[which(data_group_season$season=='1')],data_group_season$gLE[which(data_group_season$season=='1')],lwd=1.5,
     ylim=c(-100,550),type='l',ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='1')], 
          rev(data_group_season$hour[which(data_group_season$season=='1')])), 
        c(data_group_season$possdLE[which(data_group_season$season=='1')],
          rev(data_group_season$negsdLE[which(data_group_season$season=='1')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0.5,525,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='2')],data_group_season$gLE[which(data_group_season$season=='2')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='2')], 
          rev(data_group_season$hour[which(data_group_season$season=='2')])), 
        c(data_group_season$possdLE[which(data_group_season$season=='2')],
          rev(data_group_season$negsdLE[which(data_group_season$season=='2')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0.5,525,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('LE',' (','W','m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=455,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group_season$hour[which(data_group_season$season=='3')],data_group_season$gLE[which(data_group_season$season=='3')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='3')], 
          rev(data_group_season$hour[which(data_group_season$season=='3')])), 
        c(data_group_season$possdLE[which(data_group_season$season=='3')],
          rev(data_group_season$negsdLE[which(data_group_season$season=='3')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0.5,525,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='4')],data_group_season$gLE[which(data_group_season$season=='4')],lwd=1.5, type='l',
     ylim=c(-100,550),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='4')], 
          rev(data_group_season$hour[which(data_group_season$season=='4')])), 
        c(data_group_season$possdLE[which(data_group_season$season=='4')],
          rev(data_group_season$negsdLE[which(data_group_season$season=='4')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL, labels=FALSE)
text(0.5,525,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()

#####H seasonal############################################################################
jpeg(file="Fig/H_seasonsd.jpg",width=3600,height=3060,res=360, quality=100)
mat2 <- matrix(c(1,2,0,0,3,4),nrow=2, ncol=3)
layout(mat2, widths = c(20,0.1,20), heights = rep.int(5,nrow(mat2)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.2,0.35))

plot(data_group_season$hour[which(data_group_season$season=='1')],data_group_season$gH[which(data_group_season$season=='1')],lwd=1.5,
     ylim=c(-50,200),type='l',ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='1')], 
          rev(data_group_season$hour[which(data_group_season$season=='1')])), 
        c(data_group_season$possdH[which(data_group_season$season=='1')],
          rev(data_group_season$negsdH[which(data_group_season$season=='1')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0.5,190,"a)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='2')],data_group_season$gH[which(data_group_season$season=='2')],lwd=1.5, type='l',
     ylim=c(-50,200),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='2')], 
          rev(data_group_season$hour[which(data_group_season$season=='2')])), 
        c(data_group_season$possdH[which(data_group_season$season=='2')],
          rev(data_group_season$negsdH[which(data_group_season$season=='2')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0.5,190,"b)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste('H',' (','W','m'^{'-2'},')')),
      side=2,line=2.5,outer= F,at=165,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group_season$hour[which(data_group_season$season=='3')],data_group_season$gH[which(data_group_season$season=='3')],lwd=1.5, type='l',
     ylim=c(-50,200),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='3')], 
          rev(data_group_season$hour[which(data_group_season$season=='3')])), 
        c(data_group_season$possdH[which(data_group_season$season=='3')],
          rev(data_group_season$negsdH[which(data_group_season$season=='3')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0.5,190,"c)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='4')],data_group_season$gH[which(data_group_season$season=='4')],lwd=1.5, type='l',
     ylim=c(-50,200),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='4')], 
          rev(data_group_season$hour[which(data_group_season$season=='4')])), 
        c(data_group_season$possdH[which(data_group_season$season=='4')],
          rev(data_group_season$negsdH[which(data_group_season$season=='4')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL, labels=FALSE)
text(0.5,190,"d)",cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()

#####Z.L seasonal#########################################################################
jpeg(file="Fig/zl_seasonsd.jpg",width=3600,height=3060,res=360, quality=100)
mat2 <- matrix(c(1,2,0,0,3,4),nrow=2, ncol=3)
layout(mat2, widths = c(20,0.1,20), heights = rep.int(5,nrow(mat2)), respect = FALSE)
par(family="serif", mar=c(1.8,2.4,0,0), tcl=-0.6, mgp=c(1.4,1.1,0),omi=c(0.35,0.35,0.2,0.35))

plot(data_group_season$hour[which(data_group_season$season=='1')],data_group_season$gZ.L[which(data_group_season$season=='1')],lwd=1.5,
     type='l',ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='1')], 
          rev(data_group_season$hour[which(data_group_season$season=='1')])), 
        c(data_group_season$possdZ.L[which(data_group_season$season=='1')],
          rev(data_group_season$negsdZ.L[which(data_group_season$season=='1')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
text(0.8,4.5,"a)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='2')],data_group_season$gZ.L[which(data_group_season$season=='2')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',cex.axis=2.0)
polygon(c(data_group_season$hour[which(data_group_season$season=='2')], 
          rev(data_group_season$hour[which(data_group_season$season=='2')])), 
        c(data_group_season$possdZ.L[which(data_group_season$season=='2')],
          rev(data_group_season$negsdZ.L[which(data_group_season$season=='2')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
text(0.8,4.5,"b)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)
mtext(expression(paste(zeta)),side=2,line=2.5,outer= F,at=5,adj=0,family="serif",cex=1.6)
mtext("Hour (local time)",side=1,line=2.8,outer= F,at=19.5,adj=0,family="serif",cex=1.6)

plot(data_group_season$hour[which(data_group_season$season=='3')],data_group_season$gZ.L[which(data_group_season$season=='3')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='3')], 
          rev(data_group_season$hour[which(data_group_season$season=='3')])), 
        c(data_group_season$possdZ.L[which(data_group_season$season=='3')],
          rev(data_group_season$negsdZ.L[which(data_group_season$season=='3')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=1,at=NULL,labels=FALSE)
axis(side=2,at=NULL,labels=FALSE)
text(0.8,4.5,"c)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

plot(data_group_season$hour[which(data_group_season$season=='4')],data_group_season$gZ.L[which(data_group_season$season=='4')],lwd=1.5, type='l',
     ylim=c(-5,5),ylab =NA, xlab=NA,xaxt='n',yaxt='n')
polygon(c(data_group_season$hour[which(data_group_season$season=='4')], 
          rev(data_group_season$hour[which(data_group_season$season=='4')])), 
        c(data_group_season$possdZ.L[which(data_group_season$season=='4')],
          rev(data_group_season$negsdZ.L[which(data_group_season$season=='4')])), col=adjustcolor("grey",alpha.f=0.35), border = NA)
axis(side=2,at=NULL, labels=FALSE)
text(0.8,4.5,"d)",cex=2.0)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2.0)

dev.off()
rm(mat2)