#####Plotting diurnal meteorological parameters#####
#####preparing the data (1 year period)#####
df_new2 <- df_new[which
                  (df_new$time_stamp >= as.POSIXct('2013-09-10 18:26:00') &
                     df_new$time_stamp < as.POSIXct('2014-10-1 00:00:00')),1:155]

######add season column in df_new2######
season <- matrix(rep(3,18491),nrow=18491,ncol=1)
season <- as.data.frame (season)
colnames(season) <- "season"

df_timestamp <- df_new2$time_stamp[1:18491]
df_timestamp <- as.data.frame(df_timestamp)
season <- cbind(df_timestamp,season)

season$season[which(season$df_timestamp > as.POSIXct('2013-11-30 23:30:00') & 
                      season$df_timestamp < as.POSIXct('2014-04-01 00:00:00'))] <- 1
season$season[which(season$df_timestamp > as.POSIXct('2014-03-31 23:30:00') &
                      season$df_timestamp < as.POSIXct('2014-06-01 00:00:00'))] <- 2
season$season[which(season$df_timestamp > as.POSIXct('2013-09-30 23:30:00') &
                      season$df_timestamp < as.POSIXct('2013-12-01 00:00:00'))] <- 4

season <- season$season[1:18491]
season <- as.data.frame (season)
colnames(season) <- "season"
df_new2 <- cbind(df_new2,season)

rm(season,df_timestamp)

#####Create data_group for diurnal plots with stndrd deviation shades#####
#install.packages('ggplot2')
#install.packages("dplyr")
#install.packages("Hmisc")
library('ggplot2')
library('dplyr')
library('Hmisc')
data_group <- df_new2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp))%>%
  group_by(season,hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'))%>%
  
  summarise(ta_1=mean(Ta_1_1_1,na.rm=TRUE),
            ta1_up=mean(Ta_1_1_1,na.rm=TRUE)+sd(Ta_1_1_1,na.rm=TRUE),
            ta1_down=mean(Ta_1_1_1,na.rm=TRUE)-sd(Ta_1_1_1,na.rm=TRUE),
            ta_2=mean(Ta_2_1_1,na.rm=TRUE),
            ta2_up=mean(Ta_2_1_1,na.rm=TRUE)+sd(Ta_2_1_1,na.rm=TRUE),
            ta2_down=mean(Ta_2_1_1,na.rm=TRUE)-sd(Ta_2_1_1,na.rm=TRUE),
            ta_3=mean(Ta_3_1_1,na.rm=TRUE),
            ta3_up=mean(Ta_3_1_1,na.rm=TRUE)+sd(Ta_3_1_1,na.rm=TRUE),
            ta3_down=mean(Ta_3_1_1,na.rm=TRUE)-sd(Ta_3_1_1,na.rm=TRUE),
            ta_4=mean(Ta_4_1_1,na.rm=TRUE),
            ta4_up=mean(Ta_4_1_1,na.rm=TRUE)+sd(Ta_4_1_1,na.rm=TRUE),
            ta4_down=mean(Ta_4_1_1,na.rm=TRUE)-sd(Ta_4_1_1,na.rm=TRUE),
            ta_5=mean(Ta_5_1_1,na.rm=TRUE), 
            ta5_up=mean(Ta_5_1_1,na.rm=TRUE)+sd(Ta_5_1_1,na.rm=TRUE),
            ta5_down=mean(Ta_5_1_1,na.rm=TRUE)-sd(Ta_5_1_1,na.rm=TRUE),
            rh_1=mean(RH_1_1_1,na.rm=TRUE),
            rh1_up=mean(RH_1_1_1,na.rm=TRUE)+sd(RH_1_1_1,na.rm=TRUE),
            rh1_down=mean(RH_1_1_1,na.rm=TRUE)-sd(RH_1_1_1,na.rm=TRUE),
            rh_2=mean(RH_2_1_1,na.rm=TRUE),
            rh2_up=mean(RH_2_1_1,na.rm=TRUE)+sd(RH_2_1_1,na.rm=TRUE),
            rh2_down=mean(RH_2_1_1,na.rm=TRUE)-sd(RH_2_1_1,na.rm=TRUE),
            rh_3=mean(RH_3_1_1,na.rm=TRUE),
            rh3_up=mean(RH_3_1_1,na.rm=TRUE)+sd(RH_3_1_1,na.rm=TRUE),
            rh3_down=mean(RH_3_1_1,na.rm=TRUE)-sd(RH_3_1_1,na.rm=TRUE),
            rh_4=mean(RH_4_1_1,na.rm=TRUE),
            rh4_up=mean(RH_4_1_1,na.rm=TRUE)+sd(RH_4_1_1,na.rm=TRUE),
            rh4_down=mean(RH_4_1_1,na.rm=TRUE)-sd(RH_4_1_1,na.rm=TRUE),
            rh_5=mean(RH_5_1_1,na.rm=TRUE),
            rh5_up=mean(RH_5_1_1,na.rm=TRUE)+sd(RH_5_1_1,na.rm=TRUE),
            rh5_down=mean(RH_5_1_1,na.rm=TRUE)-sd(RH_5_1_1,na.rm=TRUE),
            rn=mean(Rn_1_1_1,na.rm=TRUE),
            rn_up=mean(Rn_1_1_1,na.rm=TRUE)+sd(Rn_1_1_1,na.rm=TRUE),
            rn_down=mean(Rn_1_1_1,na.rm=TRUE)-sd(Rn_1_1_1,na.rm=TRUE),
            par_1=mean(PPFD_1_1_1,na.rm=TRUE),
            par1_up=mean(PPFD_1_1_1,na.rm=TRUE)+sd(PPFD_1_1_1,na.rm=TRUE),
            par1_down=mean(PPFD_1_1_1,na.rm=TRUE)-sd(PPFD_1_1_1,na.rm=TRUE),
            par_2=mean(PPFD_2_1_1,na.rm=TRUE),
            par2_up=mean(PPFD_2_1_1,na.rm=TRUE)+sd(PPFD_2_1_1,na.rm=TRUE),
            par2_down=mean(PPFD_2_1_1,na.rm=TRUE)-sd(PPFD_2_1_1,na.rm=TRUE),
            par_3=mean(PPFD_3_1_1,na.rm=TRUE),
            par3_up=mean(PPFD_3_1_1,na.rm=TRUE)+sd(PPFD_3_1_1,na.rm=TRUE),
            par3_down=mean(PPFD_3_1_1,na.rm=TRUE)-sd(PPFD_3_1_1,na.rm=TRUE),
            rain=mean(Prain_1_1_1,na.rm=TRUE),
            ws=mean(WS_1_1_1,na.rm=TRUE),
            ws_up=mean(WS_1_1_1,na.rm=TRUE)+ sd(WS_1_1_1,na.rm=TRUE),
            ws_down=mean(WS_1_1_1,na.rm=TRUE)- sd(WS_1_1_1,na.rm=TRUE),
            wd=mean(WD_1_1_1,na.rm=TRUE),
            wd_up=mean(WD_1_1_1,na.rm=TRUE)+ sd(WD_1_1_1,na.rm=TRUE),
            wd_down=mean(WD_1_1_1,na.rm=TRUE)- sd(WD_1_1_1,na.rm=TRUE),
            swc_1=mean(SWC_1_1_1,na.rm=TRUE),
            swc_2=mean(SWC_2_1_1,na.rm=TRUE),
            swc_3=mean(SWC_3_1_1,na.rm=TRUE),
            shf_1=mean(SHF_1_1_1,na.rm=TRUE),
            shf_2=mean(SHF_2_1_1,na.rm=TRUE),
            shf_3=mean(SHF_3_1_1,na.rm=TRUE),
            st_1=mean(Ts_1_1_1,na.rm=TRUE),
            st_2=mean(Ts_2_1_1,na.rm=TRUE),
            st_3=mean(Ts_3_1_1,na.rm=TRUE),
            swc_m=mean(df_swc,na.rm=TRUE),
            swc_up=mean(df_swc,na.rm=TRUE)+sd(df_swc,na.rm=TRUE),
            swc_down=mean(df_swc,na.rm=TRUE)-sd(df_swc,na.rm=TRUE),    
            shf_m=mean(df_shf,na.rm=TRUE),
            shf_up=mean(df_shf,na.rm=TRUE)+sd(df_shf,na.rm=TRUE),
            shf_down=mean(df_shf,na.rm=TRUE)-sd(df_shf,na.rm=TRUE),
            ts_m=mean(df_ts,na.rm=TRUE),
            ts_up=mean(df_ts,na.rm=TRUE)+sd(df_ts,na.rm=TRUE),
            ts_down=mean(df_ts,na.rm=TRUE)-sd(df_ts,na.rm=TRUE))

#####rainfall timeseries data####
data_group2 <- df_new2 %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(day=format(as.POSIXlt(cut(time_stamp,breaks='day')), '%d'),
           month=format(as.POSIXlt(cut(time_stamp,breaks='month')), '%m'),
           season) %>%

data_group2$month <- as.character(data_group2$month)
data_group2$day <- as.character (data_group2$day)
data_group2 <- data_group2[order(data_group2$month,data_group2$day),]

####adding year column in data_group2
#create year column
year <- matrix(rep(2014,366),nrow=366,ncol=1)
year <- as.data.frame (year)
colnames(year) <- "year"

df_month <- data_group2$month[1:366]
df_month <- as.data.frame(df_month)
year <- cbind(df_month,year)

year$df_month <- as.character(year$df_month)
year$year <- as.character(year$year)

year$year[275:366] <- '2013'
year1 <- year$year[2:366]
year <- as.data.frame (year1)
data_group2 <- data_group2[c(-274),]
data_group2 <- cbind(data_group2,year1)
data_group2$year1 <- as.character(data_group2$year)

date <- paste(data_group2$day,data_group2$month,data_group2$year)

data_group2$month <- date
data_group2 <- data_group2[,c(-1,-5)]
colnames(data_group2)[1] <- 'date'
rm(year,df_month,date,year1)

date <- as.POSIXct(data_group2$date,format = "%d %m %Y",tz ="GMT")
data_group2$date <- date
rm(date)

###change nan to NA
foo <- as.numeric(data_group2$rain)
foo[is.nan(foo)] <- NA
data_group2$rain <- foo
rm(foo)

#clean up global env.
rm(df_timestamp,season,date)

######WindRose plot#####
#install.packages ('openair')
library ('openair')

#monthly
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/windrose_monthly.jpg", width=3060, height= 3060,res=360,family = "serif")
colnames(df_new2)[1]<- "date"
date <-as.POSIXct(df_new2$date)
df_new2$date <- date
windRose(df_new2,ws="WS_1_1_1", wd="WD_1_1_1",
         layout=c(3,4),type="month",key = TRUE,
         grid.line=20,breaks = c(2, 4, 6, 8,10),paddle = FALSE,
         key.footer = "Wind Speed (m/s)",annotate = FALSE,
         par.settings=list(fontsize=list(text=18)))
colnames(df_new2)[1]<- "time_stamp"
rm(date)
dev.off()

#season
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/windrose_season.jpg", width=3060, height= 3060,res=360,family = "serif")
windRose(df_new2,ws="WS_1_1_1", wd="WD_1_1_1",
         layout=c(3,4),type="season",key = TRUE,
         grid.line=20,breaks = c(2, 4, 6, 8,10),paddle = FALSE,
         key.footer = "Wind Speed (m/s)",annotate = FALSE,
         par.settings=list(fontsize=list(text=18)))

#####seasonal plot######
#install.packages('Hmisc')
library('Hmisc')
####air temp####
#####level 1####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_air_temp1_f2.jpeg", width=3060, height= 3060,res=360,family = 'serif')
par(family='serif',mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)

plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ta_1[which(data_group$season=='1')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)
##sd shades
x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ta1_down[which(data_group$season=='1')]
y_up <- data_group$ta1_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ta_1[which(data_group$season=='2')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ta1_down[which(data_group$season=='2')]
y_up <- data_group$ta1_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"b)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ta_1[which(data_group$season=='3')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ta1_down[which(data_group$season=='3')]
y_up <- data_group$ta1_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"c)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ta_1[which(data_group$season=='4')],
     type='l',ylim=c(20,36),xlim=c(0,23),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ta1_down[which(data_group$season=='4')]
y_up <- data_group$ta1_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"d)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)


mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 2####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_air_temp2_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)

plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ta_2[which(data_group$season=='1')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ta2_down[which(data_group$season=='1')]
y_up <- data_group$ta2_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ta_2[which(data_group$season=='2')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ta2_down[which(data_group$season=='2')]
y_up <- data_group$ta2_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"b)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ta_2[which(data_group$season=='3')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ta2_down[which(data_group$season=='3')]
y_up <- data_group$ta2_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"c)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ta_2[which(data_group$season=='4')],
     type='l',ylim=c(20,36),xlim=c(0,23),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ta2_down[which(data_group$season=='4')]
y_up <- data_group$ta2_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"d)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 3####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_air_temp3_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)

plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ta_3[which(data_group$season=='1')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ta3_down[which(data_group$season=='1')]
y_up <- data_group$ta3_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ta_3[which(data_group$season=='2')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ta3_down[which(data_group$season=='2')]
y_up <- data_group$ta3_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"b)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ta_3[which(data_group$season=='3')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ta3_down[which(data_group$season=='3')]
y_up <- data_group$ta3_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"c)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ta_3[which(data_group$season=='4')],
     type='l',ylim=c(20,36),xlim=c(0,23),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ta3_down[which(data_group$season=='4')]
y_up <- data_group$ta3_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"d)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24v),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 4####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_air_temp4_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)

plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ta_4[which(data_group$season=='1')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ta4_down[which(data_group$season=='1')]
y_up <- data_group$ta4_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ta_4[which(data_group$season=='2')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ta4_down[which(data_group$season=='2')]
y_up <- data_group$ta4_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"b)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ta_4[which(data_group$season=='3')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ta4_down[which(data_group$season=='3')]
y_up <- data_group$ta4_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"c)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ta_4[which(data_group$season=='4')],
     type='l',ylim=c(20,36),xlim=c(0,23),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ta4_down[which(data_group$season=='4')]
y_up <- data_group$ta4_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"d)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 5####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_air_temp5_f2.jpeg", width=3060, height= 3060,res=360)
par(family='serif',mfcol=c(2,2),mai=c(0.7,0.7,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)

plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ta_5[which(data_group$season=='1')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ta5_down[which(data_group$season=='1')]
y_up <- data_group$ta5_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ta_5[which(data_group$season=='2')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ta5_down[which(data_group$season=='2')]
y_up <- data_group$ta5_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"b)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ta_5[which(data_group$season=='3')],
     type='l',ylim=c(20,36),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ta5_down[which(data_group$season=='3')]
y_up <- data_group$ta5_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"c)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ta_5[which(data_group$season=='4')],
     type='l',ylim=c(20,36),xlim=c(0,23),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt ='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ta5_down[which(data_group$season=='4')]
y_up <- data_group$ta5_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,35,"d)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(20,25,30,35),cex.axis=1.7,labels=F)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)

####Relative HUmidity####
#####level 1####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_rel_humidity1_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rh_1[which(data_group$season=='1')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rh1_down[which(data_group$season=='1')]
y_up <- data_group$rh1_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rh_1[which(data_group$season=='2')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rh1_down[which(data_group$season=='2')]
y_up <- data_group$rh1_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"b)",cex = 1.7)      
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rh_1[which(data_group$season=='3')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rh1_down[which(data_group$season=='3')]
y_up <- data_group$rh1_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"c)",cex = 1.7)    
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rh_1[which(data_group$season=='4')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rh1_down[which(data_group$season=='4')]
y_up <- data_group$rh1_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('RH (%)')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 2####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_rel_humidity2_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rh_2[which(data_group$season=='1')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rh2_down[which(data_group$season=='1')]
y_up <- data_group$rh2_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rh_2[which(data_group$season=='2')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rh2_down[which(data_group$season=='2')]
y_up <- data_group$rh2_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"b)",cex = 1.7)      
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rh_2[which(data_group$season=='3')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rh2_down[which(data_group$season=='3')]
y_up <- data_group$rh2_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"c)",cex = 1.7)    
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rh_2[which(data_group$season=='4')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rh2_down[which(data_group$season=='4')]
y_up <- data_group$rh2_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)


mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('RH (%)')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 3####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_rel_humidity3_f2.jpeg", width=3060, height= 3060,res=360,famil='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rh_3[which(data_group$season=='1')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rh3_down[which(data_group$season=='1')]
y_up <- data_group$rh3_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rh_3[which(data_group$season=='2')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rh3_down[which(data_group$season=='2')]
y_up <- data_group$rh3_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"b)",cex = 1.7)      
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rh_3[which(data_group$season=='3')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rh3_down[which(data_group$season=='3')]
y_up <- data_group$rh3_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"c)",cex = 1.7)    
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rh_3[which(data_group$season=='4')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rh3_down[which(data_group$season=='4')]
y_up <- data_group$rh3_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)


mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('RH (%)')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 4####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_rel_humidity4_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rh_4[which(data_group$season=='1')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rh4_down[which(data_group$season=='1')]
y_up <- data_group$rh4_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rh_4[which(data_group$season=='2')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rh4_down[which(data_group$season=='2')]
y_up <- data_group$rh4_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"b)",cex = 1.7)      
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rh_4[which(data_group$season=='3')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rh4_down[which(data_group$season=='3')]
y_up <- data_group$rh4_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"c)",cex = 1.7)    
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rh_4[which(data_group$season=='4')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rh4_down[which(data_group$season=='4')]
y_up <- data_group$rh4_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)


mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('RH (%)')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
#####level 5####
#season 1 
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_rel_humidity5_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rh_5[which(data_group$season=='1')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rh5_down[which(data_group$season=='1')]
y_up <- data_group$rh5_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"a)",cex = 1.7)     
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
#season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rh_5[which(data_group$season=='2')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rh5_down[which(data_group$season=='2')]
y_up <- data_group$rh5_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"b)",cex = 1.7)      
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7)
#season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rh_5[which(data_group$season=='3')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rh5_down[which(data_group$season=='3')]
y_up <- data_group$rh5_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"c)",cex = 1.7)    
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)

#season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rh_5[which(data_group$season=='4')],
     type='l',ylim=c(50,110),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rh5_down[which(data_group$season=='4')]
y_up <- data_group$rh5_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3, 0.3, 0.3,0.5), border = NA)

text(0,109,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(50,75,100),cex.axis=1.7,labels=F)


mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('RH (%)')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)

####net radiaton####
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_net_rad_f2.jpeg", width=3060, height= 3060,res=360)
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
##season 1
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$rn[which(data_group$season=='1')],
     type='l',ylim=c(-50,800),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$rn_down[which(data_group$season=='1')]
y_up <- data_group$rn_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,795,"a)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600,800),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$rn[which(data_group$season=='2')],
     type='l',ylim=c(-50,800),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$rn_down[which(data_group$season=='2')]
y_up <- data_group$rn_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,795,"b)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600,800),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$rn[which(data_group$season=='3')],
     type='l',ylim=c(-50,800),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$rn_down[which(data_group$season=='3')]
y_up <- data_group$rn_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,795,"c)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600,800),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$rn[which(data_group$season=='4')],
     type='l',ylim=c(-50,800),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$rn_down[which(data_group$season=='4')]
y_up <- data_group$rn_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,795,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600,800),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('R'['n'], ' (W m'^{'-2'}, ')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)

####PPFD####
#####level 1#####
##correction, nan value to 0

z<-data_group$par_1
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par_1 <- y

z<-data_group$par1_up
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par1_up <- y

z<-data_group$par1_down
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par1_down <- y

rm(dim,z)

x <- data_group$par1_up
x[is.na(x)] <- 0
data_group$par1_up <- x

x <- data_group$par1_down
x[is.na(x)] <- 0
data_group$par1_down <- x
##season 1
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_ppfd1_f.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$par_1[which(data_group$season=='1')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$par1_down[which(data_group$season=='1')]
y_up <- data_group$par1_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"a)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$par_1[which(data_group$season=='2')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$par1_down[which(data_group$season=='2')]
y_up <- data_group$par1_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"b)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$par_1[which(data_group$season=='3')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$par1_down[which(data_group$season=='3')]
y_up <- data_group$par1_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"c)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7,labels=FALSE)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$par_1[which(data_group$season=='4')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$par1_down[which(data_group$season=='4')]
y_up <- data_group$par1_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7,labels=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('PPFD',' (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_down,y_up,y)
#####level 2#####
z<-data_group$par_2
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par_2 <- y

z<-data_group$par2_up
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par2_up <- y

z<-data_group$par2_down
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par2_down <- y

rm(dim,z)

##season 1
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_ppfd2_f.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$par_2[which(data_group$season=='1')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$par2_down[which(data_group$season=='1')]
y_up <- data_group$par2_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"a)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$par_2[which(data_group$season=='2')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$par2_down[which(data_group$season=='2')]
y_up <- data_group$par2_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"b)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$par_2[which(data_group$season=='3')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$par2_down[which(data_group$season=='3')]
y_up <- data_group$par2_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"c)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7,labels=FALSE)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$par_2[which(data_group$season=='4')],
     type='l',ylim=c(-10,1700),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$par2_down[which(data_group$season=='4')]
y_up <- data_group$par2_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,1690,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(0,500,1000,1500),cex.axis=1.7,labels=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('PPFD',' (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_down,y_up,y)
#####level 3#####
z<-data_group$par_3
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par_3 <- y

z<-data_group$par3_up
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par3_up <- y

z<-data_group$par3_down
dim <- dim(z)
y <- unlist(z)
y[ is.nan(y) ] <- 0
data_group$par3_down <- y

rm(dim,z)

##season 1
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_ppfd3_f.jpg", width=3060, height= 3060,res=360,famil='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$par_3[which(data_group$season=='1')],
     type='l',ylim=c(-50,600),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$par3_down[which(data_group$season=='1')]
y_up <- data_group$par3_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,600,"a)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$par_3[which(data_group$season=='2')],
     type='l',ylim=c(-50,600),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$par3_down[which(data_group$season=='2')]
y_up <- data_group$par3_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,600,"b)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,200,400,600),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$par_3[which(data_group$season=='3')],
     type='l',ylim=c(-90,600),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$par3_down[which(data_group$season=='3')]
y_up <- data_group$par3_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,600,"c)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)
axis(side=2,at=c(0,200,400,600),cex.axis=1.7,labels=FALSE)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$par_3[which(data_group$season=='4')],
     type='l',ylim=c(-50,600),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$par3_down[which(data_group$season=='4')]
y_up <- data_group$par3_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,600,"d)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(0,200,400,600),cex.axis=1.7,labels=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('PPFD',' (',mu,'mol ','m'^{'-2'}, ' s'^{'-1'},')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_down,y_up,y)

####Soil condition plots####
####soil heat flux####
##season 1
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_shf_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$shf_m[which(data_group$season=='1')],
     type='l',ylim=c(-25,45),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$shf_down[which(data_group$season=='1')]
y_up <- data_group$shf_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,44,"a)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(-25,0,20,40),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$shf_m[which(data_group$season=='2')],
     type='l',ylim=c(-25,45),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$shf_down[which(data_group$season=='2')]
y_up <- data_group$shf_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,44,"b)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(-25,0,20,40),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$shf_m[which(data_group$season=='3')],
     type='l',ylim=c(-25,45),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$shf_down[which(data_group$season=='3')]
y_up <- data_group$shf_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,44,"c)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(-25,0,20,40),cex.axis=1.7,labels=FALSE)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$shf_m[which(data_group$season=='4')],
     type='l',ylim=c(-25,45),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$shf_down[which(data_group$season=='4')]
y_up <- data_group$shf_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,44,"d)",cex = 1.7)       
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(-25,0,20,40),cex.axis=1.7,labels=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('G (W m', ' '^{'-2'}, ')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
####soil water content####
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_swc_f2.jpeg", width=3060, height= 3060,res=360)
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
##season 1 
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$swc_m[which(data_group$season=='1')],
     type='l',ylim=c(0.1,0.3),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt= 'n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$swc_down[which(data_group$season=='1')]
y_up <- data_group$swc_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,0.3,"a)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=2,at=c(0.1,0.2,0.3),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$swc_m[which(data_group$season=='2')],
     type='l',ylim=c(0.1,0.3),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt= 'n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$swc_down[which(data_group$season=='2')]
y_up <- data_group$swc_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,0.3,"b)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(0.1,0.2,0.3),cex.axis=1.7)

##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$swc_m[which(data_group$season=='3')],
     type='l',ylim=c(0.1,0.3),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt= 'n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$swc_down[which(data_group$season=='3')]
y_up <- data_group$swc_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,0.3,"c)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=2,at=c(0.1,0.2,0.3),cex.axis=1.7,labels=FALSE)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$swc_m[which(data_group$season=='4')],
     type='l',ylim=c(0.1,0.3),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt= 'n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$swc_down[which(data_group$season=='4')]
y_up <- data_group$swc_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,0.3,"d)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(0.1,0.2,0.3),cex.axis=1.7,labels=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('SWC',' (m'^{'3'},' m'^{'-3'},')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)

dev.off()
rm(x,y_down,y_up)
#####soil temperature####
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_st_f2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
##season 1
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ts_m[which(data_group$season=='1')], 
     type="l",lty=2,yaxt='n',xaxt='n',ylab='',xlab='',lwd=2,
     ylim=c(24,29))

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ts_down[which(data_group$season=='1')]
y_up <- data_group$ts_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,28,"a)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=2,at=c(24,26,28),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

###season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ts_m[which(data_group$season=='2')], 
     type="l",lty=2, yaxt='n',xaxt='n',ylab='',xlab='',lwd=2,
     ylim=c(24,29))


x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ts_down[which(data_group$season=='2')]
y_up <- data_group$ts_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)
text(0,28,"b)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(24,26,28),cex.axis=1.7)

###season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ts_m[which(data_group$season=='3')], 
     type="l",lty=2,yaxt='n',xaxt='n',ylab='',xlab='',lwd=2,
     ylim=c(24,29))

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ts_down[which(data_group$season=='3')]
y_up <- data_group$ts_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,28,"c)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=2,at=c(24,26,28),cex.axis=1.7,label=FALSE)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=FALSE)

###season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ts_m[which(data_group$season=='4')], 
     type="l",lty=2,yaxt='n',xaxt='n',ylab='',xlab='',lwd=2,
     ylim=c(24,29))

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ts_down[which(data_group$season=='4')]
y_up <- data_group$ts_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,28,"d)",cex = 1.7)       
minor.tick(nx=5,ny=1,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
axis(side=2,at=c(24,26,28),cex.axis=1.7,label=FALSE)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('T'['s'],' ('^{'o'}, ' C',')')),
      side=2,line = 2, outer =TRUE, at = 0.5,cex = 1.7)
rm(x,y_down,y_up)

dev.off()

#####Rainfall plot in timeseries#####
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/Rainfall_v2.jpeg", width=3060, height= 3060,res=360,family='serif')
par(family = 'serif',mar=c(2,2,1,1),omi=c(0.5,0.6,0,0),mai = c(0.4,0.3, 0.1, 0.1))
layout(matrix(c(1,1,2,3,4,4,5,6), 4, 2, byrow = TRUE))
#season 1
plot(data_group2$date[which(data_group2$season == '1')],
     data_group2$rain[which(data_group2$season == '1')], 
     type = 'h', ylim=c(0,2.5),xlab="",
     ylab= '',xaxt='n',yaxt='n',cex.axis=1.7,lwd=3)

minor.tick(ny=5,nx=1,tick.ratio=0.5)
text(as.POSIXct('2013-12-01'),2.3,"a)",cex = 1.7)     

axis(side=2, c(0,1,2),cex.axis=1.7)
axis.POSIXct(side=1, data_group2$date,
             at=c(as.POSIXct('2013-12-01'),
                  as.POSIXct('2014-01-01'),
                  as.POSIXct('2014-02-01'),
                  as.POSIXct('2014-03-01')),
             format= '%b-%Y', labels = TRUE,cex.axis=1.7)

#season 2
plot(data_group2$date[which(data_group2$season == '2')],
     data_group2$rain[which(data_group2$season == '2')], 
     type = 'h', ylim=c(0,2.5),xlab="",
     ylab= '',xaxt='n',yaxt='n',cex.axis=1.7,lwd=3)

minor.tick(ny=5,nx=1,tick.ratio=0.5)
text(as.POSIXct('2014-04-03'),2.3,"b)",cex = 1.7)     

axis(side=2, c(0,1,2),cex.axis=1.7)
axis.POSIXct(side=1, data_group2$date,
             at=c(as.POSIXct('2014-04-01'),
                  as.POSIXct('2014-05-01')),
             format= '%b-%Y', labels = TRUE,cex.axis=1.7)

plot.new()
#season 3
plot(data_group2$date[which(data_group2$season == '3')],
     data_group2$rain[which(data_group2$season == '3')], 
     type = 'h', ylim=c(0,2.5),xlab="",
     ylab= '',xaxt='n',yaxt='n',cex.axis=1.7,lwd=3)

minor.tick(ny=5,nx=1,tick.ratio=0.5)
text(as.POSIXct('2014-06-01'),2.3,"c)",cex = 1.7)     

axis(side=2, c(0,1,2),cex.axis=1.7)
axis.POSIXct(side=1, data_group2$date,
             at=c(as.POSIXct('2014-06-01'),
                  as.POSIXct('2014-07-01'),
                  as.POSIXct('2014-08-01'),
                  as.POSIXct('2014-09-01')),
             format= '%b-%Y', labels = TRUE,cex.axis=1.7)

#season 4
plot(data_group2$date[which(data_group2$season == '4')],
     data_group2$rain[which(data_group2$season == '4')], 
     type = 'h', ylim=c(0,2.5),xlab="",
     ylab= '',xaxt='n',yaxt='n',cex.axis=1.7,lwd=3)

minor.tick(ny=5,nx=1,tick.ratio=0.5)
text(as.POSIXct('2013-10-03'),2.3,"d)",cex = 1.7)     

axis(side=2, c(0,1,2),cex.axis=1.7)
axis.POSIXct(side=1, data_group2$date,
             at=c(as.POSIXct('2013-10-01'),
                  as.POSIXct('2013-11-01')),
             format= '%b-%Y', labels = TRUE,cex.axis=1.7)
plot.new()

mtext("Month", side=1, line = 1, outer=TRUE, at=0.5,cex = 1.7)
mtext("Precipitation (mm)", side=2, line = 1,outer=TRUE, at=0.5,cex = 1.7)
dev.off()

#####windspeed seasonal plot####
###season 1
jpeg(filename = "D:/USM/Sem 6 2015/FYP/New folder/Palm_oil_eddy_covariance/Plots/Final/seasonal_ws_v3.jpeg", width=3060, height= 3060,res=360)
par(family='serif',mfcol=c(2,2),mai=c(0.5,0.5,0.1,0.1),omi=c(1,1,0,0),mar = c(0,0,1,1) + 0.2)
##season 1
plot(data_group$hour[which(data_group$season=='1')], 
     data_group$ws[which(data_group$season=='1')],
     type='l',ylim=c(0,6),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='1')]
y_down <- data_group$ws_down[which(data_group$season=='1')]
y_up <- data_group$ws_up[which(data_group$season=='1')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,6,"a)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,2,4,6),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

##season 2
plot(data_group$hour[which(data_group$season=='2')], 
     data_group$ws[which(data_group$season=='2')],
     type='l',ylim=c(0,6),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='2')]
y_down <- data_group$ws_down[which(data_group$season=='2')]
y_up <- data_group$ws_up[which(data_group$season=='2')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,6,"b)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,2,4,6),cex.axis=1.7)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)
##season 3
plot(data_group$hour[which(data_group$season=='3')], 
     data_group$ws[which(data_group$season=='3')],
     type='l',ylim=c(0,6),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='3')]
y_down <- data_group$ws_down[which(data_group$season=='3')]
y_up <- data_group$ws_up[which(data_group$season=='3')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,6,"c)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,2,4,6),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7,labels=F)

##season 4
plot(data_group$hour[which(data_group$season=='4')], 
     data_group$ws[which(data_group$season=='4')],
     type='l',ylim=c(0,6),xlab="",
     ylab= '',
     lwd=2,xaxt='n',yaxt='n',cex.axis=1.7)

x <- data_group$hour[which(data_group$season=='4')]
y_down <- data_group$ws_down[which(data_group$season=='4')]
y_up <- data_group$ws_up[which(data_group$season=='4')]

polygon(c(x, rev(x)), c(y_up, rev(y_down)),
        col=rgb(0.3,0.3,0.3,0.5), border = NA)

text(0,6,"d)",cex = 1.7)       
minor.tick(ny=1,nx=5,tick.ratio=0.5)
axis(side=2,at=c(0,2,4,6),cex.axis=1.7,labels=F)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=1.7)

mtext("Hours (local time)", side=1, line = 3, outer=TRUE, at=0.5,cex = 1.7)
mtext(expression(paste('Wind Speed (ms', ' '^{'-1'}, ')')), 
      side=2, line = 2,outer=TRUE, at=0.5,cex = 1.7)
dev.off()
rm(x,y_up,y_down)
rm(x,y_up,y_down)
