#load relevant packages
library(ggplot2)
library(zoo)
library(data.table)
library(cowplot)
library(gamair)
library(mgcv)

#read in the data
data.time<-fread("data/MOGWAI/MOGWAI_timeseries_2001_2020BL.csv")
data.anom<-fread("out/MOGWAI_anomaly_2001_2020BL.csv")

head(data.time)
head(data.anom)

#merge the data
data.m<-merge(data.time,data.anom,all=T)
head(data.m)

#round the lat lon for purpose of hov muller plot
data.m[,':='(lat.round=round(latitude,0),
             lon.round=round(longitude,0))]
data.mean<-data.m[,.(lake_area=mean(DSWE_Area_km2)),.(ID,Year,lat.round)]
head(data.mean)

#interpolate the data to yearly timescale
data.interp<-expand.grid(unique(data.mean$ID),sort(unique(data.mean$Year)))
names(data.interp)<-c("ID","Year")
data.ID<-unique(data.mean[, .SD, .SDcols = c("ID","lat.round")])
data.interp<-merge(data.interp,data.ID)
data.interp<-merge(data.interp,data.mean,all=T)
head(data.interp)
summary(data.interp)
data.interp<-data.table(data.interp)
data.interp<-data.interp[,lake_area.interp:=na.approx(lake_area,Year,na.rm=F,rule = 2),by=ID]
summary(data.interp)

#sum the lake_storage values and calcualt proportion mean
data.sum<-data.interp[,.(lake_area=sum(lake_area.interp)),.(Year,lat.round)]
data.sum[,':='(lake_area.mean=mean(lake_area)),.(lat.round)]
data.sum[,':='(lake_area.propmean=lake_area/lake_area.mean),.(lat.round)]
head(data.sum)

#plot raw values
ggplot()+
  geom_tile(data=data.sum,aes(x=Year,y=lat.round,fill=lake_area.propmean))+
  scale_fill_viridis_c(option = "turbo",limits=c(0.3,1.7),direction = -1)+
  theme_bw()+
  labs(y="Latitude",
       x="Year",
       fill="Lake storage
(proportion of mean)")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))

#smooth the hov muller plot with a gam
fit.gam<-gam(data=data.sum,lake_area.propmean~s(Year,lat.round,k=100),method="REML",weights = data.sum$lake_area.mean)
summary(fit.gam)
plot(fit.gam)
  
data.exp<-data.table(expand.grid(c(-45:73),sort(unique(data.mean$Year))))
names(data.exp)<-c("lat.round","Year")
data.exp[,lake_area.propmean.predict:=predict.gam(newdata=data.exp,fit.gam)]
head(data.exp)

volume_breaks = c(seq(90, 110, 5))

hovmuller_plot = ggplot()+
  geom_tile(data=data.exp,aes(x=Year,y=lat.round,fill=lake_area.propmean.predict*100))+
  geom_contour(data=data.exp,aes(x=Year,y=lat.round,z=lake_area.propmean.predict*100),colour="black",bins=12)+
  scale_fill_fermenter(breaks = volume_breaks, palette = "BrBG", name = "MOGWAI Area Anomaly from 2001-2020 (%)", direction = 1)+
  theme_bw()+
  labs(y="Latitude",
       x="Year",
       fill="Lake area
(proportion of mean)")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  guides(fill = guide_colorbar(
    title.position = "bottom",
    frame.linewidth = 0.55,
    frame.colour = "black",
    ticks.colour = "black",
    ticks.linewidth = 0.3,
    barwidth = 10
  ),
  color = "none") +
  theme_light() +
  theme(legend.position = "bottom")

pdf(file = "out/MOGWAI_Hovmuller.pdf", width = 6, height = 4)
plot(hovmuller_plot)
dev.off()
