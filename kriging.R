library(tidyverse)
library(ggplot2)
library(sf)
library(gstat)
library(magrittr)
library(kriging)
library(raster)
library(scales)
library(usmap)

gdata0=read_csv('maindf1.csv')
gdata1=gdata0%>%
  dplyr::select(Latitude, Longitude,AvgTimeToPark)%>%
  group_by(Latitude,Longitude)%>%
  summarise(AvgTimeToPark=mean(AvgTimeToPark))    ## some duplicated coordinates, take average
coordinates(gdata1) <- ~ Longitude+Latitude 
x.range <- c(-130,-65)
y.range <- c(23,50)

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=1),
                   y=seq(from=y.range[1], to=y.range[2], by=1))

## convert grid to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
g_vgm <- variogram(log(AvgTimeToPark+1)~1, gdata1) # calculates sample variogram values 
g_fit <- fit.variogram(g_vgm, model=vgm("Sph")) # fit model
plot(g_vgm, g_fit)
g_kriged <- krige(log(AvgTimeToPark+1) ~ 1, gdata1,
                  grd, model=g_fit)
krig.output=g_kriged%>%as.data.frame()%>%
  mutate(var1.pred=rescale(var1.pred))
names(krig.output)[1:3]<-c("long","lat","var1.pred")
plot<-ggplot(data=state,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
plot+ 
  geom_tile(data=krig.output,aes(fill=var1.pred))+
  geom_path(aes(group=group),colour = "grey40", size=1)+
  scale_fill_gradient(low="#FEEBE2", high="#7A0177")+
  coord_equal()+
  guides(fill=guide_legend(title="How difficult to park"))+
  theme_minimal()
