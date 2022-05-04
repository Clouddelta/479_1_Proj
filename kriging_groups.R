kri=function(parking0){
  gdata1 = parking0 %>%
    dplyr::select(Latitude, Longitude,AvgTimeToPark)%>%
    group_by(Latitude,Longitude)%>%
    summarise(AvgTimeToPark=mean(AvgTimeToPark)) ## some duplicated coordinates, take average
  
  coordinates(gdata1) <- ~ Longitude+Latitude 
  
  # TODO: automate grid selection
  x.max=max(parking0$Longitude)
  x.min=min(parking0$Longitude)
  y.max=max(parking0$Latitude)
  y.min=min(parking0$Latitude)
  x.break=(x.max-x.min)/50
  y.break=(y.max-y.min)/50
  x.range <- c(x.min-x.break,x.max+x.break)
  y.range <- c(y.min-y.break,y.max+y.break)
  
  grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=x.break),
                     y=seq(from=y.range[1], to=y.range[2], by=y.break))
  
  ## convert grid to SpatialPixel class
  coordinates(grd) <- ~ x+y
  gridded(grd) <- TRUE
  
  g_vgm <- variogram(log(AvgTimeToPark+1)~1, gdata1) # calculates sample variogram values 
  g_fit <- fit.variogram(g_vgm, model=vgm("Sph")) # fit model
  # plot(g_vgm, g_fit)
  
  g_kriged <- krige(log(AvgTimeToPark+1) ~ 1, gdata1,
                    grd, model=g_fit)
  
  krig.output = as.data.frame(g_kriged) %>%
    transmute(var1.pred=rescale(var1.pred), long=x, lat=y)
  krig.output
}
n0=5
city=parking%>%group_by(City)%>%
  summarise(n=n())%>%arrange(-n)%>%slice(c(1:n0))%>%
  dplyr::select(City)%>%unlist%>%as.character()
parking_city=list(parking%>%filter(City==city[1]),
                  parking%>%filter(City==city[2]),
                  parking%>%filter(City==city[3]),
                  parking%>%filter(City==city[4]),
                  parking%>%filter(City==city[5]))
names(parking_city)=city
kri_groups=map(parking_city,kri)

for(i in 1:n0){
  write.csv(kri_groups[[i]], paste0(city[i],".csv"))
}
