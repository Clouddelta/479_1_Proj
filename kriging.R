
gdata1 = parking %>%
  dplyr::select(Latitude, Longitude,AvgTimeToPark)%>%
  group_by(Latitude,Longitude)%>%
  summarise(AvgTimeToPark=mean(AvgTimeToPark)) ## some duplicated coordinates, take average

coordinates(gdata1) <- ~ Longitude+Latitude 

# TODO: automate grid selection
x.range <- c(-130,-65)
y.range <- c(23,50)

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=1),
                   y=seq(from=y.range[1], to=y.range[2], by=1))

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

map_states = map_data('state')
#start with the base-plot and add the Kriged data to it

rm(g_kriged, g_vgm, g_fit, gdata1, grd, x.range, y.range); gc()