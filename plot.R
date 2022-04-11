plot_leaflet = function(data.selected) {
  pal = colorNumeric("plasma", c(0, 12))
  leaflet(data.selected) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      ~Longitude, ~Latitude, 
      label = ~Geohash, radius=0.2,
      color = ~pal(AvgTimeToPark)) %>%
    addLegend("topright", pal = pal, values = ~AvgTimeToPark,
              title = "Avg Park Time",
              labFormat = labelFormat(suffix = "min"),
              opacity = 1
    )
}

plot_krig = function() {
  ggplot(data=map_states, aes(x=long,y=lat)) + 
    geom_tile(data=krig.output,aes(fill=var1.pred)) +
    geom_path(aes(group=group),colour = "grey40", size=1) +
    scale_fill_gradient(low="#FEEBE2", high="#7A0177") +
    coord_equal() +
    guides(fill=guide_legend(title="How difficult to park")) +
    theme(legend.position = "bottom", plot.margin=margin())
}

plot_bar_top = function(data.selected, n=15) {
  p_big <- data.selected %>%
    slice_max(POPESTIMATE2020, n=n) %>%
    ggplot() +
    geom_bar(
      aes(y = reorder(County, POPESTIMATE2020), x = AvgTimeToPark),
      stat = "identity") +
    scale_x_continuous(limits = c(0,10)) +
    labs(x = "Average time to park",
         y = "County",
         title = "Average Time to Park by County",
         subtitle = "Top 15 and Bottom 15 Populated counties")
  
  p_small <- data.selected %>%
    slice_min(POPESTIMATE2020, n=n) %>%
    ggplot()+
    geom_bar(
      aes(y = reorder(County, POPESTIMATE2020), x = AvgTimeToPark),
      stat = "identity")+
    labs(x = "Average time to park",
         y = "County")
  
  p_big/p_small+
    plot_layout(guides="collect")
}

plot_scatter = function(data.selected) {
  ggplot(data.selected)+
    geom_point(aes(x = AvgTimeToPark, y=POPESTIMATE2020, alpha = PercentSearching))+
    geom_text_repel(aes(x=AvgTimeToPark, y=POPESTIMATE2020, label=County))+
    labs(x = "Average Time to Park",
         y = "Population Estimate (by county 2020)",
         title = "Population vs. Average Time to Park",
         alpha = "Percent Searching",
         subtitle = "By County, Alpha by Percentage of Drivers Searching for Parking")+
    scale_y_continuous(label=comma)
}
