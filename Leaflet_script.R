require(leaflet)

data <- read.csv("assignment-02-data-formated.csv",header = TRUE);

#set up factors
data$coralType <- as.factor(data$coralType);
data$location <- as.factor(data$location);
#convert values column to decimal format
data$value <- as.numeric(sub("%", "",data$value,fixed=TRUE))/100

#Setting the different colors for different sites.
color_df <- data.frame(stringsAsFactors = T,
                       site = levels(data$location), 
                       color = c('#F4FBEA','#EEF9D7','#DAF1D9','#B6E3DC', '#96D7E1','#83C4DE','#8AADD3','#7C93C1')
)
#merging the color data, so that the site color info can be accessed.
data <- merge(data, color_df, by.x = "location", by.y = "site")
pal <- colorFactor(palette = "YlGnBu",domain = data$location)
leaflet(data) %>%
  addCircleMarkers(~longitude, ~latitude,popup=~location,radius = 7,
                   color = ~(color),
                   label = ~location,
                   labelOptions = labelOptions(noHide = T)
  ) %>%
  addProviderTiles(provider=providers$Esri.NatGeoWorldMap,options=providerTileOptions(opacity = 1)) %>%
  addLegend(pal=pal,values=data$location, position = "bottomright") 