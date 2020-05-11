require(shiny)
require(shinythemes)
require(leaflet)
require(ggplot2)

data <- read.csv("assignment-02-data-formated.csv",header = TRUE);

#set up factors
data$coralType <- as.factor(data$coralType);
#ordering
data$location<-factor(data$location, levels = unique(data$location[order(data$latitude, decreasing = TRUE)]))
#convert values column to decimal format
data$value <- as.numeric(sub("%", "",data$value,fixed=TRUE))/100

ui <- fluidPage(
  theme = shinytheme("superhero"),
  h2("ASSIGNMENT 2: FIT5147"),
  h2("STUDENT Name: Anirban Roy Chowdhury"),
  h3("STUDENT ID: 30539676"),
  titlePanel("Bleaching % according to site"),
  
  #Side bar input selection
  sidebarLayout(
    sidebarPanel(
      #Input selection for corals
      selectInput("coralSel",label = "Choose a coral Type to inspect",
                  list("Blue Corals" = "blue corals", "Hard Corals" = "hard corals", "Sea Fans" = "sea fans",
                       "Sea Pens" = "sea pens", "Soft Corals" = "soft corals", "Blue Corals" = "blue corals", "Hard Corals" = "hard corals"), 
                  selected = "Blue Corals"),
      #Input selection for line smoothening 
      selectInput("methodSel",label = "Choose a the method for line smoothing",
                  list("lm"="lm","loess"="loess","gam"="gam","glm"="glm"), 
                  selected = "lm"),
    ),
    #Main panel
    mainPanel(
      #Selected coral
      plotOutput("selected_site"),
      #Reactive map showing the coral types in site locations.
      leafletOutput("site_loc")
    )
  )
)

server <- function(input, output) {
  #Plot showing the graph for a particular coral type.
  output$selected_site <- renderPlot({
    ggplot(data[data$coralType==input$coralSel,],aes(year,value))+facet_grid(coralType~location)+labs(x="Year",y="Bleaching %")+theme_bw()+geom_point()+geom_smooth(aes(color=..y..),method = input$methodSel)
  })

  #Reactive leaflet map showing the different sites where a particular coral type may be found.
  output$site_loc <- renderLeaflet ({
    color_df <- data.frame(stringsAsFactors = T,
                           site = levels(data$location), 
                           color = c('#F4FBEA','#EEF9D7','#DAF1D9','#B6E3DC', '#96D7E1','#83C4DE','#8AADD3','#7C93C1')
    )
    data <- merge(data, color_df, by.x = "location", by.y = "site")
    pal <- colorFactor(palette = "YlGnBu",domain = data$location)
    slicedData = subset(data, data$coralType %in% input$coralSel)
    leaflet(data = slicedData) %>%
      addCircleMarkers(~longitude, ~latitude,popup=~location,radius = 7,
                       color = ~(color),
                       label = ~location,
                       labelOptions = labelOptions(noHide = T)
      ) %>%
      addProviderTiles(provider=providers$Esri.NatGeoWorldMap,options=providerTileOptions(opacity = 1)) %>%
      addLegend(pal=pal,values=data$location, position = "bottomright") 
    
  }
  )
  
  
}
#start shiny server instance
shinyApp(ui, server)
