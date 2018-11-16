
library(shiny)
library(rgdal)
library(tidyverse)
library(leaflet)
library(openintro)
library(magrittr)
library(RColorBrewer)

##io connection
library(rsconnect)


rsconnect::setAccountInfo(name='fkdong',
                          token='AE638605902BFA15D0F38847F3E9D232',
                          secret='/VonGbmSuEfvmmKVckHW1o2fG27PGbsy/b2qQIOv')
##io connection




#Shiny Configuration
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#Shape File Read-in
#This is the original Shapefile
states <- rgdal::readOGR("gz_2010_us_040_00_20m", "gz_2010_us_040_00_20m",GDAL1_integer64_policy = TRUE)
#This is the df that we can use join functions
states_df <- as.data.frame(states)
states_df$NAME <- as.character(states_df$NAME)
#This is the datas from Haviland's sample code
dmapR <- read_csv("dmapR.csv")
dmapD <- read_csv("dmapD.csv")
#Data Transformation
dmapD$st_abrev <-abbr2state(dmapD$st_abrev)
dmapR$st_abrev <-abbr2state(dmapR$st_abrev)
dmapD <- na.omit(dmapD)[,-1]
dmapR <- na.omit(dmapR)[,-1]
colnames(dmapD)<- c("NAME","log10(DonationsD)","DonorsD")
colnames(dmapR)<- c("NAME","log10(DonationsR)","DonorsR")
dmapD$`log10(DonationsD)` <- log10(dmapD$`log10(DonationsD)`)
dmapR$`log10(DonationsR)` <- log10(dmapR$`log10(DonationsR)`)

#These are the ready-to-use shapefiles we will use in our maps
statesD <- full_join(states_df,dmapD,by="NAME")
statesR <- full_join(states_df,dmapR,by="NAME")


# Define UI
ui <- fluidPage(
  titlePanel("Election Donation Information by Megha, Sky, Xuan, Yifu"),
  sidebarLayout(
    sidebarPanel(selectInput("DorR","Democrats or Republicans?",
                             c("Democrats","Republicans"),selected="Democrats")
  ),mainPanel(leafletOutput("mymap",width = "100%"))))

# Define server
server <- function(input, output) {
  output$mymap <- renderLeaflet({
  bins <- c(2.5, 3.5, 4, 4.5,5,5.5, 6,6.5, Inf)
  if (input$DorR=="Democrats"){
    pal <- colorBin("YlOrRd", domain =statesD$`log10(DonationsD)` , bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Donors ",
      statesD$NAME, statesD$DonorsD
    ) %>% lapply(htmltools::HTML)
      mymap <- leaflet(states) %>%
        setView(-96, 37.8, 4) %>%
        addTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% addPolygons(fillColor = ~pal(statesD$`log10(DonationsD)`),
                                                                           weight = 2,
                                                                           opacity = 1,
                                                                           color = "white",
                                                                           dashArray = "3",
                                                                           fillOpacity = 0.7,
                                                                           highlight = highlightOptions(
                                                                             weight = 5,
                                                                             color = "#666",
                                                                             dashArray = "",
                                                                             fillOpacity = 0.7,
                                                                             bringToFront = TRUE),
                                                                           label = labels,
                                                                           labelOptions = labelOptions(
                                                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                             textsize = "15px",
                                                                             direction = "auto"))%>% addLegend(pal = pal, values = ~statesD$`log10(DonationsD)`, opacity = 0.7, title = NULL,
                                                                                                               position = "bottomright")
      
    }else {
    pal <- colorBin("YlOrRd", domain =statesR$`log10(DonationsR)` , bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Donors ",
      statesR$NAME, statesR$DonorsR
    ) %>% lapply(htmltools::HTML)
      mymap <- leaflet(states) %>%
        setView(-96, 37.8, 4) %>%
        addTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% addPolygons(fillColor = ~pal(statesR$`log10(DonationsR)`),
                                                                           weight = 2,
                                                                           opacity = 1,
                                                                           color = "white",
                                                                           dashArray = "3",
                                                                           fillOpacity = 0.7,
                                                                           highlight = highlightOptions(
                                                                             weight = 5,
                                                                             color = "#666",
                                                                             dashArray = "",
                                                                             fillOpacity = 0.7,
                                                                             bringToFront = TRUE),
                                                                           label = labels,
                                                                           labelOptions = labelOptions(
                                                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                           textsize = "15px",                                                           direction = "auto"))%>% addLegend(pal = pal, values = ~statesR$`log10(DonationsR)`, opacity = 0.7, title = NULL,
                                                                                                               position = "bottomright")}})}

# Run the application 
shinyApp(ui = ui, server = server)

