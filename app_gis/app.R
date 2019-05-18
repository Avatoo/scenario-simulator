#-----------------------------
# Web app for location of assets
#-----------------------------

library(shiny)
library(leaflet)

gis = read_xlsx("../data/Infrahack - Asset Data - DN Clean.xlsx", 
                sheet = 'Infrahack - Asset Location')

# Define UI for application that draws a leaflet map
ui <- fluidPage(
    titlePanel("Asset Locations"),
    leafletOutput("mymap"),
    p(),
    selectizeInput("asset", 
                   "Asset:",
                   choices = c(Choose="", gis$Asset_Name),
                   selected =  "London",
                   multiple = TRUE)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    filteredData <- reactive({
        # Manually create a All cenario if nothing selected
        if (length(input$asset) == 0) gis
        else  filter(gis, Asset_Name %in% input$asset) 
        
        ## Nothing rendered if nothing selected
        #req(input$asset)
        #filter(gis, asset %in% input$asset)
    })
    
    
    output$mymap <- renderLeaflet({
        filteredData() %>% 
            leaflet() %>% 
            addTiles() %>%
            addCircles(lng = ~Longitude, lat = ~Latitude
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

