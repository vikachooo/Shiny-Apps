

library(shiny)
library(shinyWidgets)
library(leaflet)
library(viridis)

ui <- fluidPage(
  titlePanel("Map of Russia"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable to Visualise:",
                  choices = c("Online Transactions" = "transactions_av", 
                              "Economic Activity" = "activity_av", 
                              "Average Wage" = "wage", 
                              "Unemployment Rate" = "unemployment"),
                  selected = "transactions_av"),
      pickerInput("palette", "Select Color Palette:",
                  choices = c("Cool Blues" = "Viridis", "Green to Blue" = "YlGnBu", "Sunset" = "RdYlBu", "Lavender Tones" = "BuPu"),
                  selected = "Cool Blues",
                  options = list(`style` = "btn-default"))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)



##### server ####
server <- function(input, output, session) {
  
  reactive_map <- reactive({
    # Define color palette
    pal <- switch(input$palette,
                  "Viridis" = colorNumeric(viridis(10), merged_data[[input$variable]]),
                  "YlGnBu" = colorNumeric('YlGnBu', merged_data[[input$variable]]),
                  "RdYlBu" = colorNumeric('RdYlBu', merged_data[[input$variable]]),
                  "BuPu" = colorNumeric('BuPu', merged_data[[input$variable]]))
    
    # colors and labels
    merged_data <- merged_data %>%
      mutate(reg_color = pal(!!sym(input$variable)),
             reg_label = paste0(name, ': ', formatC(!!sym(input$variable)), digits = 2))
    
    # create leaflet map
    rus_polygons <- sp::SpatialPolygonsDataFrame(rus_corrected, merged_data)
    
    rus_polygons %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, fillColor = ~reg_color, label = ~reg_label, group = "Regions") %>%
      addLegend(pal = pal, values = merged_data[[input$variable]], title = input$variable) %>%
      setView(lng = 100, lat = 66, zoom = 2)
  })
  
  # render the map
  output$map <- renderLeaflet({
    reactive_map()
  })
}


shinyApp(ui = ui, server = server)

