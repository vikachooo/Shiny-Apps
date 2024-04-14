library(shiny)

all_data_eng <- all_data_eng[,-1]

indicators <- colnames(all_data_eng)[5:16]
indicators <- indicators[-c(7,10)]  # For options

regions <- c("Russia", unique(all_data_eng$Region))  # For options

# Titles for indicators
indicatorTitles <- c(transactions = "Online Transactions",
                     activity = "Economic Activity",
                     tourism = "Tourism (difference from the last year)",
                     average.wage = "Average Wage",
                     unemployment.rate = "Unemployment Rate",
                     internet.users = "Internet Users",
                     online.loan.applications.share = "Online loan applications",
                     offline.loan.applications.share = "Offline loan applications",
                     new.build.share = "Mortgage for new build",
                     already.owned.share = "Mortgage for already owned")


# UI
ui <- fluidPage(
  titlePanel("Data Visualization App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region:", choices = regions, selected = "Russia"),
      selectInput("indicator", "Indicator:", choices = names(indicatorTitles), selected = names(indicatorTitles)[1]),
      selectInput("type", "Type:", choices = c("Value distribution", "Time trend"), selected = "Time trend"),
      textInput("colorInput", "Enter Color Code:", value = "#3498db")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# server 
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    data <- all_data_eng
    
    if (input$region != "Russia") {
      data <- filter(data, Region == input$region)
    } else {
      # Group by 'Month' and calculate the mean for numeric columns
      data <- data %>%
        group_by(Month) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE))
    }
    
    if (nrow(data) == 0) {
      plot.new()
      title(main = "No data available", col.main = "red")
      return()
    }
    
    plotTitle <- indicatorTitles[input$indicator]
    
    if (input$type == "Value distribution") {
      boxplot(data[[input$indicator]], main = paste("Distribution of", plotTitle, "in", input$region), col = input$colorInput, ylab = plotTitle)
    } else {
      months <- 1:nrow(data)
      plot(months, data[[input$indicator]], type = "l", main = paste(plotTitle, "over time in", input$region), col = input$colorInput, xlab = "Month", ylab = plotTitle, xaxt = 'n')
      points(months, data[[input$indicator]], pch = 19, col = input$colorInput)
      axis(1, at = months, labels = month.abb, las = 2)  # Use abbreviated month names, with labels perpendicular to the axis
    }
  })
}


shinyApp(ui = ui, server = server)
