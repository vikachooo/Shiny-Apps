

library(shiny)

setwd("/Users/viktoriazajceva/Desktop/R/19 Shiny/19hw(Titanic)")
tit <- read.csv("Titanic.csv")

ui <- fluidPage(
  titlePanel("Titanic passengers"),
  fluidRow(
    column(4,
           selectInput("nominal", "Barplot:",
                       choices = c("Survived", "PClass", "Sex"))),
    column(4,
           selectInput("numeric", "Histogram:",
                       choices = c("Age", "Fare")))
    ),
  fluidRow(
    column(4,
           selectInput("col1", "Barplot Colour:",
                       choices = c("navy", "lightblue","palegreen"))),
    column(4,
            selectInput("col2", "Histogram Colour:",
                        choices = c("navy", "lightblue","palegreen")))),
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))
    ))
  )

server <- function(input, output) {
  output$distPlot1 <- renderPlot( {
    data <- tit
    hcolor <- "navy"
    if (input$nominal == "Survived") {
      barplot(table(data$Survived), col = input$col1,
              xlab = "Survived", ylab = "Frequency")
    }
    if (input$nominal == "PClass") {
      barplot(table(data$Pclass), col = input$col1,
              xlab = "Passenger's class", ylab = "Frequency")
    }
    if (input$nominal == "Sex") {
      barplot(table(data$Sex), col = input$col1,
              xlab = "Sex", ylab = "Frequency")
    }
  })
  output$distPlot2 <- renderPlot( {
    data <- tit
    hcolor <- "navy"
    if (input$numeric == "Age") {
      hist(data$Age, col = input$col2,
           xlab = "Age", ylab = "Frequency",
           main = "") #убрать дефолтное
    }
    if (input$numeric == "Fare") {
      hist(data$Fare, col = input$col2,
           xlab = "Fare", ylab = "Frequency",
           main = "")
    }
  })
}


library(rsconnect)

# Replace these paths with the actual paths to your Shiny app directory and main document
app_directory <- "/Users/viktoriazajceva/Desktop/R/19 Shiny/19hw(Titanic)"
main_document <- "19hw.R"

# Explicitly specify the main document using appPrimaryDoc argument
rsconnect::deployApp(appDir = app_directory, appPrimaryDoc = main_document)


shinyApp(ui = ui, server = server)


