library(DT)

library(shiny)

setwd("/Users/viktoriazajceva/Desktop/R/19 Shiny")

stars <- read.csv("Stars.csv")

ui <- fluidPage(
  titlePanel("Stars"),
  fluidRow(
    column(10,
           selectInput("type", "Type: ",
                       choices = c("Все" = "All",
                                   "Коричневый карлик" = 0, 
                                   "Красный карлик" = 1,
                                   "Белый карлик" = 2, 
                                   "Звезда главной последовательности" = 3, 
                                   "Супергигант" = 4, 
                                   "Гипергигант" = 5) # unique() не подходит, так как нужно прописать названия
    ))
  ),
  mainPanel(
  DT::dataTableOutput("table"),
  plotOutput("distPlot")
))

server <- function(input,output) {
  
  output$distPlot <- renderPlot(
    {
      data <- stars
      hcolor <- 'grey' # цвет по умолчанию если стоит опция All, иначе..
      if (input$type != "All"){
        data <- data[data$Star.type == input$type,]
        hcolor <- 'antiquewhite'
      }
      
      x <- data$Temperature..K. #или сразу data$Temperature..K. в boxplot
      boxplot(x, col = hcolor)
    }
  )

  output$table <- DT::renderDataTable(DT::datatable
                                      (
                                        {
                                          data <- stars
                                          if (input$type != "All"){
                                            data <- data[data$Star.type == input$type,]
                                          }
                                          else {
                                            data
                                          }
                                        }
                                      ))

} # конец function


shinyApp(ui = ui, server = server)
