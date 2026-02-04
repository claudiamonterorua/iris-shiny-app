library(shiny)

ui <- fluidPage(
  titlePanel("Iris Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "species",
        "Select species:",
        choices = unique(iris$Species)
      ),
      
      selectInput(
        "variable",
        "Select variable:",
        choices = names(iris)[1:4]
      )
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  data_filtered <- reactive({
    iris[iris$Species == input$species, ]
  })
  
  output$plot <- renderPlot({
    hist(
      data_filtered()[[input$variable]],
      col = "lightblue",
      main = input$variable
    )
  })
  
  output$table <- renderTable({
    data_filtered()
  })
}

shinyApp(ui = ui, server = server)
