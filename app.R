library(shiny)

ui <- fluidPage(
  
  titlePanel("Iris Shiny App"),
  
  p("This Shiny application demonstrates reactive programming through univariate and multivariate visualizations of iris flower measurements."),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Selection"),
      
      selectInput(
        "species",
        "Species:",
        choices = unique(iris$Species)
      ),
      
      hr(),
      
      h4("Histogram"),
      
      selectInput(
        "dimension",
        "Dimension:",
        choices = names(iris)[1:4]
      ),
      
      hr(),
      
      h4("Scatter plot"),
      
      radioButtons(
        "compared_dimensions",
        "Compared dimensions:",
        choices = c("Lengths", "Widths"),
        selected = "Lengths"
      )
    ),
    
    mainPanel(
      
      h4(textOutput("hist_title")),
      plotOutput("hist_plot"),
      
      br(),
      
      h4(textOutput("scatter_title")),
      plotOutput("scatter_plot"),
      
      br(),
      
      h4("Data"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  iris_data <- reactive({
    iris[iris$Species == input$species, ]
  })
  
  output$hist_title <- renderText({
    paste("Histogram for", input$species, "of", input$dimension)
  })
  
  output$hist_plot <- renderPlot({
    hist(
      iris_data()[[input$dimension]],
      col = "lightblue",
      xlab = input$dimension,
      main = NULL
    )
  })
  
  output$scatter_title <- renderText({
    if (input$compared_dimensions == "Lengths") {
      paste("Scatter plot for ", input$species, "of Sepal.Length vs Petal.Length")
    } else {
      paste("Scatter plot for ", input$species, "of Sepal.Width vs Petal.Width")
    }
  })
  
  output$scatter_plot <- renderPlot({
    
    if (input$compared_dimensions == "Lengths") {
      x <- iris_data()$Sepal.Length
      y <- iris_data()$Petal.Length
      xlab <- "Sepal.Length"
      ylab <- "Petal.Length"
    } else {
      x <- iris_data()$Sepal.Width
      y <- iris_data()$Petal.Width
      xlab <- "Sepal.Width"
      ylab <- "Petal.Width"
    }
    
    plot(
      x, y,
      col = "steelblue",
      pch = 19,
      xlab = xlab,
      ylab = ylab,
      main = NULL
    )
  })
  
  output$table <- renderTable({
    iris_data()
  })
}

shinyApp(ui = ui, server = server)
