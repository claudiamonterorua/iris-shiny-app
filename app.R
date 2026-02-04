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
        "hist_var",
        "Dimension:",
        choices = names(iris)[1:4]
      ),
      
      hr(),
      
      h4("Scatter plot"),
      
      radioButtons(
        "comparison_type",
        "Compared dimensions:",
        choices = c("Lengths", "Widths"),
        selected = "Lengths"
      )
    ),
    
    mainPanel(
      
      h4(textOutput("hist_title")),
      plotOutput("histPlot"),
      
      br(),
      
      h4(textOutput("scatter_title")),
      plotOutput("scatterPlot"),
      
      br(),
      
      h4("Data"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  data_filtered <- reactive({
    iris[iris$Species == input$species, ]
  })
  
  output$hist_title <- renderText({
    paste("Histogram for", input$species, "of", input$hist_var)
  })
  
  output$histPlot <- renderPlot({
    hist(
      data_filtered()[[input$hist_var]],
      col = "lightblue",
      xlab = input$hist_var,
      main = NULL
    )
  })
  
  output$scatter_title <- renderText({
    if (input$comparison_type == "Lengths") {
      paste("Scatter plot for ", input$species, "of Sepal.Length vs Petal.Length")
    } else {
      paste("Scatter plot for ", input$species, "of Sepal.Width vs Petal.Width")
    }
  })
  
  output$scatterPlot <- renderPlot({
    
    if (input$comparison_type == "Lengths") {
      x <- data_filtered()$Sepal.Length
      y <- data_filtered()$Petal.Length
      xlab <- "Sepal.Length"
      ylab <- "Petal.Length"
    } else {
      x <- data_filtered()$Sepal.Width
      y <- data_filtered()$Petal.Width
      xlab <- "Sepal.Width"
      ylab <- "Petal.Width"
    }
    
    plot(
      x, y,
      col = "darkgreen",
      pch = 19,
      xlab = xlab,
      ylab = ylab,
      main = NULL
    )
  })
  
  output$table <- renderTable({
    data_filtered()
  })
}

shinyApp(ui = ui, server = server)
