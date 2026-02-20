library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#E83E8C",
    bg = "#FFFFFF",
    fg = "#2C2C2C"
  ),
  
  div(style = "padding:25px;",
      
      h2("Iris R Shiny App",
         style = "font-weight:600;"),
      
      p("This R Shiny application demonstrates reactive programming through univariate and multivariate visualizations in an interactive dashboard for exploring iris flower measurements.",
        style = "color:#666; margin-bottom:30px;"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          h4("Filters"),
          
          checkboxGroupInput(
            "species",
            "Species:",
            choices = unique(iris$Species),
            selected = unique(iris$Species)
          ),
          
          selectInput(
            "variable",
            "Variable:",
            choices = names(iris)[1:4],
            selected = "Sepal.Length"
          )
        ),
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("Scatter",
                     plotOutput("scatter_plot", height = "450px")
            ),
            
            tabPanel("Histogram",
                     plotOutput("hist_plot", height = "450px")
            ),
            
            tabPanel("Boxplot",
                     plotOutput("box_plot", height = "450px")
            ),
            
            tabPanel("Data",
                     DTOutput("raw_data")
            )
          )
        )
      )
  )
)

server <- function(input, output) {
  
  iris_filtered <- reactive({
    req(input$species)
    iris %>% filter(Species %in% input$species)
  })
  
  # ===== SCATTER =====
  output$scatter_plot <- renderPlot({
    
    df <- iris_filtered()
    
    if (grepl("Length", input$variable)) {
      x_var <- "Sepal.Length"
      y_var <- "Petal.Length"
    } else {
      x_var <- "Sepal.Width"
      y_var <- "Petal.Width"
    }
    
    ggplot(df,
           aes(x = .data[[x_var]],
               y = .data[[y_var]],
               color = Species)) +
      geom_point(size = 3, alpha = 0.85) +
      theme_minimal(base_size = 15) +
      scale_color_manual(values = c(
        "setosa" = "#F48FB1",
        "versicolor" = "#EC407A",
        "virginica" = "#AD1457"
      )) +
      labs(
        title = paste("Scatter of", x_var, "vs", y_var),
        x = x_var,
        y = y_var
      )
  })
  
  # ===== HISTOGRAM =====
  output$hist_plot <- renderPlot({
    
    ggplot(iris_filtered(),
           aes(x = .data[[input$variable]],
               fill = Species)) +
      geom_histogram(
        alpha = 0.6,
        position = "identity",
        bins = 20,
        color = "white"
      ) +
      theme_minimal(base_size = 15) +
      scale_fill_manual(values = c(
        "setosa" = "#F8BBD0",
        "versicolor" = "#F06292",
        "virginica" = "#C2185B"
      )) +
      labs(
        title = paste("Histogram of", input$variable),
        x = input$variable,
        y = "Count"
      )
  })
  
  # ===== BOXPLOT =====
  output$box_plot <- renderPlot({
    
    ggplot(iris_filtered(),
           aes(x = Species,
               y = .data[[input$variable]],
               fill = Species)) +
      geom_boxplot(alpha = 0.85) +
      theme_minimal(base_size = 15) +
      scale_fill_manual(values = c(
        "setosa" = "#F8BBD0",
        "versicolor" = "#F06292",
        "virginica" = "#C2185B"
      )) +
      labs(
        title = paste("Boxplot of", input$variable),
        x = "",
        y = input$variable
      )
  })
  
  # ===== DATA =====
  output$raw_data <- renderDT({
    datatable(
      iris_filtered(),
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)