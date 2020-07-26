# Load packages ----
library(shiny)
library(tidyverse)

make_sample_distribution <- function(distribution) {
  if (distribution == 'Exponential') { 
    x_sample <- rexp(1000, rate=2)
    
  } else if (distribution == 'Normal') {
    x_sample <- rnorm(1000)
    
  } else {
    x_sample <- rpois(1000, lambda = 1)
    
  }
  
  x_sample_df <- as.data.frame(x_sample)
  names(x_sample_df)[1] <- 'sample'
  
  return(x_sample_df)
}

make_distribution <- function(distribution,
                              sample_size,
                              n_iterations)
{
  if (distribution == 'Exponential') { 
    x <- matrix(
      rexp(sample_size * n_iterations, rate=2),
      ncol = n_iterations
    )

  } else if (distribution == 'Normal') {
    x <- matrix(
      rnorm(sample_size * n_iterations),
      ncol = n_iterations
    )
    
  } else {
    x <- matrix(
      rpois(sample_size * n_iterations, lambda = 1),
      ncol = n_iterations
    )
  }
  
  x_means <- colMeans(x)
  x_means_df <- as.data.frame(x_means)
  names(x_means_df)[1] <- 'sample_means'
  
  return(x_means_df)
}

# User interface ----
ui <- fluidPage(
  titlePanel("Stats Reasoning"),
  
  navbarPage('Stats Reasoning',
             navbarMenu('Central Limit Theorem',
                        tabPanel('Simulation Demonstration',
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput('distribution',
                                                 'Choose distribution',
                                                 c('Normal', 'Exponential', 'Poisson'), selected = NULL, multiple = FALSE,
                                                 selectize = TRUE, width = NULL, size = NULL),
                                     
                                     numericInput('sample_size',
                                                  'Choose sample size for each experiment',
                                                  '1',
                                                  min = 1),
                                     
                                     numericInput('n_iterations',
                                                  'Choose number of experiments to run',
                                                  '1000'),
                                     
                                     actionButton('button', 'Run experiments')
                                     
                                   ),
                                   
                                   mainPanel(plotOutput('sample_distribution'),
                                             plotOutput('sample_mean_distribution')
                                             )
                                 )
                        )
             ),
             navbarMenu('p-values'),
             
             navbarMenu('Type I & Type II Error')
  )
)

# Server logic
server <- function(input, output) {
  getSampleDistribution <- reactive({
    make_sample_distribution(input$distribution)
  })
  
  output$sample_distribution <- renderPlot({
    df <- getSampleDistribution()
    ggplot(df, aes(sample)) + geom_histogram()
  })

  observeEvent(input$button, {
    output$sample_mean_distribution <- renderPlot({
      df <- make_distribution(input$distribution, isolate(input$sample_size), isolate(input$n_iterations))
      ggplot(df, aes(sample_means)) + geom_histogram()
    })
  })
  
}

# Run the app
shinyApp(ui, server)