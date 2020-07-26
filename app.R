# Load packages ----
library(shiny)
library(tidyverse)

make_distribution <- function(distribution,
                              sample_size,
                              n_iterations)
{
  # Fix this
  if (distribution == 'exponential') { 
    x <- matrix(
      rexp(sample_size * n_iterations, rate=2),
      ncol = n_iterations
    )
  } else {
    x <- matrix(
      rnorm(sample_size),
      ncol = n_iterations
    )
  }
  
  x_means <- colMeans(x)
  x_df <- as.data.frame(x_means)
  names(x_df)[1] <- 'sample_means'
  
  return(x_df)
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
                                                 c('Exponential', 'Normal'), selected = NULL, multiple = FALSE,
                                                 selectize = TRUE, width = NULL, size = NULL),
                                     
                                     numericInput('sample_size',
                                                  'Choose sample size',
                                                  '1'),
                                     
                                     numericInput('n_iterations',
                                                  'Choose number of samples to draw',
                                                  '1000'),
                                   ),
                                   
                                   mainPanel(plotOutput('sample_distribution'))
                                 )
                        )
             ),
             navbarMenu('p-values'),
             
             navbarMenu('Type I & Type II Error')
  )
)

# Server logic
server <- function(input, output) {
  
  getSamples <- reactive({
    make_distribution(input$distribution,
                      input$sample_size,
                      input$n_iterations)
  })
  
  # iosInputIncrease <- reactive({
  #   mau_conversion_increase(mau_data_ios,
  #                           input$mom_one_increase,
  #                           input$mom_six_increase,
  #                           input$mom_twelve_increase)
  # })
  
  output$sample_distribution <- renderPlot({
    df <- getSamples()
    ggplot(df, aes(sample_means)) + geom_histogram()
  })
  
  # output$ios_twelve_plot <- renderPlot({
  #   df <- iosInputIncrease()
  #   df <- df %>% filter(date >= input$dates[1] & date <= input$dates[2])
  #   ggplot(df, aes(date, twelve_month_conversion)) +
  #     geom_line() +
  #     geom_point() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #     theme_bw() +
  #     labs(title = 'iOS - Twelve month conversion rate',
  #          x = '',
  #          y = 'MAU conversion rate')
  # })
  
}

# Run the app
shinyApp(ui, server)