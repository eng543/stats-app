# Load packages ----
setwd("~/Documents/stats-app")

library(shiny)
library(tidyverse)
source('ggplot_theme.R')
source('helper_functions.R')

# User interface ----
ui <- fluidPage(
  titlePanel(
    h1('Stats Reasoning',
    h4('Zac Caddick & Erin Gustafson'))
    ),
  
  navbarPage('Stats Reasoning',
             navbarMenu('Central Limit Theorem',
                        tabPanel('Learn some stuff!',
                                 mainPanel(
                                   h2("We're gonna learn some stuff"),
                                   p("This is where we're gonna learn some stuff"),
                                   p('All sorts of stuff!'),
                                   p("It's gonna be great"),
                                   p('Why do we even care about this?')
                                 )
                                 ),
                        tabPanel('Practice with simulations',
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput('distribution',
                                                 'Choose distribution',
                                                 c('Normal', 'Exponential', 'Poisson', 'Binomial', 'Chi-squared', 'Lognormal'),
                                                 multiple = FALSE,
                                                 selectize = TRUE),

                                     numericInput('sample_size',
                                                  'Choose sample size for each experiment',
                                                  '1',
                                                  min = 1),

                                     numericInput('n_iterations',
                                                  'Choose number of experiments to run',
                                                  '1000'),

                                     actionButton('button', 'Run experiments'),
                                   ),
                                   
                                   mainPanel(
                                     h2('Hypothetical population distribution', align = 'left'),
                                     p('This is the hypothetical population distribution that we are sampling from!'),
                                     plotOutput('population_distribution'),
                                     h2('Distribution of experiment sample means', align = 'left'),
                                     p("When you run a series of experiments, you'll see the distribution of sample means below..."),
                                     plotOutput('sample_mean_distribution')
                                     )
                                 )
                        )
             ),
             # Assumptions of test & relation to CLT
             # Bootstrapping
             navbarMenu('t-test'),
             
             # Repeat experiment 100 times, difference observed 5/100 times
             # Instability of p-values for under-powered experiments
             navbarMenu('p-values'),
             
             navbarMenu('Type I & Type II Error')
  )
)

# Server logic
server <- function(input, output) {
  
  ## Plot hypothetical population distribution
  # Get distribution
  getPopulationDistribution <- reactive({
    make_population_distribution(input$distribution)
  })

  # Plot it
  output$population_distribution <- renderPlot({
    df <- getPopulationDistribution()
    
    # Get population mean
    population_mean <- round(mean(df$sample), 2)
    mean_output <- paste('mean=', population_mean, sep='')
    
    # Run Shapiro-Wilk test for normality
    # Format for display
    shapiro_output <- shapiro.test(df$sample)$p.value
    normal_display <- shapiro_display_format(shapiro_output)
    
    # Plot population distribution
    ggplot(df, aes(sample)) +
      geom_histogram(aes(y = stat(count / sum(count)))) +
      labs(
        subtitle = paste(normal_display, '\n', mean_output, sep=''),
        x = 'Value',
        y = 'Proportion of observations'
        ) +
      our_theme
  })

  ## Plot sample mean distribution from current simulation
  # Wait for button press
  observeEvent(input$button, {
    # Get sample mean distribution
    output$sample_mean_distribution <- renderPlot({
      df <- make_distribution(input$distribution, isolate(input$sample_size), isolate(input$n_iterations))
      
      # Get mean of sample means
      mean_sample_mean <- round(mean(df$sample_means), 2)
      mean_output <- paste('mean=', mean_sample_mean, sep='')
      
      # Run Shapiro-Wilk test for normality
      # Format for display
      shapiro_output <- shapiro.test(df$sample_means)$p.value
      normal_display <- shapiro_display_format(shapiro_output)
      
      # Plot sample mean distribution
      ggplot(df, aes(sample_means)) +
        geom_histogram(aes(y = stat(count / sum(count)))) +
        labs(
          subtitle = paste(normal_display, '\n', mean_output, sep=''),
          x = 'Sample means',
          y = 'Proportion of samples'
        ) +
        ylim(0, 0.115) +
        our_theme
    })
  })
}

# Run the app
shinyApp(ui, server)