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
                        tabPanel('Background',
                                 mainPanel(
                                   h2('Population vs. Sample'),
                                   p('Imagine you want to know the average height of people who live in the United States. People who live in the United States are your', span(em('population.'))),
                                   p('To measure this precisely, you would need to find every person in the United States and measure their height. We would call this the', span(em('population mean.'))),
                                   p('However, this is impractical and unnecessary. Thanks to basic principles of statistics, we can accurately infer the population mean by taking a', span(em('random sample,')), 'where every potential person in the population has an equal likelihood of being included in the sample.'),
                                   p('For estimating the average height of people in the US, then, we can pick a random subset of the population and measure their heights. Assuming our sample is large enough (and truly random), this', span(em('sample mean')), 'will give us a close approximation of the true population mean.'),
                                   p('In other words, all else being equal, a larger sample size will yield a more accurate estimate of the population. This is illustrated in the visualization below. For a Normal Distribution, like the example below, it turns out the sample size can be fairly small to get a good estimate of the underlying population mean (and standard deviation)!'),
                                   plotOutput('sample_distribution_example',
                                              width = 750,
                                              height = 500),
                                   
                                   h2('What is an empirical sampling distribution?'),
                                   p("So far we've seen a population distribution and two sample distributions. We found that when we sampled 25 random Americans from the population, we were able to get a pretty good approximation of the mean and standard deviation of the underlying population."),
                                   p('Now imagine that we want to replicate our experiment above by drawing another sample of 25 random Americans. Or, we could even replicate our experiment 1000 times.'),
                                   p('The', span(em('empirical sampling distribution')), 'is the distribution of means from each of your 1000 experiments.'),
                                   plotOutput('sample_mean_distribution_example',
                                              width = 750,
                                              height = 500),
                                   
                                   
                                   
                                   h2('What is the central limit theorem?'),
                                   p('The sampling distribution of the sample mean will be normal even if the population is not normally distributed, as long as the sample size is large enough.'),
                                   p(''),
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
                                     h2('Population distribution', align = 'left'),
                                     p('This is the population distribution that we are sampling from!'),
                                     plotOutput('population_distribution'),
                                     h2('Empirical Sampling Distribution', align = 'left'),
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
  
  default_distribution <- 'Normal'
  default_population_mean <- 65
  getDefaultDistribution <- reactive({
    make_population_distribution(default_distribution,
                                 mean = default_population_mean,
                                 sample_size = 10000)
  })
  
  # getDefaultSampleMeanDistribution <- reactive({
  #   
  # })
  
  output$sample_distribution_example <- renderPlot({
    ## Population
    df_population <- getDefaultDistribution()
    df_population$label <- 'Population Distribution'
    
    # Summary stats
    df_population_n <- length(df_population$sample)
    df_population_n_label <- paste('Obs =', df_population_n)
    
    df_population_mean <- round(mean(df_population$sample), 2)
    df_population_mean_label <- paste('Mean =', df_population_mean)
    
    df_population_sd <- round(sd(df_population$sample), 2)
    df_population_sd_label <- paste('SD =', df_population_sd)
    
    ## Small sample
    df_sample <- sample(df_population$sample, 25)
    df_sample <- as.data.frame(df_sample)
    df_sample$label <- 'Sample Distribution 1'
    names(df_sample)[1] <- 'sample'
    
    # Summary stats
    df_sample_n <- length(df_sample$sample)
    df_sample_n_label <- paste('Obs =', df_sample_n)
    
    df_sample_mean <- round(mean(df_sample$sample), 2)
    df_sample_mean_label <- paste('Mean =', df_sample_mean)
    
    df_sample_sd <- round(sd(df_sample$sample), 2)
    df_sample_sd_label <- paste('SD =', df_sample_sd)
    
    ## Large sample
    df_sample_large <- sample(df_population$sample, 500)
    df_sample_large <- as.data.frame(df_sample_large)
    df_sample_large$label <- 'Sample Distribution 2'
    names(df_sample_large)[1] <- 'sample'
    
    # Summary stats
    df_sample_large_n <- length(df_sample_large$sample)
    df_sample_large_n_label <- paste('Obs =', df_sample_large_n)
    
    df_sample_large_mean <- round(mean(df_sample_large$sample), 2)
    df_sample_large_mean_label <- paste('Mean =', df_sample_large_mean)
    
    df_sample_large_sd <- round(sd(df_sample_large$sample), 2)
    df_sample_large_sd_label <- paste('SD =', df_sample_large_sd)
    
    # Combine datasets
    df <- rbind(df_population, df_sample, df_sample_large)
    df_text <- data.frame(
      label = as.factor(c('Population Distribution', 'Sample Distribution 1', 'Sample Distribution 2')),
      obs_label = as.factor(c(df_population_n_label, df_sample_n_label, df_sample_large_n_label)),
      mean_label = as.factor(c(df_population_mean_label, df_sample_mean_label, df_sample_large_mean_label)),
      sd_label = as.factor(c(df_population_sd_label, df_sample_sd_label, df_sample_large_sd_label))
    )
    
    # Make the plot
    ggplot(df, aes(sample)) +
      geom_histogram(aes(y = stat(density)), binwidth = 0.3, fill = '#b3ecff') +
      stat_function(fun=dnorm, args = list(mean = default_population_mean), color='darkgrey', size=1.2) +
      facet_wrap(~label, ncol = 3) +
      labs(
        x = 'Height (inches)',
        y = 'Density'
      ) +
      our_theme +
      geom_text(
        size    = 5,
        data    = df_text,
        mapping = aes(x = Inf, y = Inf, label = obs_label),
        hjust   = 1.05,
        vjust   = 1.5
      ) +
      geom_text(
        size    = 5,
        data    = df_text,
        mapping = aes(x = Inf, y = Inf, label = mean_label),
        hjust   = 1.05,
        vjust   = 3.0
      ) +
      geom_text(
        size    = 5,
        data    = df_text,
        mapping = aes(x = Inf, y = Inf, label = sd_label),
        hjust   = 1.05,
        vjust   = 4.5
      )
  })
  
  output$sample_mean_distribution_example <- renderPlot({
    df_population <- getDefaultDistribution()
    df_population$label <- 'Population Distribution'
    df_sample <- make_default_sampling_distribution(df_population, 25, 1000)
    df_sample$label <- 'Empirical Sampling Distribution'
    names(df_sample)[1] <- 'sample'
    
    df <- rbind(df_population, df_sample)
    
    # # Get population mean
    # population_mean <- round(mean(df$sample), 2)
    # mean_output <- paste('mean=', population_mean, sep='')
    # 
    # # Run Shapiro-Wilk test for normality
    # # Format for display
    # shapiro_output <- shapiro.test(df$sample)$p.value
    # normal_display <- shapiro_display_format(shapiro_output)
    
    # Plot population distribution
    ggplot(df, aes(sample)) +
      geom_histogram(aes(y = stat(density)), binwidth = 0.3, fill = '#b3ecff') +
      stat_function(fun=dnorm, args = list(mean = default_population_mean), color='darkgrey', size=1.2) +
      facet_grid(cols = vars(label)) +
      labs(
        # subtitle = paste(normal_display, '\n', mean_output, sep=''),
        x = 'Height (inches)',
        y = 'Density'
      ) +
      our_theme
  })
  
  output$example_sample_distribution <- renderPlot({
    df <- getDefaultSampleDistribution()
    
    # Get population mean
    population_mean <- round(mean(df$sample), 2)
    mean_output <- paste('mean=', population_mean, sep='')
    
    # Run Shapiro-Wilk test for normality
    # Format for display
    shapiro_output <- shapiro.test(df$sample)$p.value
    normal_display <- shapiro_display_format(shapiro_output)
    
    # Plot population distribution
    ggplot(df, aes(sample_means)) +
      geom_histogram(aes(y = stat(count / sum(count)))) +
      labs(
        subtitle = paste(normal_display, '\n', mean_output, sep=''),
        x = 'Value',
        y = 'Proportion of observations'
      ) +
      our_theme
  })
  
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