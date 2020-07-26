# Generate hypothetical population distribution
make_population_distribution <- function(distribution) {
  if (distribution == 'Exponential') { 
    x_sample <- rexp(1000, rate=2)
    
  } else if (distribution == 'Normal') {
    x_sample <- rnorm(1000)
    
  } else if (distribution == 'Binomial') {
    x_sample <- rbinom(1000, 1, 0.5)
    
  } else if (distribution == 'Chi-squared') {
    x_sample <- rchisq(1000, df=1, ncp = 0)
    
  } else if (distribution == 'Lognormal') {
    x_sample <- rlnorm(1000, meanlog = -4, sdlog = 3)
    
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
    
  } else if (distribution == 'Binomial') {
    x <- matrix(
      rbinom(sample_size * n_iterations, 1, 0.5),
      ncol = n_iterations
    )
    
  } else if (distribution == 'Chi-squared') {
    x <- matrix(
      rchisq(sample_size* n_iterations, df=1, ncp = 0),
      ncol = n_iterations
    )
    
  } else if (distribution == 'Lognormal') {
    x <- matrix(
      rlnorm(sample_size * n_iterations, meanlog = -4, sdlog = 3),
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

shapiro_display_format <- function(shapiro_output) {
  if (shapiro_output < 0.05) {
    normal_display <- 'Not normal according to Shapiro-Wilk test'
    p_value_display <- '(p<0.05)'
  } else {
    normal_display <- 'Normal according to Shapiro-Wilk test'
    p_value_display <- paste('(p=', round(shapiro_output, 2), ')', sep='')
  }
  normal_display <- paste(normal_display, p_value_display)
  
  return(normal_display)
}