#' sim_mean_sd
#'
#' @param n number of subjects
#' @param mu true sample mean
#' @param sigma true sample sd
#'
#' @returns tibble with sample mean and sample sd

sim_mean_sd = function(n, mu = 2, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}