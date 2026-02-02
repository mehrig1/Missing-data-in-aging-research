

library(tidyverse)
library(missForest)

n_runs <- 100
n      <- 100
p_mcar <- 0.30

simulate_once <- function(seed) {
  set.seed(seed)  # per-run seeding
  
  age   <- rnorm(n, mean = 65, sd = 10)
  sex   <- rbinom(n, size = 1, prob = 0.50)
  BMI   <- rnorm(n, mean = 27, sd = 6)
  error <- rnorm(n, mean = 0, sd = 0.25)
  gs <- 2.75 - 0.015 * age - 0.3 * sex - 0.02 * BMI + error
  gs <- ifelse(gs < 0, 0, gs)
  
  data = cbind.data.frame(age, sex, BMI, gs)
  data = data %>% mutate(
    prob = case_when(
      sex == 1 ~ 0.6,
      sex == 0 ~ 0.05
    )
  )
  miss_ind = rbinom(n = 100, size = 1, prob = data$prob)
  BMI_miss <- ifelse(miss_ind == 1, NA_real_, BMI)
  
  data <- data.frame(age,
                     sex = factor(sex),
                     BMI_miss,
                     gs)
  
  imp <- missForest(data, maxiter = 10, ntree = 100, verbose = FALSE)
  BMI_imp <- imp$ximp$BMI_miss
  
  imputations <- data.frame(miss_ind, BMI, BMI_imp) |>
    dplyr::filter(miss_ind == 1)
  
  abs_diff    <- abs(imputations$BMI - imputations$BMI_imp)
  sum_squares <- (imputations$BMI - imputations$BMI_imp)^2
  
  c(
    mean_BMI        = mean(BMI),
    sd_BMI          = sd(BMI),
    mean_BMI_miss   = mean(BMI_miss, na.rm = TRUE),
    sd_BMI_miss     = sd(BMI_miss, na.rm = TRUE),
    mean_BMI_imp    = mean(BMI_imp),
    sd_BMI_imp      = sd(BMI_imp),
    mean_abs_diff   = mean(abs_diff),
    MSE             = mean(sum_squares),
    miss_perc = length(which(miss_ind == 1))
  )
}

# One seed per run
seeds <- 1234 + seq_len(n_runs)

# Run and collect
mat <- sapply(seeds, simulate_once)
res <- as.data.frame(t(mat))

# mean of results
round(mean(res$mean_BMI), 3)
round(mean(res$mean_BMI_miss), 3)
round(mean(res$mean_BMI_imp), 3)
round(mean(res$sd_BMI), 3)
round(mean(res$sd_BMI_miss), 3)
round(mean(res$sd_BMI_imp), 3)

round(mean(res$mean_abs_diff), 3)
round(mean(res$MSE), 3)
round(mean(res$miss_perc), 3)