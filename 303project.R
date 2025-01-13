# Load required libraries
library(ggplot2)
library(MASS)
library(fitdistrplus)
library(dplyr)

install.packages('fitdistrplus')
# Assume data_set_12_new, data_set_22_new, and data_set_32_new are available as lists or matrices
data_set_32_new <- data.set.32
data_set_12_new<- data.set.12
data_set_22_new<- data.set.22
data_set_32<- data.set.32
data_set_12<- data.set.12
data_set_22<- data.set.22
# Goodness-of-fit test for Gamma distribution
gamma_shape <- 3
gamma_scale <- 1
gamma_loc <- 0

ks_test_32_new <- ks.test(data_set_32_new[[1]], "pgamma", shape = gamma_shape, rate = 1 / gamma_scale)

# Fit normal distributions for data set 12 and 22
norm_params_12_new <- fitdist(data_set_12_new[[1]], "norm")
norm_params_22_new <- fitdist(data_set_22_new[[1]], "norm")

# Goodness-of-fit for normal distributions
ks_test_12_new <- ks.test(data_set_12_new[[1]], "pnorm", mean = norm_params_12_new$estimate[1], sd = norm_params_12_new$estimate[2])
ks_test_22_new <- ks.test(data_set_22_new[[1]], "pnorm", mean = norm_params_22_new$estimate[1], sd = norm_params_22_new$estimate[2])

# Overlay fitted distributions on histograms
x_12_new <- seq(min(data_set_12_new[[1]]), max(data_set_12_new[[1]]), length.out = 1000)
x_22_new <- seq(min(data_set_22_new[[1]]), max(data_set_22_new[[1]]), length.out = 1000)
x_32_new <- seq(min(data_set_32_new[[1]]), max(data_set_32_new[[1]]), length.out = 1000)

fitted_12_new <- dnorm(x_12_new, mean = norm_params_12_new$estimate[1], sd = norm_params_12_new$estimate[2])
fitted_22_new <- dnorm(x_22_new, mean = norm_params_22_new$estimate[1], sd = norm_params_22_new$estimate[2])
fitted_32_new <- dgamma(x_32_new, shape = gamma_shape, rate = 1 / gamma_scale)

# Plot for data set 12
ggplot(data = data.frame(x = data_set_12_new[[1]]), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, fill = "blue") +
  geom_line(aes(x = x_12_new, y = fitted_12_new), color = "red", size = 1) +
  labs(title = "Data Set 12 with Fitted Distribution", x = "Value", y = "Density")

# Plot for data set 22
ggplot(data = data.frame(x = data_set_22_new[[1]]), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, fill = "blue") +
  geom_line(aes(x = x_22_new, y = fitted_22_new), color = "red", size = 1) +
  labs(title = "Data Set 22 with Fitted Distribution", x = "Value", y = "Density")

# Plot for data set 32
ggplot(data = data.frame(x = data_set_32_new[[1]]), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, fill = "blue") +
  geom_line(aes(x = x_32_new, y = fitted_32_new), color = "red", size = 1) +
  labs(title = "Data Set 32 with Fitted Distribution", x = "Value", y = "Density")

# Summary of fitting and goodness-of-fit results
fit_results_new <- data.frame(
  Data_Set = c("data set 12", "data set 22", "data set 32"),
  Distribution = c("Normal", "Normal", "Gamma"),
  Shape_Loc = c(norm_params_12_new$estimate[1], norm_params_22_new$estimate[1], gamma_shape),
  Scale = c(norm_params_12_new$estimate[2], norm_params_22_new$estimate[2], gamma_scale),
  KS_Test_Statistic = c(ks_test_12_new$statistic, ks_test_22_new$statistic, ks_test_32_new$statistic),
  KS_Test_p_value = c(ks_test_12_new$p.value, ks_test_22_new$p.value, ks_test_32_new$p.value)
)

print(fit_results_new)
###
###
###
###
###
summary_stats <- data.frame(
  Data_Set = c("data set 12", "data set 22", "data set 32"),
  Mean = c(mean(data_set_12[[1]]), mean(data_set_22[[1]]), mean(data_set_32[[1]])),
  Standard_Deviation = c(sd(data_set_12[[1]]), sd(data_set_22[[1]]), sd(data_set_32[[1]])),
  Minimum = c(min(data_set_12[[1]]), min(data_set_22[[1]]), min(data_set_32[[1]])),
  Maximum = c(max(data_set_12[[1]]), max(data_set_22[[1]]), max(data_set_32[[1]]))
)

# Create histograms to visualize the distributions of each dataset
data_combined <- data.frame(
  Value = c(data_set_12[[1]], data_set_22[[1]], data_set_32[[1]]),
  Data_Set = rep(c("Data Set 12", "Data Set 22", "Data Set 32"),
                 times = c(length(data_set_12[[1]]), length(data_set_22[[1]]), length(data_set_32[[1]])))
)

ggplot(data_combined, aes(x = Value, fill = Data_Set)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Data Sets", x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("blue", "green", "red")) +
  theme_minimal()

# Display summary statistics
print(summary_stats)
###
###
###
###

# Load required libraries
library(MASS)
library(fitdistrplus)
library(ggplot2)

# Assume 'data_cleaned' is a data frame with a column 'Values'
values <- data_set_12
str(data.set.12)
values <- unlist(values)
values <- as.numeric(values)

# Fit the data to an exponential distribution
exp_fit <- fitdist(values, "exp")

x <- seq(min(values), max(values), length.out = 1000)
pdf_exp <- dexp(x, rate = 1 / exp_fit$estimate[1])

# Plot the histogram of the data with the fitted exponential PDF
ggplot(data = data.frame(values), aes(x = values)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  geom_line(data = data.frame(x, pdf_exp), aes(x = x, y = pdf_exp), color = "red", size = 1.2, linetype = "solid") +
  labs(title = "Fitted Exponential Distribution to the Data", x = "Values", y = "Density") +
  theme_minimal()

# Perform Kolmogorov-Smirnov test for goodness-of-fit
ks_exp <- ks.test(values, "pexp", rate = 1 / exp_fit$estimate[1])

# Display the KS test results
ks_results <- data.frame(
  Distribution = "Exponential",
  KS_Statistic = ks_exp$statistic,
  p_value = ks_exp$p.value
)

print(ks_results)
###
###
###
# Load required libraries
library(fitdistrplus)

# Assume data_set_12 is a numeric vector
values <- as.numeric(data_set_12[[1]])

# Fit the Gamma distribution
gamma_fit <- fitdist(values, "gamma")

# Perform Kolmogorov-Smirnov test for goodness-of-fit
ks_gamma <- ks.test(values, "pgamma", shape = gamma_fit$estimate["shape"], rate = gamma_fit$estimate["rate"])

# Extract results
filled_data <- data.frame(
  Data_Set = "data set 12",
  Distribution = "Gamma",
  Shape_Loc = gamma_fit$estimate["shape"],
  Scale = 1 / gamma_fit$estimate["rate"], # Scale is the inverse of rate
  KS_Test_Stat = ks_gamma$statistic,
  KS_Test_p_value = ks_gamma$p.value
)

print(filled_data)
###
###
### QQ plot for dataset12 gamma dist
###
###
###
# Load required libraries
library(fitdistrplus)
library(ggplot2)

# Assume 'data' is a data frame and we're analyzing the first column
column_data <- values

# Fit the Gamma distribution to the column data
gamma_fit <- fitdist(values, "gamma")

# Extract the estimated parameters
shape_param <- gamma_fit$estimate["shape"]
rate_param <- gamma_fit$estimate["rate"]  # Rate is 1/scale in R
scale_param <- 1 / rate_param  # Convert rate to scale

# Generate the theoretical quantiles
theoretical_quantiles <- qgamma(ppoints(length(column_data)), shape = shape_param, rate = rate_param)

# Generate the Q-Q plot
qqplot(theoretical_quantiles, column_data,
       main = "Dataset 12 QQ Plot for Gamma Distribution (Fitted Parameters)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)
### D12 MME MLE
###
### VALUES=DATA12
# Load necessary libraries
library(stats4)



# Extracting the mean and variance from the data
sample_mean <- mean(values)
sample_variance <- var(values)

# Method of Moments Estimation (MME)
alpha_mme <- sample_mean^2 / sample_variance
theta_mme <- sample_variance / sample_mean

# MLE Procedure: Define the negative log-likelihood function for the Gamma distribution
neg_log_likelihood <- function(alpha, theta) {
  if (alpha <= 0 || theta <= 0) {
    return(Inf)  # To ensure parameters stay positive
  }
  -sum(dgamma(values, shape = alpha, scale = theta, log = TRUE))
}

# Optimization to find MLE estimates
mle_fit <- mle(neg_log_likelihood, start = list(alpha = alpha_mme, theta = theta_mme),
               method = "L-BFGS-B", lower = c(1e-6, 1e-6))

# Extract MLE estimates
alpha_mle <- coef(mle_fit)["alpha"]
theta_mle <- coef(mle_fit)["theta"]

# Generating a random sample of size 100 using the Gamma distribution
random_sample <- rgamma(100, shape = alpha_mle, scale = theta_mle)

# Calculating the MME for the random sample
sample_mean_random <- mean(random_sample)
sample_variance_random <- var(random_sample)
alpha_mme_random <- sample_mean_random^2 / sample_variance_random
theta_mme_random <- sample_variance_random / sample_mean_random

# MLE for random sample
neg_log_likelihood_random <- function(alpha, theta) {
  if (alpha <= 0 || theta <= 0) {
    return(Inf)
  }
  -sum(dgamma(random_sample, shape = alpha, scale = theta, log = TRUE))
}

mle_fit_random <- mle(neg_log_likelihood_random, start = list(alpha = alpha_mme_random, theta = theta_mme_random),
                      method = "L-BFGS-B", lower = c(1e-6, 1e-6))

alpha_mle_random <- coef(mle_fit_random)["alpha"]
theta_mle_random <- coef(mle_fit_random)["theta"]

# Prepare a summary of results
results <- data.frame(
  Parameter = c("Alpha (Shape)", "Theta (Scale)"),
  `MME (Original Data)` = c(alpha_mme, theta_mme),
  `MLE (Original Data)` = c(alpha_mle, theta_mle),
  `MME (Random Sample)` = c(alpha_mme_random, theta_mme_random),
  `MLE (Random Sample)` = c(alpha_mle_random, theta_mle_random)
)

print(results)
###
###
### d22 shapiro test
###
# Load required libraries
library(ggplot2)

# Extract the data column
data_column <- as.numeric(data_set_22[[1]])

# Test for normality using the Shapiro-Wilk test
shapiro_test <- shapiro.test(data_column)

# Extract test statistic and p-value
shapiro_test_stat <- shapiro_test$statistic
shapiro_p_value <- shapiro_test$p.value

# Calculate mean and standard deviation
mean_value <- mean(data_column)
std_value <- sd(data_column)

# Generate x values for the normal distribution curve
x <- seq(min(data_column), max(data_column), length.out = 100)

# Generate the normal distribution curve
normal_curve <- dnorm(x, mean = mean_value, sd = std_value)

# Create the histogram with the fitted normal distribution
ggplot(data = data.frame(data_column), aes(x = data_column)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  geom_line(data = data.frame(x, normal_curve), aes(x = x, y = normal_curve), color = "red", size = 1) +
  labs(title = "Data Histogram with Fitted Normal Distribution", x = "Values", y = "Density") +
  theme_minimal()

# Print Shapiro-Wilk test results
shapiro_test_stat
shapiro_p_value

# Generate a Q-Q plot for the data
qqnorm(data_column, main = "Q-Q Plot for Normality Check",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(data_column, col = "red", lwd = 2)  # Add a reference line
###
###
###
### 1-b kismi
###
###
# Load required libraries
library(stats4)
library(dplyr)


population_data <- as.numeric(data_set_32[[1]])

# Generate a random sample of size 100
set.seed(42)
sample <- sample(population_data, size = 100)

# Step 1: Method of Moments Estimation (MME)
sample_mean <- mean(sample)
sample_variance <- var(sample)

alpha_mme <- sample_mean^2 / sample_variance
beta_mme <- sample_mean / sample_variance

# Step 2: Maximum Likelihood Estimation (MLE)
negative_log_likelihood <- function(alpha, beta) {
  if (alpha <= 0 || beta <= 0) return(Inf)
  -sum(dgamma(sample, shape = alpha, rate = beta, log = TRUE))
}

# Minimize the negative log-likelihood
mle_fit <- mle(negative_log_likelihood, 
               start = list(alpha = alpha_mme, beta = beta_mme), 
               method = "L-BFGS-B", 
               lower = c(1e-6, 1e-6))

# Extract MLE estimates
alpha_mle <- coef(mle_fit)["alpha"]
beta_mle <- coef(mle_fit)["beta"]

# Compile the results
estimates <- list(
  MME = list(Alpha = alpha_mme, Beta = beta_mme),
  MLE = list(Alpha = alpha_mle, Beta = beta_mle)
)

print(estimates)
###
###
###d12 transformation denemeleri
###
# Load required libraries
library(MASS)
library(car)
library(dplyr)
install.packages('car')
# Assume 'data' is your dataset, and it contains a column named 'Values'


# Applying various transformations
log_transformed <- log1p(values)  # Logarithmic transformation
sqrt_transformed <- sqrt(values)  # Square root transformation

# Box-Cox transformation (only for positive values)
# Shifting data slightly to ensure all values are positive for Box-Cox
shifted_values <- values + 1e-6
boxcox_result <- boxcox(lm(shifted_values ~ 1), lambda = seq(-2, 2, by = 0.1))
lmbda <- boxcox_result$x[which.max(boxcox_result$y)]  # Optimal lambda
boxcox_transformed <- ifelse(lmbda == 0, log(shifted_values), (shifted_values^lmbda - 1) / lmbda)

data<- values
# Adding the transformations to the dataframe
data <- data %>%
  mutate(
    Log_Transformed = log_transformed,
    Sqrt_Transformed = sqrt_transformed,
    BoxCox_Transformed = boxcox_transformed
  )
length(boxcox_transformed)

# Checking for normality using Shapiro-Wilk test (alternative to D'Agostino's test)
normality_results <- list(
  Original = shapiro.test(values),
  Log_Transformed = shapiro.test(log_transformed),
  Sqrt_Transformed = shapiro.test(sqrt_transformed),
  BoxCox_Transformed = shapiro.test(boxcox_transformed)
)

# Display the transformed dataset
print(data)

# Display normality results
normality_results
#
#
#
#
#
#
# Load required libraries
library(dplyr)

# Extract the data column
data_values <- as.numeric(data_set_22[[1]])

# Theoretical estimation using MME and MLE
# For normal distribution N(μ, σ^2):
# MME: μ = sample mean, σ^2 = sample variance (population variance)
# MLE: μ = sample mean, σ^2 = (1/n) * Σ(x_i - μ)^2 (unbiased variance)

# Calculate MME and MLE estimates
sample_mean <- mean(data_values)  # μ estimate for both MME and MLE
sample_variance_mme <- var(data_values) * (length(data_values) - 1) / length(data_values)  # σ^2 for MME (population variance)
sample_variance_mle <- var(data_values)  # σ^2 for MLE (unbiased variance)

# Generate a random sample of size 100 from the normal distribution using these estimates
set.seed(42)  # For reproducibility
sample <- rnorm(100, mean = sample_mean, sd = sqrt(sample_variance_mle))

# Calculate MME and MLE estimates for the random sample
sample_mean_new <- mean(sample)
sample_variance_mme_new <- var(sample) * (length(sample) - 1) / length(sample)  # MME variance
sample_variance_mle_new <- var(sample)  # MLE variance

# Compile the results into a data frame
results <- data.frame(
  Parameter = c("Mean (μ)", "Variance (σ^2)"),
  `MME Estimate (Original Data)` = c(sample_mean, sample_variance_mme),
  `MLE Estimate (Original Data)` = c(sample_mean, sample_variance_mle),
  `MME Estimate (Random Sample)` = c(sample_mean_new, sample_variance_mme_new),
  `MLE Estimate (Random Sample)` = c(sample_mean_new, sample_variance_mle_new)
)

# Display the results
print(results)
