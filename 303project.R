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
