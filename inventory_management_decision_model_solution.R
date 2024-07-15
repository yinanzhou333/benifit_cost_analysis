# ALY6050 Module4 Assignment - A Prescriptive Model for Strategic Decision-making: An Inventory Model
# Yinan Zhou 06/14/2024

# Part1

# Define constants
D <- 15000     # Annual demand
C <- 80        # Cost per unit
h <- 0.18      # Holding cost rate
S <- 220       # Ordering cost per time

# Define a function to calculate total cost
total_cost <- function(Q) {
  ordering_cost <- (D / Q) * S
  holding_cost <- (Q / 2) * C * h
  return(ordering_cost + holding_cost)
}

# Use the optimize function to find the optimal order quantity
opt_result <- optimize(total_cost, c(1, 2000))

# Extract the optimal order quantity and round it up
optimal_Q <- opt_result$minimum
# Calculate the minimum total cost
optimal_cost <- total_cost(optimal_Q)

# Print the results
cat("Optimal Order Quantity:", optimal_Q, "\n")
cat("Minimum Total Cost:", optimal_cost, "\n")

################################################################################

# install.packages("data.table")
library(data.table)

# Generate a sequence of order quantities
Q_values <- seq(500, 1000, by=1)

# Calculate the total cost for each order quantity
costs <- sapply(Q_values, total_cost)

# Create a data table
dt <- data.table(Order_Quantity = Q_values, Total_Cost = costs)

# Print the first few rows of the data table
head(dt)

# Find the index of the minimum total cost
min_cost_index <- which.min(dt$Total_Cost)

# Find the order quantity corresponding to the minimum total cost
optimal_Q <- dt$Order_Quantity[min_cost_index]
optimal_cost <- dt$Total_Cost[min_cost_index]

# Print the optimal order quantity and the corresponding total cost
cat("Optimal Order Quantity:", optimal_Q, "\n")
cat("Minimum Total Cost:", optimal_cost, "\n")

################################################################################

# Plot Total Cost vs Order Quantity
plot(dt$Order_Quantity, dt$Total_Cost, type = "l", col = "black", lwd = 2,
     xlab = "Order Quantity (Q)", ylab = "Total Inventory Cost (TIC)",
     main = "Total Inventory Cost vs Order Quantity")

# Add ablines for optimal Q and optimal cost
abline(v = optimal_Q, col = "red", lwd = 2, lty = 2)
abline(h = optimal_cost, col = "green", lwd = 2, lty = 2)

# Mark the intersection point
points(optimal_Q, optimal_cost, col = "blue", pch = 19, cex = 1.5)
text(optimal_Q, optimal_cost, labels = paste("(", optimal_Q, ", ", round(optimal_cost, 2), ")", sep = ""), 
     pos = 3, offset = 1, col = "blue")

################################################################################

# Part2

# Define parameters
set.seed(123)  # Set seed for reproducibility
n <- 1000 # n_simulations

a <- 13000 # min_demand
b <- 17000 # max_demand
c <- 15000 # mode_demand

# Function to generate random variates from a triangular distribution
rtriangular <- function(n, a, b, c) {
  u <- runif(n)
  return(ifelse(u < (c - a) / (b - a),
                a + sqrt(u * (b - a) * (c - a)),
                b - sqrt((1 - u) * (b - a) * (b - c))))
}

# Generate demand data
D <- rtriangular(n, a, b, c)

# Define constants
C <- 80        # Cost per unit
h <- 0.18      # Holding cost rate
S <- 220       # Ordering cost per time

# Define a function to calculate total cost
total_cost <- function(Q, D, S, C, h) {
  ordering_cost <- (D / Q) * S
  holding_cost <- (Q / 2) * C * h
  return(ordering_cost + holding_cost)
}

# Initialize vectors to store results
optimal_Q <- numeric(n)
min_total_cost <- numeric(n)  
number_of_orders <- numeric(n)

# Iterate through demand vector D, calculating the optimal order quantity and minimum total cost for each demand
for (i in 1:n) {
  # Current demand
  current_D <- D[i]
  # Define the total cost function for the current demand
  current_total_cost <- function(Q) {
    return(total_cost(Q, current_D, S, C, h))
  }
  # Use the optimize function to find the optimal order quantity
  opt_result <- optimize(current_total_cost, c(1, 2000))
  # Extract the optimal order quantity and round it up
  optimal_Q[i] <- ceiling(opt_result$minimum)
  # Calculate the minimum total cost for the optimal order quantity
  min_total_cost[i] <- current_total_cost(optimal_Q[i])
  # Calculate the number of orders based on current demand and optimal order quantity
  number_of_orders[i] <- current_D / optimal_Q[i]
}


################################################################################

# (i) Estimate expected minimum total cost and its 95% confidence interval
mean_min_total_cost <- mean(min_total_cost)
sd_min_total_cost <- sd(min_total_cost)
n_obs <- length(min_total_cost)
se_min_total_cost <- sd_min_total_cost / sqrt(n_obs)
z_score <- qnorm(0.975)  # For 95% confidence interval
ci_min_total_cost <- c(mean_min_total_cost - z_score * se_min_total_cost,
                       mean_min_total_cost + z_score * se_min_total_cost)

# Output the results
cat("Estimated Expected Minimum Total Cost:\n")
cat("Mean:", mean_min_total_cost, "\n")
cat("95% Confidence Interval:", ci_min_total_cost, "\n\n")

# Histogram plot to visualize the distribution
hist(min_total_cost, freq = FALSE, main = "Histogram of Minimum Total Cost")

library(fBasics)
skewness(min_total_cost)
kurtosis(min_total_cost)

# Create a data frame for the histogram plot
df1 <- data.frame(min_total_cost)

# Fit distributions 
library(MASS)
fit_normal1 <- fitdistr(min_total_cost, "normal")

# Create a histogram for 'min_total_cost'
hist_min_total_cost <- hist(min_total_cost, breaks = 500, freq = FALSE)
observed_counts <- hist_min_total_cost$counts
bin_edges <- hist_min_total_cost$breaks

# Calculate expected counts for normal distribution
expected_normal <- diff(pnorm(bin_edges, mean = fit_normal1$estimate["mean"], sd = fit_normal1$estimate["sd"])) * length(min_total_cost)

# Perform Chi-squared Goodness-of-fit test
chi_square_normal <- chisq.test(observed_counts, p = expected_normal, rescale.p = TRUE)

# Print the Chi-squared test result
print(chi_square_normal)

# Create the base plot
library(ggplot2)
p1 <- ggplot(df1, aes(x = min_total_cost)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", alpha = 0.5) +
  geom_density(aes(y = ..density..), fill = "gray", alpha = 0.5) +
  ggtitle("Distribution Comparison") +
  xlab("Minimum Total Cost") +
  ylab("Density")

# Add lines for PDF
p1 <- p1 + stat_function(fun = dnorm, args = list(mean = fit_normal1$estimate["mean"], sd = fit_normal1$estimate["sd"]), 
                       color = "green", size = 1.2, linetype = "dashed")

# Display the plot
p1

################################################################################

# (ii) Estimate expected order quantity and its 95% confidence interval
mean_optimal_Q <- mean(optimal_Q)
sd_optimal_Q <- sd(optimal_Q)
se_optimal_Q <- sd_optimal_Q / sqrt(n_obs)
ci_optimal_Q <- c(mean_optimal_Q - z_score * se_optimal_Q,
                  mean_optimal_Q + z_score * se_optimal_Q)

# Output the results
cat("Estimated Expected Order Quantity:\n")
cat("Mean:", mean_optimal_Q, "\n")
cat("95% Confidence Interval:", ci_optimal_Q, "\n\n")

# Histogram plot to visualize the distribution
hist(optimal_Q, freq = FALSE, main = "Histogram of Expected Order Quantity")

skewness(optimal_Q)
kurtosis(optimal_Q)

# Create a data frame for the histogram plot
df2 <- data.frame(optimal_Q)

# Fit distributions
library(MASS)
fit_normal2 <- fitdistr(optimal_Q, "normal")

# Create a histogram for 'optimal_Q'
hist_optimal_Q <- hist(optimal_Q, breaks = 50, freq = FALSE)
observed_counts <- hist_optimal_Q$counts
bin_edges <- hist_optimal_Q$breaks

# Calculate expected counts for normal distribution
expected_normal <- diff(pnorm(bin_edges, mean = fit_normal2$estimate["mean"], sd = fit_normal2$estimate["sd"])) * length(optimal_Q)

# Perform Chi-squared Goodness-of-fit test
chi_square_normal <- chisq.test(observed_counts, p = expected_normal, rescale.p = TRUE)

# Print the Chi-squared test result
print(chi_square_normal)

# Create the base plot
p2 <- ggplot(df2, aes(x = optimal_Q)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", alpha = 0.5) +
  geom_density(aes(y = ..density..), fill = "gray", alpha = 0.5) +
  ggtitle("Distribution Comparison") +
  xlab("Optimal Order Quantity") +
  ylab("Density")

# Add lines for PDF
p2 <- p2 + stat_function(fun = dnorm, args = list(mean = fit_normal2$estimate["mean"], sd = fit_normal2$estimate["sd"]), 
                       color = "green", size = 1.2, linetype = "dashed")

# Display the plot
p2

################################################################################

# (iii) Estimate expected annual number of orders and its 95% confidence interval
mean_number_of_orders <- mean(number_of_orders)
sd_number_of_orders <- sd(number_of_orders)
se_number_of_orders <- sd_number_of_orders / sqrt(n_obs)
ci_number_of_orders <- c(mean_number_of_orders - z_score * se_number_of_orders,
                         mean_number_of_orders + z_score * se_number_of_orders)

# Output the results
cat("Estimated Expected Annual Number of Orders:\n")
cat("Mean:", mean_number_of_orders, "\n")
cat("95% Confidence Interval:", ci_number_of_orders, "\n")

# Histogram plot to visualize the distribution
hist(number_of_orders, freq = FALSE, main = "Histogram of Expected Annual Number of Orders")

skewness(number_of_orders)
kurtosis(number_of_orders)

# Create a data frame for the histogram plot
df3 <- data.frame(number_of_orders)

# Fit distributions 
library(MASS)
fit_normal3 <- fitdistr(number_of_orders, "normal")

# Create a histogram for 'number_of_orders'
hist_number_of_orders <- hist(number_of_orders, breaks = 50, freq = FALSE)
observed_counts <- hist_number_of_orders$counts
bin_edges <- hist_number_of_orders$breaks

# Calculate expected counts for normal distribution
expected_normal <- diff(pnorm(bin_edges, mean = fit_normal3$estimate["mean"], sd = fit_normal3$estimate["sd"])) * length(number_of_orders)

# Perform Chi-squared Goodness-of-fit test
chi_square_normal <- chisq.test(observed_counts, p = expected_normal, rescale.p = TRUE)

# Print the Chi-squared test result
print(chi_square_normal)

# Create the base plot
library(ggplot2)
p3 <- ggplot(df1, aes(x = number_of_orders)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", alpha = 0.5) +
  geom_density(aes(y = ..density..), fill = "gray", alpha = 0.5) +
  ggtitle("Distribution Comparison") +
  xlab("Expected Annual Number of Orders") +
  ylab("Density")

# Add lines for PDF
p3 <- p3 + stat_function(fun = dnorm, args = list(mean = fit_normal3$estimate["mean"], sd = fit_normal3$estimate["sd"]), 
                         color = "green", size = 1.2, linetype = "dashed")

# Display the plot
p3

################################################################################

# Summarize the results
cat("Estimated Expected Minimum Total Cost:\n")
cat("Mean:", mean_min_total_cost, "\n")
cat("95% Confidence Interval:", ci_min_total_cost, "\n")
cat("Best-fitting distribution for Estimated Expected Minimum Total Cost: Normal\n\n")

cat("Estimated Expected Order Quantity:\n")
cat("Mean:", mean_optimal_Q, "\n")
cat("95% Confidence Interval:", ci_optimal_Q, "\n")
cat("Best-fitting distribution for Estimated Expected Order Quantity: Normal\n\n")

cat("Estimated Expected Annual Number of Orders:\n")
cat("Mean:", mean_number_of_orders, "\n")
cat("95% Confidence Interval:", ci_number_of_orders, "\n")
cat("Best-fitting distribution for Expected Annual Number of Orders: Normal\n\n")


