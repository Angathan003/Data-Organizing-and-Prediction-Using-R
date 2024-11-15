# Question 1:

# Defining data for the two bags
bag1 <- data.frame(color = c('Red', 'Green'), value = c(5, 10))
bag2 <- data.frame(color = c('Blue', 'Yellow'), value = c(15, 20))

# Finding possible sums of values from both bags
possible_sums <- apply(expand.grid(bag1$value, bag2$value), 1, sum)
possible_values <- unique(possible_sums)
possible_values <- sort(possible_values)

# Printing possible values for the sum of numbers from the two bags
cat("a) Possible values for the sum of numbers from the two bags:\n")
cat(possible_values, "\n\n")

# Calculating probabilities for each possible sum
probabilities <- c(5/8 * 2/6, 5/8 * 4/6 + 3/8 * 2/6, 3/8 * 4/6)
pmf_X_df <- data.frame(X = possible_values, Probability = probabilities)

# Defining random variable Y = 2X - 3
Y_values <- 2 * possible_values - 3
pmf_Y_df <- data.frame(Y = Y_values, Probability = probabilities)

# Printing the probability mass function (pmf) for X and Y
cat("b) PMF for the random variable X:\n")
print(pmf_X_df)
cat("\n")

cat("   PMF for the random variable Y:\n")
print(pmf_Y_df)
cat("\n")

# Calculating expected value E(X) and variance Var(X) for X
E_X <- sum(pmf_X_df$X * pmf_X_df$Probability)
Var_X <- sum((pmf_X_df$X - E_X)^2 * pmf_X_df$Probability)

cat("c) Expected value E(X):\n")
print(E_X)

cat("   Variance Var(X):\n")
print(Var_X)
cat("\n")

# Calculating expected value E(Y) and variance Var(Y) for Y
E_Y <- sum(pmf_Y_df$Y * pmf_Y_df$Probability)
Var_Y <- sum((pmf_Y_df$Y - E_Y)^2 * pmf_Y_df$Probability)

cat("d) Expected value E(Y):\n")
print(E_Y)

cat("   Variance Var(Y):\n")
print(Var_Y)
cat("\n")

# Calculating cumulative probabilities for Y
cdf_Y <- cumsum(probabilities)
cdf_Y_df <- data.frame(Y = Y_values, Cumulative_Probability = cdf_Y)

# Printing cumulative probabilities for Y
cat("e) Cumulative probabilities for the random variable Y:\n")
print(cdf_Y_df)
cat("\n")

# Calculating P(Y = 37) using the cdf of Y
p_Y_37 <- pmf_Y_df$Probability[pmf_Y_df$Y == 37]

# Printing probability P(Y = 37)
cat("f) Probability P(Y = 37):\n")
print(p_Y_37)


# Question 2:

# Set seed for reproducibility
set.seed(123)

# Generate a random sample of 500 values from a normal distribution
random_sample <- rnorm(500, mean = 36, sd = 8)

# a) Create a histogram with 10 bins using the range (min to max)
hist(random_sample, breaks = 10, col = "lightblue", main = "Histogram of Random Sample",
     xlab = "Values", ylab = "Frequency", freq = F)

# b) Show the density curve over the histogram constructed above
lines(density(random_sample), col = "red", lwd = 2)

# c) Comment on the histogram and the density curve with respect to the data generated
# The histogram and density curve both depict the distribution of the random sample.
# The histogram visually represents the frequency of values, while the density curve provides a smoothed version of the distribution.
# They both indicate that the data is approximately normally distributed, centered around the mean of 36.

# Question 3:

# a) Create a data frame using the given data and plot x versus y
x <- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)
y <- c(6, 7.25, 8, 9.0625, 10, 11.0625, 12.25, 13.5625, 15)
data <- data.frame(x = x, y = y)

# Plot x versus y
plot(data$x, data$y, main = "Scatterplot of x versus y", xlab = "x", ylab = "y")

# b) Find the Pearson’s correlation coefficient (rx,y)
correlation_coefficient <- cor(data$x, data$y)
cat("b) Pearson's correlation coefficient (rx,y): ", correlation_coefficient, "\n\n")

# c) Comment on the possible reasons for the value of (Rx,y)
# Pearson's correlation coefficient measures the strength and direction of the linear relationship between x and y.
# The coefficient of approximately 0.997 suggests a strong positive linear relationship between x and y.

# d) Find the Pearson’s correlation coefficient (Rx,y) by considering only the last six pairs of data
subset_data <- data[4:9, ]  # Consider only the last six pairs of data
correlation_coefficient_subset <- cor(subset_data$x, subset_data$y)
cat("d) Pearson's correlation coefficient (Rx,y) for the last six pairs of data: ", correlation_coefficient_subset, "\n\n")

# Comment on the possible reasons why rx,y has changed
# The correlation coefficient might change due to the smaller sample size and potential variations in the data subset.

# e) Consider the new variables X1 = 2x − 1 and X2 = x^2
X1 <- 2 * x - 1
X2 <- x^2

# Calculate the Pearson’s correlation coefficients (rx1,y) and (rx2,y)
correlation_coefficient_X1 <- cor(X1, y)
correlation_coefficient_X2 <- cor(X2, y)

cat("e) Pearson's correlation coefficient (rx1,y): ", correlation_coefficient_X1, "\n")
cat("   Pearson's correlation coefficient (rx2,y): ", correlation_coefficient_X2, "\n\n")

# Comment on the relationship between rx,y, rx1,y, and rx2,y
# These correlation coefficients measure the linear relationship between different transformations of x and y.


# Question 4:

# Load the mtcars dataset
data(mtcars)

# a) Print the first ten rows of the "mtcars" dataset
head(mtcars, 10)

# b) Print the 5-number summary of two variables: mpg (miles per gallon) and hp (horsepower)
summary(mtcars$mpg)
summary(mtcars$hp)

# c) Plot a scatter plot to visualize the association between mpg (X-axis) and hp (Y-axis)
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", main = "Scatter Plot of mpg vs hp")

# d) Fit a linear regression model to predict mpg in terms of hp (horsepower)
model <- lm(mpg ~ hp, data = mtcars)
summary(model)

# Plotting the regression line over the data
plot(mtcars$hp, mtcars$mpg, xlab = "Horsepower", ylab = "Miles per Gallon", main = "Scatter Plot of mpg vs hp")
abline(model, col = "red")

# e) Write down the fitted equation of the form Y_hat = beta0 + beta1*X
# By how many units does mpg increase or decrease when hp (horsepower) increases by one unit?
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
cat("Fitted Equation: Y_hat = ", beta0, " + ", beta1, "X\n")
cat("Change in mpg when hp increases by one unit: ", beta1, "\n")

# f) Plot the residuals versus hp (i.e., residual plot) to check for the adequacy of the fitted line and possible outliers
plot(mtcars$hp, residuals(model), xlab = "Horsepower", ylab = "Residuals", main = "Residual Plot")

# g) Predict the mpg for a car with a horsepower of 110
new_hp <- 110
predicted_mpg <- predict(model, newdata = data.frame(hp = new_hp))
cat("Predicted mpg for a car with horsepower 110:", predicted_mpg, "\n")


# Question 5:

# Given parameters
mean_spending <- 5.50
std_dev <- 1.20

# Function to calculate percentile based on mean and standard deviation
percentile_calc <- function(percentile, mean, std_dev) {
  z_score <- qnorm(percentile/100)
  spending <- mean + z_score * std_dev
  return(spending)
}

# a) Find the 90th percentile of customer spending
percentile_90 <- percentile_calc(90, mean_spending, std_dev)
print(paste("90th percentile of customer spending:", round(percentile_90, 2)))

# b) Determine the 25th percentile of customer spending
percentile_25 <- percentile_calc(25, mean_spending, std_dev)
print(paste("25th percentile of customer spending:", round(percentile_25, 2)))

# c) Calculate the median value of customer spending
median_spending <- percentile_calc(50, mean_spending, std_dev)
print(paste("Median value of customer spending:", round(median_spending, 2)))

# d) What percentage of customers spend more than $7.00?
percent_more_than_7 <- pnorm((7 - mean_spending) / std_dev, lower.tail = FALSE) * 100
print(paste("Percentage of customers spending more than $7.00:", round(percent_more_than_7, 2), "%"))


# Quetion 5-b
# Given parameters
p <- 0.05  # probability of being infected in the population
n <- 50    # sample size

# a) Binomial distribution setup
# Specify the distribution with parameters n and p
dist_binomial <- list(n = n, p = p)

# b) Probability that fewer than 3 individuals in the sample are infected
# Calculate the cumulative probability for X less than 3
prob_less_than_3 <- pbinom(2, size = dist_binomial$n, prob = dist_binomial$p)
print(paste("Probability that fewer than 3 individuals in the sample are infected:", round(prob_less_than_3, 4)))

# c) Mean and Variance of X
# Mean
mean_X <- dist_binomial$n * dist_binomial$p
print(paste("Mean of X:", mean_X))

# Variance
var_X <- dist_binomial$n * dist_binomial$p * (1 - dist_binomial$p)
print(paste("Variance of X:", var_X))

# d) New scenario with infection rate of 2% and sample size of 200
# New parameters
p_new <- 0.02
n_new <- 200

# Binomial distribution setup for the new scenario
dist_binomial_new <- list(n = n_new, p = p_new)

# For the new scenario, it's still a binomial distribution
print("For the new scenario:")
print(paste("Mean of X:", dist_binomial_new$n * dist_binomial_new$p))
print(paste("Variance of X:", dist_binomial_new$n * dist_binomial_new$p * (1 - dist_binomial_new$p)))


# Question 6:

# Given dataset
exam_scores <- c(82, 88, 75, 94, 90, 85, 78, 91, 86, 89, 92, 80, 87, 79, 84, 77, 83, 81,
                 76, 93, 88, 85, 89, 90, 82, 86, 75, 91, 79, 84, 78, 95, 88, 87, 93, 86, 82, 89, 90, 80)

# Number of bootstrap samples
n_bootstrap <- 20000

# a) Bootstrap sampling
set.seed(123)  # for reproducibility
bootstrap_means <- replicate(n_bootstrap, mean(sample(exam_scores, replace = TRUE)))

# b) Histogram of bootstrap means
hist(bootstrap_means, main = "Histogram of Bootstrap Means", xlab = "Mean", col = "skyblue", border = "white")

# c) Bootstrap percentile confidence interval for the mean
confidence_interval <- quantile(bootstrap_means, c(0.05, 0.95))
print(paste("90% Bootstrap Percentile Confidence Interval for the Mean:", confidence_interval))

# d) Normal Q-Q plot
qqnorm(bootstrap_means)
qqline(bootstrap_means, col = "red")

