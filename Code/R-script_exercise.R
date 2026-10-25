# BIOS 6621: Practice  Coding  R Script
# Author: Adarsh Sharma
# Date: Sept 19, 2026
#==============================================================
#==============================================================

# US execution data
Executions <- c(0, 1, 0, 2, 0, 1, 2, 5, 21, 18, 18, 25, 11, 16, 23, 14, 31, 38, 28)
Year <- 1976:1994

x <- c(1, 3, 4, 6, 9, 10, 11, 14)
y1 <- c(102, 101, 89, 92, 81, 80, 75, 72)
y2 <- c(88, 85, 81, 80, 76, 71, 66, 64)
lab <- c("a", "b", "c", "d", "e", "f", "g", "h")

xy.mat <- cbind(x, y1, y2)

# Exercise 1: Adding horizontal lines at the mean and maximum executions.

plot(Year, Executions, pch = 19,
     main = "Execution in the U.S. from 1976-1994 ",
     xlab = "Year", ylab = "Executions",
     col = "blue")
abline(h = mean(Executions), col = "red")
abline(h = max(Executions), col = "blue")

# Label for each horizontal line
text(x = 1980, y = mean(Executions),
     labels = "Mean", col = "red", pos = 3)

text(x = 1985, y = max(Executions),
     labels = "Maximum", col = "blue", pos = 1)

# Exercise 2: Color points before 1984 red and keep the remaining points blue.

plot(Year, Executions, pch = 19,
     main = "Execution in the U.S. from 1976-1994",
     xlab = "Year", ylab = "Executions",
     col = "blue")

# Add red points over the observations before 1984.
points(Year[Year < 1984], Executions[Year < 1984],
       pch = 19, col = "red")

abline(v = 1983.5, lty = 2)

legend("topleft", legend = c("Before 1984", "1984 and after"),
       col = c("red", "blue"),
       pch = 19)

# Exercise 3: Repeat each value twice, then repeat the entire pattern three times.

# Generate 0.2, 0.4, and 0.6
values <- seq(0.2, 0.6, by = 0.2)

# Repeat values twice
pattern <- rep(values, each = 2)

# Repeat pattern three times
rep(pattern, times = 3)

# Exercise 4
set.seed(6621)

# Simulate 10000 observations with mean 10 and SD 4
normal_d <- rnorm(10000, mean = 10, sd = 4)

# Plot the simulated values
hist(normal_d,
     main = "Histogram of Simulated Normal Data",
     xlab = "Simulated value",
     col = "grey", border = "white")
# The histogram is roughly bell-shaped and centered around 10.

# Compare the sample mean and SD with 10 and 4
mean(normal_d)
sd(normal_d)

# The sample mean was 9.997 and SD was 3.986, both close to 10 and 4. 
#In Random Sampling small differences are expected.

# Find the probability of a value above 18
1 - pnorm(18, mean = 10, sd = 4)
# About 2.28% of values in this distribution are greater than 18.

# Find the value with 97.5% of the distribution below it
qnorm(0.975, mean = 10, sd = 4)
# This value is approximately 17.84.

# x values to draw the density curve
normal_x <- seq(-6, 26, by = 0.01)

plot(normal_x, dnorm(normal_x, mean = 10, sd = 4),
     type = "l",
     main = "Normal Density: Mean 10 and SD 4",
     xlab = "x", ylab = "Density")
# The density curve is symmetric around 10 and peaks at the mean.


# Exercise 5 ----------------------------------------------------------
# Generate Gamma data with mean 10 and SD 4.

gamma_shape <- 10^2 / 4^2
gamma_rate <- 10 / 4^2
gamma_data <- rgamma(10000,
                     shape = gamma_shape,
                     rate = gamma_rate)
mean(gamma_data)
sd(gamma_data)

# Exercise 6: Explain each statement. 

# Selects 1 through 5 of x because 2 is subtracted from the indices.
x[c(3:7) - 2]

# Selects 3 through 7 of x then subtracts 2 from their values.
x[c(3:7)] - 2

# Error in xy.mat[14, ] : subscript out of bounds
# Attempts to select row 14 and all columns but xy.mat has only 8 rows.
xy.mat[14, ]

# Selects elements 2, 3, and 4 in column major order giving rows 2-4 of column1.
xy.mat[c(2:4)]

# Combines x and lab as columns of a matrix converting x to character values.
cbind(x, lab)

# Exercise 7: Plot y2 versus x from xy.mat, excluding row 4, without new assignments.

plot(xy.mat[-4, "x"], xy.mat[-4, "y2"],
     xlab = "x", ylab = "y2")

