##testing circular x-xis for simulations

mu <- 0
sigma <- 1

x <- seq(-3,3, length.out = 100)

y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y)

x_roll <- c(x - max(x), x, x+ max(x))
view(x_roll)
y_roll <- c(y,y,y)

data <- data.frame(x_roll, y_roll)

ggplot(data, aes(x = x_roll, y = y_roll)) +
  geom_line() +
  scale_x_continuous(limits = c(min(x), max(x)))


start <- 2

# Adjust the x values
x_adj <- ifelse(x < start, x + max(x), x)

# Recalculate the y values for the adjusted x
y_adj <- dnorm(x_adj, mean = mu, sd = sigma)

# Duplicate the adjusted data
x_roll_adj <- c(x_adj - max(x), x_adj, x_adj + max(x))
y_roll_adj <- c(y_adj, y_adj, y_adj)

# Create a data frame for plotting the adjusted data
data_adj <- data.frame(x_roll_adj, y_roll_adj)

# Plot the adjusted distribution with a rolling x-axis
ggplot(data_adj, aes(x = x_roll_adj, y = y_roll_adj)) +
  geom_line() +
  scale_x_continuous(limits = c(min(x_adj), max(x_adj))) +
  labs(title = "Normal Distribution with Circular X-Axis (Adjusted)",
       x = "X-Axis (circular)",
       y = "Density")
