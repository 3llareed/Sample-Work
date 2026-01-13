install.packages("ggplot2") 
library(ggplot2)
library(readr)

my_peaches <- read.csv("peach_data.csv")

# Basic plot
ggplot(my_peaches, aes(x = year, y = price_fresh)) +
  geom_line() +           # line connecting price points over time
  geom_point() +          # points at each time for clarity
  labs(title = "Price Received by Growers for Fresh Peaches Annually",
       x = "Year",
       y = "Price (Cents per Pound)") +
  theme_minimal()

ggplot(my_peaches, aes(x = year, y = price_all)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Price Received by Growers for Peaches Annually (with trend)",
       x = "Year",
       y = "Price (cents per pound)") +
  theme_minimal()

#super simple price over time
plot(my_peaches$year, my_peaches$price_fresh)           # scatter plot
abline(lm(price_fresh ~ year, data = my_peaches))    # add linear trend line

#price over timee
plot(my_peaches$year, my_peaches$price_fresh,
     main = "Price Received by Growers for 
     Fresh Peaches Annually",    # title
     xlab = "Year",                      # x-axis label
     ylab = "Price (cents per pound)",
     cex = 1,
     pch = 16,
     col = "chocolate1")                     # y-axis label

abline(lm(price_fresh ~ year, data = my_peaches))    # add linear trend line

#production graphed over time
plot(my_peaches$year, my_peaches$prod_all,
     main = "Quantity of U.S. Peaches Produced Annually",    # title
     xlab = "Year",                      # x-axis label
     ylab = "Quantity (million pounds)",
     cex = 1,
     pch = 16,
     col = "chocolate1")                     # y-axis label

abline(lm(prod_all ~ year, data = my_peaches))

y_values <- c(my_peaches$price_all, my_peaches$prod_all)
y_values <- y_values[is.finite(y_values)]
ylim = range(y_values)


# PLOT WITH THREE LINES, TREND LINES, AND LEGEND  
# First plot the first line (price) with type="l" for line plot
plot(my_peaches$year, my_peaches$price_all,
     type = "l",            # line plot
     col = "tan1",     # peach color for first line
     lwd = 2,               # line width
     ylim = c(15,85),  # y-axis range to fit both
     main = "Peach Price Recieved by Grower by Use",
     xlab = "Year",
     ylab = "Value")

# Add second line (quantity) with lines()
lines(my_peaches$year, my_peaches$price_fresh,
      col = "steelblue",
      lwd = 2)
# Add third line (quantity) with lines()
lines(my_peaches$year, my_peaches$price_proc,
      col = "salmon",
      lwd = 2)

# add points for clarity
points(my_peaches$year, my_peaches$price_all, col = "tan1", pch = 16)
points(my_peaches$year, my_peaches$price_fresh, col = "steelblue", pch = 16)
points(my_peaches$year, my_peaches$price_proc, col = "salmon", pch = 16)

# Add legend
legend("topleft", legend = c("All", "Fresh", "Processed"),
       col = c("peachpuff", "steelblue", "salmon"), lwd = 2, pch = 16)



# DEFLATED PRICE RECEIVED BY GROWER FRESH VS. PROCESSED
# First plot the first line (price) with type="l" for line plot
plot(my_peaches$year, my_peaches$fresh_dp,
     type = "l",            # line plot
     col = "tan1",     # peach color for first line
     lwd = 2,               # line width
     ylim = c(10,70),  # y-axis range to fit both
     main = "Deflated Price Received by Grower
     Fresh vs. Processed Peaches",
     xlab = "Year",
     ylab = "Price (cents per pound)")

# Add second line (quantity) with lines()
lines(my_peaches$year, my_peaches$proc_dp,
      col = "salmon",
      lwd = 2)

# Optional: add points for clarity
points(my_peaches$year, my_peaches$fresh_dp, col = "tan1", pch = 16)
points(my_peaches$year, my_peaches$proc_dp, col = "salmon", pch = 16)

# Add legend
legend("topleft", legend = c("Fresh", "Processed"),
       col = c("peachpuff", "salmon"), lwd = 2, pch = 16)


peach_yeild_data <- read.csv("supply_peaches.csv")


#plot first line with type=l for line plot 
plot(peach_yeild_data$year, peach_yeild_data$yeild,
     type="l",          
     col = "tan1",     # peach color for first line
     lwd = 2, 
     main = "Annual Peach Yeild",
     xlab = "Year",
     ylab = "Yeild (tons per acre)")
#add points for clarity
points(peach_yeild_data$year, peach_yeild_data$yeild, col = "tan1", pch = 16)



# First plot the first line (price) with type="l" for line plot
plot(peach_yeild_data$year, peach_yeild_data$prod,
     type = "l",            # line plot
     col = "tan1",     # peach color for first line
     lwd = 2,               # line width
     main = "Annual Peach Production",
     xlab = "Year",
     ylab = "Production (million pounds)")
# Optional: add points for clarity
points(peach_yeild_data$year, peach_yeild_data$prod, col = "tan1", pch = 16)

# First plot the first line (price) with type="l" for line plot
plot(peach_yeild_data$year, peach_yeild_data$acres,
     type = "l",            # line plot
     col = "tan1",     # peach color for first line
     lwd = 2,               # line width
     main = "Annual Acres Bearing for Peaches",
     xlab = "Year",
     ylab = "Acres Bearing")
# Optional: add points for clarity
points(peach_yeild_data$year, peach_yeild_data$acres, col = "tan1", pch = 20)


#utilization over timee
plot(my_peaches$year, my_peaches$util_all,
     main = "Utilization of Peaches Annually",    # title
     xlab = "Year",                      # x-axis label
     ylab = "Quantity (million pounds",
     cex = 1,
     pch = 16,
     col = "chocolate1")                     # y-axis label

abline(lm(util_all ~ year, data = my_peaches))

#utilization fresh over timee
plot(my_peaches$year, my_peaches$util_fresh,
     main = "Utilization of Fresh Peaches Annually",    # title
     xlab = "Year",                      # x-axis label
     ylab = "Quantity (million pounds",
     cex = 1,
     pch = 16,
     col = "chocolate1")                     # y-axis label

abline(lm(util_fresh ~ year, data = my_peaches))

#utilization processed over timee
plot(my_peaches$year, my_peaches$util_proc,
     main = "Utilization of Processed Peaches Annually",    # title
     xlab = "Year",                      # x-axis label
     ylab = "Quantity (million pounds",
     cex = 1,
     pch = 16,
     col = "chocolate1")                     # y-axis label

abline(lm(util_proc ~ year, data = my_peaches))


# PLotting three lines at ONCE for Utilization 
#First plot the first line (price) with type="l" for line plot
plot(my_peaches$year, my_peaches$util_all,
     type = "l",            # line plot
     col = "tan1",     # peach color for first line
     lwd = 2,               # line width
     ylim = c(500,2500),  # y-axis range to fit both
     main = "Fresh vs. Processed Annual Utilization of Peaches",
     xlab = "Year",
     ylab = "Quantity (million pounds)")

# Add second line (quantity) with lines()
lines(my_peaches$year, my_peaches$util_proc,
      col = "steelblue",
      lwd = 2)
# Add second line (quantity) with lines()
lines(my_peaches$year, my_peaches$util_fresh,
      col = "salmon",
      lwd = 2)

# Optional: add points for clarity
points(my_peaches$year, my_peaches$util_all, col = "tan1", pch = 16)
points(my_peaches$year, my_peaches$util_proc, col = "steelblue", pch = 16)
points(my_peaches$year, my_peaches$util_fresh, col = "salmon", pch = 16)

# Add legend
legend("topright", legend = c("All", "Fresh", "Processed"),
       col = c("tan1", "steelblue", "salmon"), lwd = 2, pch = 16)






