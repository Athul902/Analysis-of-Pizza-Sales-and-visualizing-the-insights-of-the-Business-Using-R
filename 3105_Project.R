

#install libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gridExtra")

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Read the CSV file (replace 'file.csv' with your file path)
data = read.csv(file.choose())
df = data

# Display the first few rows of the data frame
head(df)

# Display the last few rows of the data frame
tail(df)

# Check the dimensions of the data frame
dim(df)

# Standard Deviation
# Calculate standard deviation of total price
sd_total_price <- sd(df$total_price)
cat("Standard Deviation of Total Price:", sd_total_price, "\n")

#Coefficient of Variation (CV)
# Calculate mean of total price
mean_total_price <- mean(df$total_price)

# Calculate coefficient of variation
cv_total_price <- (sd_total_price / mean_total_price) * 100
cat("Coefficient of Variation of Total Price:", cv_total_price, "%\n")

# Display the column names
colnames(df)

# Display the data frame summary
summary(df)

# Check for missing values
sum(is.na(df))

# Check for duplicated rows
sum(duplicated(df))

# Show the number of orders for each category of pizza
categories <- table(df$pizza_category)
categories

# Find the category with the highest number of orders
highest_category <- names(which.max(categories))
highest_category

# Set colors for the bar chart
colors <- ifelse(names(categories) == highest_category, "#EE6A50", "#76EEC6")
# Plot the bar chart
bar_plot <- barplot(categories, horiz = TRUE, col = colors,
                    xlab = "Number of orders", ylab = "Pizza category",
                    main = "Number of orders by pizza categories")

# Add percentage labels to the bars
total <- nrow(df)
percentage_labels <- sprintf("%.0f%%", (categories / total) * 100)
text(categories, bar_plot, labels = percentage_labels, pos = 4, col = "white", cex = 1.2)

# Total number of orders
total_orders <- nrow(df)

# Calculate the percentages of each pizza size
pizza_sizes <- table(df$pizza_size)
size_percentages <- pizza_sizes / total_orders

# Plot the pie chart
pie_labels <- c("L", "M", "S", "XL+XXL")
pie_colors <- c("#7EC0EE", "#EED2EE", "#F4A460", "#388E8E")
pie(size_percentages, labels = pie_labels, col = pie_colors,
    main = "Pizza sizes by orders (%)")

# Identify the best selling pizza
best_selling_pizza <- names(which.max(table(df$pizza_name)))
cat("The best selling pizza is:", best_selling_pizza, "\n")

# Identify the worst selling pizza
worst_selling_pizza <- names(which.min(table(df$pizza_name)))
cat("The worst selling pizza is:", worst_selling_pizza, "\n")

# Calculate mean and median of total prices
mean_price <- mean(df$total_price)
median_price <- median(df$total_price)
cat("Mean: USD", formatC(mean_price, format = "f", digits = 2), "\n")
cat("Median: USD", formatC(median_price, format = "f", digits = 2), "\n")

# Calculate total revenue
total_revenue <- sum(df$total_price)
cat("Total revenue: USD", formatC(total_revenue, 
                                  format = "f", digits = 2), "\n")

# Extract month and month_name from order_date
df$order_date <- as.Date(df$order_date)
df$month <- format(df$order_date, format = "%m")
df$month_name <- format(df$order_date, format = "%B")

# Calculate total revenue by month
revenue_by_month <- df %>%
  group_by(month_name) %>%
  summarise(total_revenue = sum(total_price))

# Rank the months by total revenue
revenue_by_month$rank <- rank(-revenue_by_month$total_revenue)

# Sort the data frame by rank
revenue_by_month <- revenue_by_month %>%
  arrange(rank)

# Create a bar plot of monthly total revenue
custom_colors <- c('#8B475D', '#ff7f0e', '#2ca02c', '#d62728',
                   '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
                   '#17becf', '#B0C4DE', '#FFC0CB')
ggplot(revenue_by_month, aes(x = reorder(month_name, -total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = custom_colors) +
  labs(x = "Month", y = "Total Revenue", title = "Monthly Total Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top pizza analysis
top_pizza_analysis <- df %>%
  group_by(pizza_name) %>%
  summarise(average_unit_price = mean(unit_price),
            revenue_per_pizza = sum(unit_price * quantity)) %>%
  top_n(5, revenue_per_pizza)

# Create a bar plot for revenue by top pizzas
ggplot(top_pizza_analysis, aes(x = reorder(pizza_name, -revenue_per_pizza), y = revenue_per_pizza)) +
  geom_bar(stat = "identity", fill = "#17becf") +
  labs(x = "Pizza Name", y = "Revenue", title = "Revenue by Top Pizzas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of orders per day
daily_orders <- df %>%
  group_by(order_date) %>%
  summarise(num_orders = n()) %>%
  ggplot(aes(x = order_date, y = num_orders)) +
  geom_line(color = "#71C671") +
  labs(x = "Date", y = "Number of orders", title = "Number of orders per day") +
  theme_minimal()
daily_orders

# Revenue per day
daily_revenue <- df %>%
  group_by(order_date) %>%
  summarise(total_revenue = sum(total_price)) %>%
  ggplot(aes(x = order_date, y = total_revenue)) +
  geom_line(color = "#D02090") +
  labs(x = "Date", y = "Revenue", title = "Revenue per day") +
  theme_minimal()
daily_revenue

# Boxplot of unit prices by pizza name
unit_price_boxplot <- ggplot(df, aes(x = pizza_name, y = unit_price)) +
  geom_boxplot() +
  labs(x = "Pizza Name", y = "Price", title = "Boxplot of Unit Prices by Pizza Name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
unit_price_boxplot

#Scatterplot total price vs quantity
plot(df$total_price, df$quantity, 
     xlab = "Total Price", ylab = "Quantity", 
     main = "Scatterplot of Total Price vs. Quantity")

# Boxplot for Total Price
boxplot(df$total_price, 
        main = "Boxplot of Total Price",
        ylab = "Total Price")

# Correlation matrix
correlation_matrix <- cor(df[, c("unit_price", "total_price", "quantity")])
correlation_matrix

#Data Preparation

# Decision Tree Regression (you may need to install the 'rpart' package)
#install.packages("rpart")
library(rpart)
library(rpart.plot)


# Define features and target
X <- df[, c("unit_price", "total_price", "pizza_category", "pizza_name", "month_name", "pizza_size")]
y <- df$quantity

# Split the dataset into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Train a Decision Tree model
dct_model <- rpart(y_train ~ ., data = data.frame(X_train, y_train), method = "anova")

# Make predictions
y_pred <- predict(dct_model, data.frame(X_test))

# Calculate RMSE
rmse <- sqrt(mean((y_test - y_pred)^2))
cat("The RMSE for the Decision Tree model is:", rmse, "\n")

# Calculate R-squared
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
cat("The R-squared for the Decision Tree model is:", r_squared, "\n")

# Create a variable importance plot
var_importance <- round(dct_model$variable.importance, 2)
var_importance_df <- data.frame(Variable = names(var_importance), Importance = var_importance)
var_importance_df <- var_importance_df[order(-var_importance_df$Importance), ]

# Plot the variable importance
par(mar = c(6, 4, 4, 2))
barplot(var_importance_df$Importance, names.arg = var_importance_df$Variable, las = 2, cex.names = 0.7, main = "Variable Importance Plot", col = "blue", horiz = TRUE)

# ANOVA Test for pizza category
#Hypothesis:
#Null Hypothesis (H0): There is no significant difference in the quantity of orders among different pizza categories.
#Alternative Hypothesis (H1): There is a significant difference in the quantity of orders among different pizza categories.
# Perform ANOVA test for pizza category
anova_result <- aov(quantity ~ pizza_category, data = df)

# Print ANOVA summary
summary(anova_result)

# T-Test for Pizza Category (Two-Sample Independent T-Test):
# Extract data for two pizza categories (e.g., Category A and Category B)
category_a <- df$total_price[df$pizza_category == "Chicken"]
category_b <- df$total_price[df$pizza_category == "Classic"]

# Perform two-sample independent t-test
t_test_result <- t.test(category_a, category_b)

# Print t-test result
print(t_test_result)
A
# chi-squared test for independence
#Hypothesis:
#H0: There is no association between pizza size and pizza category
#H1: There is an association between pizza size and pizza category
# Create a contingency table of pizza size vs. pizza category
contingency_table <- table(df$pizza_size, df$pizza_category)

# Perform chi-squared test for independence
chi_squared_result <- chisq.test(contingency_table)

# Print chi-squared test result
print(chi_squared_result)




