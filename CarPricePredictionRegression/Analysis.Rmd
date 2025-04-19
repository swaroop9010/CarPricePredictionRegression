---
title: "Statistical Learning"
output: html_document
---

library(tidyverse)  # for data manipulation and visualization
library(modelr)     # for easy model fitting
library(broom)      # for tidying model outputs

# Assuming the data is in a CSV file named 'gcar_data.csv'
car_data <- read.csv("gcar_data.csv", stringsAsFactors = TRUE)

# Convert 'price_in_euro' to numeric if needed
car_data$price_in_euro <- as.numeric(gsub(",", "", car_data$price_in_euro))

# Preview the unique values that might not be numeric
unique(car_data$price_in_euro)

# Remove commas, euro symbols, and spaces
car_data$price_in_euro <- gsub(",", "", car_data$price_in_euro)
car_data$price_in_euro <- gsub("???", "", car_data$price_in_euro)
car_data$price_in_euro <- gsub(" ", "", car_data$price_in_euro)

# Convert to numeric
car_data$price_in_euro <- as.numeric(car_data$price_in_euro)

# Check for NA values which indicate conversion issues
sum(is.na(car_data$price_in_euro))

mean_price <- mean(car_data$price_in_euro, na.rm = TRUE)
car_data$price_in_euro[is.na(car_data$price_in_euro)] <- mean_price

car_data <- na.omit(car_data)

library(ggplot2)

# Histogram of price_in_euro
ggplot(car_data, aes(x = price_in_euro)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Car Prices", x = "Price in Euro", y = "Frequency") +
  theme_minimal()
  
# Boxplot of price_in_euro
ggplot(car_data, aes(y = price_in_euro)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Boxplot of Car Prices", y = "Price in Euro") +
  theme_minimal()

# Summary statistics for numeric variables
summary(select(car_data, -contains("_")))

# QQ plot of price_in_euro
qqnorm(car_data$price_in_euro)
qqline(car_data$price_in_euro, col = "red", lwd = 2)

# Subset data with numerical predictors
numerical_data <- select(car_data, power_kw, mileage_in_km, year, fuel_consumption_l_100km)

library(GGally)
ggpairs(numerical_data, cardinality_threshold = 500)

# Re-load or re-examine the original data (if possible, show the structure or a snapshot)
str(numerical_data)
head(numerical_data)

# Check if 'fuel_consumption_l_100km' was accidentally converted incorrectly
unique(numerical_data$fuel_consumption_l_100km)

# Re-apply conversion logic cautiously
numerical_data$fuel_consumption_l_100km <- as.numeric(as.character(numerical_data$fuel_consumption_l_100km))
numerical_data$fuel_consumption_l_100km[numerical_data$fuel_consumption_l_100km == 2006] <- NA

# Safely convert other columns to numeric if they are still not
numerical_data$power_kw <- as.numeric(as.character(numerical_data$power_kw))
numerical_data$mileage_in_km <- as.numeric(as.character(numerical_data$mileage_in_km))
numerical_data$year <- as.numeric(as.character(numerical_data$year))

# Check again for NAs
sum(is.na(numerical_data$power_kw))
sum(is.na(numerical_data$mileage_in_km))
sum(is.na(numerical_data$year))
sum(is.na(numerical_data$fuel_consumption_l_100km))

# Review the summary to ensure data integrity
summary(numerical_data)

# Apply NA removal only after ensuring the data is properly converted
numerical_data <- na.omit(numerical_data)
summary(numerical_data)

# Using base R's pairs function for simplicity and to avoid further complications
if(nrow(numerical_data) > 0) {
  pairs(~ power_kw + mileage_in_km + year + fuel_consumption_l_100km, data = numerical_data, main = "Scatterplot Matrix")
} else {
  cat("No data available to plot.")
}

ggplot(numerical_data, aes(x = power_kw)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Power KW", x = "Power KW", y = "Frequency")
  
# Analyze impact of a categorical variable 'transmission_type' on 'price_in_euro'
ggplot(car_data, aes(x = transmission_type, y = price_in_euro)) +
  geom_boxplot() +
  labs(title = "Impact of Transmission Type on Car Prices", x = "Transmission Type", y = "Price in Euro")

# histogram for 'power_kw'
ggplot(numerical_data, aes(x = power_kw)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Power KW", x = "Power (KW)", y = "Count")

names(numerical_data)

names(car_data)

# Assuming numerical_data is created by selecting specific columns from car_data
numerical_data <- car_data[, c("power_kw", "mileage_in_km", "year", "fuel_consumption_l_100km", "price_in_euro")]

# Verify the structure
summary(numerical_data)


# Assuming 'Unnamed..0' can serve as a unique identifier and exists in both datasets
if("Unnamed..0" %in% names(car_data) && "Unnamed..0" %in% names(numerical_data)) {
  numerical_data <- merge(numerical_data, car_data[, c("Unnamed..0", "price_in_euro")], by = "Unnamed..0")
} else {
  cat("No appropriate key found for merging. Ensure both data sets include a common unique identifier.\n")
}

# Verify the addition
summary(numerical_data$price_in_euro)

# Manual alignment example
numerical_data <- car_data[, c("power_kw", "mileage_in_km", "year", "fuel_consumption_l_100km")]
numerical_data$price_in_euro <- car_data$price_in_euro

# Verify the structure again
summary(numerical_data)

ggplot(numerical_data, aes(x = power_kw, y = price_in_euro)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship Between Power KW and Price in Euro", x = "Power KW", y = "Price in Euro")

# Ensure all variables are numeric before calculating correlations
car_data$price_in_euro <- as.numeric(as.character(car_data$price_in_euro))
car_data$power_kw <- as.numeric(as.character(car_data$power_kw))
car_data$mileage_in_km <- as.numeric(as.character(car_data$mileage_in_km))
car_data$year <- as.numeric(as.character(car_data$year))  # Typically, 'year' should already be numeric, but just to be sure

# Check the structure to confirm changes
str(car_data[c("price_in_euro", "power_kw", "mileage_in_km", "year")])

# Calculate the correlation matrix
correlations <- cor(car_data[, c("price_in_euro", "power_kw", "mileage_in_km", "year")], use = "complete.obs")

library(corrplot)

# Plot the correlation matrix
corrplot(correlations, method = "circle")


# Visualizing relationships and distributions
library(ggplot2)
ggplot(car_data, aes(x = mileage_in_km, y = price_in_euro)) + 
    geom_point() + 
    geom_smooth(method = "lm", col = "blue") + 
    labs(title = "Price vs. Mileage", x = "Mileage in km", y = "Price in Euro")

# Boxplot to see price distribution by car brand
ggplot(car_data, aes(x = brand, y = price_in_euro)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Price Distribution by Brand", x = "Brand", y = "Price in Euro")

# Use the model.matrix function to create dummy variables excluding the intercept
car_data_model <- model.matrix(~ brand + model + color + transmission_type + fuel_type - 1, data = car_data)

# Create a new dataframe for the regression analysis by combining the dummy variables with other numeric predictors
regression_data <- cbind(car_data_model, car_data[c("price_in_euro", "power_kw", "mileage_in_km", "year", "fuel_consumption_l_100km")])

library(stats)

# Fit the linear regression model
model <- lm(price_in_euro ~ ., data = regression_data)

# Output the summary of the model to view results
summary(model)
show(model)

model <- lm(price_in_euro ~ power_kw + mileage_in_km + year + as.factor(brand) + as.factor(transmission_type), data = car_data)

# Check for aliasing in the summary
summary(model)

library(car)  # Ensure the library is loaded
vif_values <- vif(model)  # This should work now without error
print(vif_values)

# model is stored in a variable called 'model'
summary_output <- summary(model)
print(summary_output)

dummies <- model.matrix(~ brand - 1, data = car_data)  # '-1' omits intercept

# View the first few rows of the dummy variables
head(dummies)

car_data_model <- cbind(car_data, dummies)
car_data_model$brand <- NULL  # Remove the original 'brand' column
str(car_data_model)


library(caret)

sapply(car_data, function(x) if(is.factor(x)) length(levels(x)) else NA)
summary(car_data)

# Identifying the most common models (you can adjust the number as needed)
top_models <- names(sort(table(car_data$model), decreasing = TRUE)[1:20])
car_data$model <- ifelse(car_data$model %in% top_models, car_data$model, 'Other')

# Now `model` has fewer levels
table(car_data$model)


sapply(car_data[c("brand", "model", "transmission_type", "fuel_type")], function(x) length(levels(factor(x))))

dummies <- dummyVars(~ model + transmission_type + fuel_type, data = car_data)
car_data_transformed <- predict(dummies, newdata = car_data)
car_data_transformed <- as.data.frame(car_data_transformed)

# Check the transformed data
head(car_data_transformed)

# Remove 'brand' from the dummy variable creation process
dummies <- dummyVars(~ model + transmission_type + fuel_type, data = car_data)

# Generate the dummy encoded data
car_data_transformed <- predict(dummies, newdata = car_data)
car_data_transformed <- as.data.frame(car_data_transformed)

# Check the structure and view the first few rows of the transformed data
str(car_data_transformed)
head(car_data_transformed)

# Check unique values in the 'brand' column
unique(car_data$brand)

# decision is to exclude 'brand'
dummies <- dummyVars(~ model + transmission_type + fuel_type, data = car_data)

# Apply dummyVars
car_data_transformed <- predict(dummies, newdata = car_data)
car_data_transformed <- as.data.frame(car_data_transformed)

# View the structure to confirm changes
str(car_data_transformed)
head(car_data_transformed)

# Continue with any further analysis, e.g., fitting a regression model
model <- lm(price_in_euro ~ ., data = car_data_transformed)
summary(model)

colnames(car_data_transformed)

car_data_transformed$price_in_euro <- car_data$price_in_euro

# Verify the column has been added correctly
head(car_data_transformed$price_in_euro)

sum(is.na(car_data_transformed$price_in_euro))

car_data_transformed <- na.omit(car_data_transformed)

# Set seed for reproducibility
set.seed(123)

# Define training control
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using caret's train function
model_cv <- train(price_in_euro ~ ., data = car_data_transformed, method = "lm", trControl = train_control)

# Print the model's cross-validation results
print(model_cv)

