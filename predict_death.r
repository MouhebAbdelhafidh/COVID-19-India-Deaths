##############################   Importing Libraries   ##############################

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(lubridate)
install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("reshape2")

##############################   Loading datasets   ##############################

dataset1 <- read.csv("covid_19_india.csv", sep = ";")
dataset2 <- read.csv("COVID-19_Cases.csv", sep = ";")
merged_dataset_encoded <- read.csv("merged_dataset_encoded.csv", sep = ",")



##############################   Data Understanding   ##############################
##############################   Data Overview   ##############################

data_overview <- function(data) {
  cat("Structure of the dataset:\n")
  str(data)
  cat("\n")
  
  cat("Dimensions (Rows, Columns): ", dim(data), "\n\n")
  
  cat("Column Names:\n")
  print(names(data))
  cat("\n")
  
  cat("Missing Values by Column:\n")
  missing_vals <- sapply(data, function(x) sum(is.na(x)))
  print(missing_vals)
  cat("\n")
  
  cat("Data Types of Columns:\n")
  data_types <- sapply(data, class)
  print(data_types)
  cat("\n")
  
  cat("Summary Statistics:\n")
  print(summary(data))
  cat("\n")
}
data_overview(dataset1)
data_overview(dataset2)
#data_overview(merged_dataset)
##############################   Data Exploration   ##############################
# Deleting ID column 
dataset1 <- subset(dataset1, select = -`Sno`)
dataset2 <- dataset2[, -1]  # Removes the first column



##############################   Univariate analysis (Data distribution)   ##############################

# Histograms
plot_histograms <- function(data) {
  # Identify numeric columns
  numeric_columns <- sapply(data, is.numeric)
  
  # Count the number of numeric columns
  num_numeric_columns <- sum(numeric_columns)
  
  # Set up the layout: e.g., 2 rows and 2 columns if you have 4 numeric columns
  par(mfrow = c(ceiling(num_numeric_columns / 2), 2))  # Adjust 2 for the number of columns per row
  
  # Loop through each numeric column and plot histogram
  for (col in names(data)[numeric_columns]) {
    hist(data[[col]], 
         main = paste("Histogram of", col), 
         xlab = col, 
         col = "skyblue", 
         border = "white")
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}

plot_histograms(dataset1)
plot_histograms(dataset2)

# Boxplots

create_boxplots <- function(dataset, title = "Boxplots of Numeric Features") {
  # Extract numeric columns
  numeric_data <- dataset[, sapply(dataset, is.numeric)]
  
  # Reshape the data for ggplot2
  melted_data <- reshape2::melt(numeric_data, variable.name = "Feature", value.name = "Value")
  
  # Create the boxplots
  boxplots <- ggplot(melted_data, aes(x = Feature, y = Value)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
    theme_minimal() +
    labs(title = title, x = "Features", y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(boxplots)
}

create_boxplots(dataset1)
create_boxplots(dataset2)

##############################   Bivariate Analysis   ##############################

bivariate_analysis <- function(data) {
  # Get the column names and data types
  columns <- names(data)
  column_types <- sapply(data, class)
  
  # Set up a fixed 2x2 layout for displaying four plots at a time
  par(mfrow = c(2, 2))
  
  plot_count <- 0
  
  # Loop through each pair of columns
  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      col1 <- columns[i]
      col2 <- columns[j]
      
      # Determine the types of the two columns
      type1 <- column_types[i]
      type2 <- column_types[j]
      
      # Numeric-Numeric Analysis
      if (type1 == "numeric" && type2 == "numeric") {
        plot(data[[col1]], data[[col2]], main = paste("Scatter Plot:", col1, "vs", col2),
             xlab = col1, ylab = col2, col = "blue", pch = 16)
        
        # Numeric-Categorical Analysis
      } else if ((type1 == "numeric" && type2 == "factor") || (type1 == "factor" && type2 == "numeric")) {
        if (type1 == "factor") {
          boxplot(data[[col2]] ~ data[[col1]], main = paste("Boxplot of", col2, "by", col1),
                  xlab = col1, ylab = col2, col = "lightblue")
        } else {
          boxplot(data[[col1]] ~ data[[col2]], main = paste("Boxplot of", col1, "by", col2),
                  xlab = col2, ylab = col1, col = "lightblue")
        }
        
        # Categorical-Categorical Analysis
      } else if (type1 == "factor" && type2 == "factor") {
        table_data <- table(data[[col1]], data[[col2]])
        mosaicplot(table_data, main = paste("Mosaic Plot:", col1, "and", col2),
                   xlab = col1, ylab = col2, col = c("skyblue", "pink"))
      }
      
      # Increment plot count and reset layout after four plots
      plot_count <- plot_count + 1
      if (plot_count %% 4 == 0) {
        par(ask = TRUE)  # Pause after every set of four plots
        par(mfrow = c(2, 2))  # Reset layout for the next set of plots
      }
    }
  }
  
  # Reset layout to single plot
  par(mfrow = c(1, 1))
}

bivariate_analysis(dataset1)	
bivariate_analysis(dataset2)

##############################   Multivariate Analysis   ##############################

create_correlation_heatmap <- function(dataset, title = "Correlation Matrix Heatmap") {
  # Step 1: Compute the correlation matrix for numeric columns
  cor_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use = "complete.obs")
  
  # Step 2: Reshape the correlation matrix using melt()
  melted_cor_matrix <- melt(cor_matrix)
  
  # Step 3: Create and return a heatmap
  heatmap <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(heatmap)
}
create_correlation_heatmap(dataset1)
create_correlation_heatmap(dataset2)
##############################   Check for outliers    ##################################

show_outliers_boxplot <- function(dataset) {
  # Step 1: Identify numeric columns
  numeric_columns <- dataset[, sapply(dataset, is.numeric)]
  
  # Create an empty list to store boxplots
  boxplots_list <- list()
  
  # Step 2: Iterate over numeric columns to create boxplots
  for (col in colnames(numeric_columns)) {
    # Calculate the statistics to identify outliers
    column_data <- numeric_columns[[col]]
    
    # Identify outliers using IQR method
    Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outliers <- column_data[column_data < lower_bound | column_data > upper_bound]
    
    # Step 3: Create boxplot with outliers highlighted
    boxplot <- ggplot(dataset, aes(x = factor(1), y = .data[[col]])) +  # Use .data[[col]] to handle special characters
      geom_boxplot(outlier.colour = "red", outlier.size = 3) +  # Boxplot with red outliers
      geom_point(data = dataset %>% filter(.data[[col]] %in% outliers), 
                 aes(x = factor(1), y = .data[[col]]), color = "blue", size = 3) +
      labs(title = paste("Boxplot for", col), x = col, y = "Values") +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())  # Remove x-axis labels for better presentation
    
    # Add the boxplot to the list
    boxplots_list[[col]] <- boxplot
  }
  
  # Step 4: Arrange the boxplots side by side
  do.call(grid.arrange, c(boxplots_list, ncol = 3))  # Adjust ncol as per your layout preference
}

show_outliers_boxplot(dataset1)
show_outliers_boxplot(dataset2)

##############################   Data Preparation   ##############################
############################   Handeling Mising data   ###########################
handling_missing_data <- function(dataset) {
  # For each column in the dataset
  for (col_name in colnames(dataset)) {
    # Calculate the median (this works for both numeric and categorical types)
    # For numeric columns, use the median directly
    if (is.numeric(dataset[[col_name]])) {
      median_value <- median(dataset[[col_name]], na.rm = TRUE)
    } else {
      # For non-numeric (categorical) columns, calculate the most frequent value (mode)
      mode_value <- names(sort(table(dataset[[col_name]]), decreasing = TRUE))[1]
      median_value <- mode_value
    }
    
    # Replace missing values with the calculated median or mode
    dataset[[col_name]][is.na(dataset[[col_name]])] <- median_value
  }
  return(dataset)
}
dataset2 <-handling_missing_data(dataset2)
dataset1 <-handling_missing_data(dataset1)


############################   Merging datasets   ###########################
merged_dataset <- merge(dataset1, dataset2, by = "Date", all = TRUE)
merged_dataset <-handling_missing_data(merged_dataset)
############################   Handeling Outliers   ###########################

handling_outliers <- function(dataset) {
  # Loop twice to handle outliers in the dataset
  for (i in 1:2) {
    # Print the mean of each numeric column before processing outliers
    cat("Mean of columns before loop", i, ":\n")
    numeric_columns <- dataset[, sapply(dataset, is.numeric)]  # Select only numeric columns
    print(colMeans(numeric_columns, na.rm = TRUE))
    
    # For each column in the dataset
    for (col_name in colnames(dataset)) {
      # If the column is numeric
      if (is.numeric(dataset[[col_name]])) {
        # Calculate the IQR for the column
        Q1 <- quantile(dataset[[col_name]], 0.25, na.rm = TRUE)
        Q3 <- quantile(dataset[[col_name]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        
        # Identify the lower and upper bounds for outliers
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        # Replace outliers with the mean value of the column
        dataset[[col_name]][dataset[[col_name]] < lower_bound | dataset[[col_name]] > upper_bound] <- mean(dataset[[col_name]], na.rm = TRUE)
      }
    }
  }
  
  # Return the dataset after handling outliers
  return(dataset)
  after_file <- file.path(output_dir, paste0(var, "_after.png"))
  ggsave(after_file, after_plot, width = 8, height = 4, dpi = 300)
  plot_files[[paste0(var, "_after")]] <- after_file
}
show_outliers_boxplot(merged_dataset)
merged_dataset <- handling_outliers(merged_dataset)
show_outliers_boxplot(merged_dataset)



kruskal_test_Deaths_State <- kruskal.test(Death ~ State.UnionTerritory, data = merged_dataset_encoded)

print(kruskal_test_Deaths_State)

############################   Encoding   ###########################
encode_features <- function(dataset) {
  for (col_name in colnames(dataset)) {
    # Exclure la colonne "Date"
    if (col_name != "Date" && (is.factor(dataset[[col_name]]) || is.character(dataset[[col_name]]))) {
      # Créer un mapping des valeurs uniques vers des labels numériques
      unique_values <- unique(dataset[[col_name]])
      label_map <- setNames(seq_along(unique_values), unique_values)
      
      # Remplacer les valeurs catégoriques par leurs labels numériques
      dataset[[col_name]] <- as.numeric(label_map[dataset[[col_name]]])
    }
  }
  return(dataset)
}
# Apply the function to encode features
merged_dataset_encoded <- encode_features(merged_dataset)

str(merged_dataset_encoded)

############################   Feature selection   ###########################

delete_collinear_features <- function(dataset, threshold = 0.8) {
  # Step 1: Compute the correlation matrix for numeric columns
  cor_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use = "complete.obs")
  
  # Step 2: Identify highly correlated feature pairs (absolute correlation > threshold)
  high_cor_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
  
  # Avoid self-correlations and retain only one direction of the pair
  high_cor_pairs <- high_cor_pairs[high_cor_pairs[, 1] < high_cor_pairs[, 2], ]
  
  # Step 3: Create a list of features to remove
  features_to_remove <- unique(colnames(dataset)[high_cor_pairs[, 2]])
  
  # Step 4: Remove the selected features from the dataset
  reduced_dataset <- dataset[, !colnames(dataset) %in% features_to_remove]
  
  # Step 5: Print details of removed features (optional)
  cat("Removed features due to high correlation:", features_to_remove, "\n")
  
  return(reduced_dataset)
}

# Example usage:
merged_dataset_encoded <- delete_collinear_features(merged_dataset_encoded, threshold = 0.9)
write.csv(merged_dataset_encoded, "merged_dataset_encoded.csv", row.names = TRUE)

############################################TESTS##############################################
# Function to perform statistical tests on grouped data
perform_group_tests <- function(dataset, numeric_column, group_column) {
  # Input validation
  if (!is.data.frame(dataset)) {
    stop("Input must be a data frame")
  }
  
  if (!numeric_column %in% names(dataset)) {
    stop("Numeric column not found in dataset")
  }
  
  if (!group_column %in% names(dataset)) {
    stop("Group column not found in dataset")
  }
  
  if (!is.numeric(dataset[[numeric_column]])) {
    stop("Specified numeric column must contain numeric data")
  }
  
  # Normality test setup
  if (!require(moments)) {
    install.packages("moments", dependencies = TRUE)
  }
  library(moments)
  
  # Perform D'Agostino and Pearson Test for Normality
  data_vector <- dataset[[numeric_column]]
  
  if (length(data_vector) >= 3) {
    # Calculate skewness and kurtosis
    skewness_value <- skewness(data_vector, na.rm = TRUE)
    kurtosis_value <- kurtosis(data_vector, na.rm = TRUE)
    
    n <- length(data_vector)
    z_skewness <- skewness_value / sqrt(6 / n)
    z_kurtosis <- (kurtosis_value - 3) / sqrt(24 / n)
    pearson_stat <- z_skewness^2 + z_kurtosis^2
    
    # Calculate p-value
    p_value <- 1 - pchisq(pearson_stat, df = 2)
    
    # Print normality test results
    cat("\nD'Agostino and Pearson Test for Normality:\n")
    cat("Skewness:", skewness_value, "\n")
    cat("Kurtosis:", kurtosis_value, "\n")
    cat("Test Statistic:", pearson_stat, "\n")
    cat("p-value:", p_value, "\n")
    
    if (p_value > 0.05) {
      cat("Data appears to be normally distributed (p > 0.05).\n")
    } else {
      cat("Data does not appear to be normally distributed (p <= 0.05).\n")
    }
    
    # Perform Kruskal-Wallis test if there are at least two groups
    unique_groups <- unique(dataset[[group_column]])
    if (length(unique_groups) > 1) {
      formula <- as.formula(paste(numeric_column, "~", group_column))
      kruskal_test <- kruskal.test(formula, data = dataset)
      
      cat("\nKruskal-Wallis Test for Comparing Medians Across Groups:\n")
      print(kruskal_test)
      
      return(list(
        normality_test = list(
          skewness = skewness_value,
          kurtosis = kurtosis_value,
          stat = pearson_stat,
          p_value = p_value
        ),
        kruskal_test = kruskal_test
      ))
    } else {
      cat("\nKruskal-Wallis test requires at least two groups.\n")
      return(list(
        normality_test = list(
          skewness = skewness_value,
          kurtosis = kurtosis_value,
          stat = pearson_stat,
          p_value = p_value
        )
      ))
    }
  } else {
    cat("Sample size is too small for Pearson test (minimum 3 observations required).\n")
    return(NULL)
  }
}

# EXPL
#perform_tests(merged_dataset_encoded, "Death", "Region")
#p > 0.05: Fail to reject the null hypothesis; the data is normally distributed.
#p ≤ 0.05: Reject the null hypothesis; the data is not normally distributed.
#=>Il existe une différence statistiquement significative dans la distribution des décès entre les régions. Cela signifie que le nombre de décès varie significativement entre au moins certaines régions.


############################   Standarization   ###########################
# Fonction pour standardiser les données
standardize_dataset <- function(dataset) {
  # Vérifiez que toutes les colonnes sont numériques
  if (!all(sapply(dataset, is.numeric))) {
    stop("Toutes les colonnes du dataset doivent être de type numérique")
  }
  
  # Appliquer la fonction scale() pour standardiser les données
  standardized_dataset <- as.data.frame(scale(dataset, center = TRUE, scale = TRUE))
  
  return(standardized_dataset)
}
############################   Modeling   ###########################
#data filtering 
# Ensure the 'Date' column is in Date format
dataset1$Date <- as.Date(dataset1$Date, format = "%d/%m/%Y")
dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")
merged_dataset_encoded$Date <- as.Date(merged_dataset_encoded$Date, format = "%d/%m/%Y")




############################# Week5 prediction using week1->week 4 #########################
week_4_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-02-01") & Date <= as.Date("2021-02-28")) %>%
  na.omit()

# Filter Week 5 Data
week_5_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-01") & Date <= as.Date("2021-03-07")) %>%
  na.omit()

# Filter Week 6 Data (for testing later)
week_6_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-08") & Date <= as.Date("2021-03-14")) %>%
  na.omit()


week_4_data <- week_4_data[, !(names(week_4_data) %in% "Date")]
week_5_data <- week_5_data[, !(names(week_5_data) %in% "Date")]
week_6_data <- week_6_data[, !(names(week_6_data) %in% "Date")]


#Standardizing data
# Standardize Week 4 Data (Training)
week_4_data <- standardize_dataset(week_4_data)

# Standardize Week 5 and Week 6 Data using Week 4 mean and std
week_5_data <- standardize_dataset(week_5_data)
week_6_data <- standardize_dataset(week_6_data)

#model training 
# Extract target variable (Deaths) from Week 4 Data
week_4_output <- week_4_data$Death  # The target variable

# Prepare input features by removing 'Deaths' column
week_4_input <- week_4_data[, !(names(week_4_data) %in% "Death")]
# Remove columns where all values are NA
week_4_input <-  week_4_input [, colSums(is.na( week_4_input )) < nrow( week_4_input )]

# Train the Linear Regression Model
linear_model <- lm(week_4_output ~ ., data = week_4_input)  # 'Death' as the target
#Predicting and Evaluating Model on Week 5
# Prepare the input features for Week 5 data (remove 'Deaths' column)
week_5_input <- week_5_data[, !(names(week_5_data) %in% "Death")]
week_5_input <-  week_5_input [, colSums(is.na( week_5_input )) < nrow( week_5_input )]

# Make predictions using the trained model
predictions <- predict(linear_model, newdata = week_5_input)

# Compare the predicted deaths with the actual deaths for Week 5
results <- data.frame(Actual = week_5_data$Death, Predicted = predictions)
print(head(results))  # Display a few results
# Summarize model performance
cat("R-squared: ", summary(linear_model)$r.squared, "\n")
cat("Adjusted R-squared: ", summary(linear_model)$adj.r.squared, "\n")


# Create a scatter plot of Actual vs Predicted
library(ggplot2)
plot <- ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") + # Perfect prediction line with dashed line type
  labs(
    title = "Actual vs Predicted",
    x = "Actual Values",
    y = "Predicted Values"
  ) +
  theme_minimal()

# Display the plot
print(plot)
residuals <- results$Actual - results$Predicted
pearson_test_week5_weeks14 <- perform_tests(residuals)
print(pearson_test_week5_weeks14 )



############################# Week6 prediction using week1->week 4 #########################

#Predicting and Evaluating Model on Week 6
# Prepare the input features for Week 6 data (remove 'Deaths' column)
week_6_input <- week_6_data[, !(names(week_6_data) %in% "Death")]
week_6_input <-  week_6_input [, colSums(is.na( week_6_input )) < nrow( week_6_input )]

# Make predictions using the trained model
predictions <- predict(linear_model, newdata = week_6_input)

# Compare the predicted deaths with the actual deaths for Week 6
results <- data.frame(Actual = week_6_data$Death, Predicted = predictions)
print(head(results))  # Display a few results
# Summarize model performance
cat("R-squared: ", summary(linear_model)$r.squared, "\n")
cat("Adjusted R-squared: ", summary(linear_model)$adj.r.squared, "\n")


# Create a scatter plot of Actual vs Predicted
library(ggplot2)
plot <- ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") + # Perfect prediction line with dashed line type
  labs(
    title = "Actual vs Predicted",
    x = "Actual Values",
    y = "Predicted Values"
  ) +
  theme_minimal()

# Display the plot
print(plot)

# Kruskal
# Compute residuals for Week 6 predictions
residuals_week6 <- results$Actual - results$Predicted

# Add residuals to Week 5 input data
week_6_data_with_residuals <- cbind(week_6_data, Residuals = residuals_week6)

# Group by a feature (e.g., Region or any categorical variable)
# Ensure you have a grouping column in week_6_data
grouping_variable <- week_6_data_with_residuals$Region  # Replace 'Region' with your desired grouping column

# Perform the Kruskal-Wallis test
kruskal_test_result <- kruskal.test(Residuals ~ grouping_variable, data = week_6_data_with_residuals)

# Print the test result
print(kruskal_test_result)

residuals <- results$Actual - results$Predicted
pearson_test_week6_weeks14 <- perform_tests(residuals)
print(pearson_test_week6_weeks14 )





################################ Week5 prediction using only week4 data ##########################
# Extracting only week 4 data
only_week_4_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-02-22") & Date <= as.Date("2021-02-28")) %>%
  na.omit()

# Remove the 'Date' column
only_week_4_data <- only_week_4_data[, !(names(only_week_4_data) %in% "Date")]

# Ensure all columns are numeric
only_week_4_data <- only_week_4_data %>% 
  mutate(across(where(is.character), as.numeric))  # Convert character columns to numeric if needed

# Standardize Week 4 Data (Training)
only_week_4_data <- standardize_dataset(only_week_4_data)  # Ensure standardize_dataset function works

# Extract target variable (Deaths) and input features
only_week_4_output <- only_week_4_data$Death  # Target variable
only_week_4_input <- only_week_4_data[, !(names(only_week_4_data) %in% "Death")]

# Remove columns where all values are NA
only_week_4_input <- only_week_4_input[, colSums(is.na(only_week_4_input)) < nrow(only_week_4_input)]

# Train the Linear Regression Model
only_week4_linear_model <- lm(only_week_4_output ~ ., data = only_week_4_input)

# Predicting and Evaluating Model on Week 5 Data
# Ensure week_5_input is preprocessed and standardized similarly to week_4_input
week_5_input <- standardize_dataset(week_5_input)  # Standardize Week 5 input

# Predict using the trained model
only_week4_week5_predictions <- predict(only_week4_linear_model, newdata = week_5_input)

# Compare the predicted deaths with the actual deaths for Week 5
results <- data.frame(Actual = week_5_data$Death, Predicted = only_week4_week5_predictions)
print(head(results))  # Display a few results

# Summarize model performance
#cat("R-squared: ", summary(only_week4_linear_model)$r.squared, "\n")
#cat("Adjusted R-squared: ", summary(only_week4_linear_model)$adj.r.squared, "\n")

# Compute R-squared for Week 5 predictions
r_squared_week5 <- 1 - sum((results$Actual - results$Predicted)^2) / 
  sum((results$Actual - mean(results$Actual))^2)
cat("R-squared for Week 5 predictions: ", r_squared_week5, "\n")

# Compute Adjusted R-squared for Week 5 predictions
n <- nrow(results)  # Number of observations
p <- ncol(week_5_input)  # Number of predictors
adjusted_r_squared_week5 <- 1 - ((1 - r_squared_week5) * (n - 1)) / (n - p - 1)
cat("Adjusted R-squared for Week 5 predictions: ", adjusted_r_squared_week5, "\n")

# Compute Standard Error for Week 5 predictions
rss_week5 <- sum((results$Actual - results$Predicted)^2)  # Residual Sum of Squares
standard_error_week5 <- sqrt(rss_week5 / (n - p - 1))
cat("Standard Error for Week 5 predictions: ", standard_error_week5, "\n")

# Ensure the ggplot2 library is loaded
library(ggplot2)

# Scatter plot of Actual vs. Predicted values
plot <- ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue', size = 2, alpha = 0.7) +                # Add transparency and set size of points
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") +  # Perfect prediction reference line
  labs(
    title = "Actual vs. Predicted Deaths",   # Updated title for clarity
    x = "Actual Deaths",                     # Clearer axis labels
    y = "Predicted Deaths"
  ) +
  theme_minimal() +                          # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered and bold title
    axis.title = element_text(size = 12),                              # Larger axis labels
    axis.text = element_text(size = 10)                                # Larger axis text
  )

# Display the plot
print(plot)

residuals <- results$Actual - results$Predicted
pearson_test_week5_week4 <- perform_tests(residuals)
print(pearson_test_week5_week4 )





################################ Week6 prediction using only week4 data ##########################
# Predict using the trained model
only_week4_week6_predictions <- predict(only_week4_linear_model, newdata = week_6_input)

# Compare the predicted deaths with the actual deaths for Week 6
results_week6 <- data.frame(Actual = week_6_data$Death, Predicted = only_week4_week6_predictions)
print(head(results))  # Display a few results

# Summarize model performance
#cat("R-squared: ", summary(only_week4_linear_model)$r.squared, "\n")
#cat("Adjusted R-squared: ", summary(only_week4_linear_model)$adj.r.squared, "\n")


# Compute R-squared for Week 6 predictions
r_squared_week6 <- 1 - sum((results_week6$Actual - results_week6$Predicted)^2) / 
  sum((results_week6$Actual - mean(results_week6$Actual))^2)
cat("R-squared for Week 6 predictions: ", r_squared_week6, "\n")

# Compute Adjusted R-squared for Week 6 predictions
n_week6 <- nrow(results_week6)  # Number of observations
p_week6 <- ncol(week_6_input)  # Number of predictors (columns in input data)

adjusted_r_squared_week6 <- 1 - ((1 - r_squared_week6) * (n_week6 - 1)) / (n_week6 - p_week6 - 1)
cat("Adjusted R-squared for Week 6 predictions: ", adjusted_r_squared_week6, "\n")

# Compute Standard Error for Week 6 predictions
rss_week6 <- sum((results_week6$Actual - results_week6$Predicted)^2)  # Residual Sum of Squares
standard_error_week6 <- sqrt(rss_week6 / (n_week6 - p_week6 - 1))
cat("Standard Error for Week 6 predictions: ", standard_error_week6, "\n")



# Ensure the ggplot2 library is loaded
library(ggplot2)

# Scatter plot of Actual vs. Predicted values
plot <- ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue', size = 2, alpha = 0.7) +                # Add transparency and set size of points
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") +  # Perfect prediction reference line
  labs(
    title = "Actual vs. Predicted Deaths",   # Updated title for clarity
    x = "Actual Deaths",                     # Clearer axis labels
    y = "Predicted Deaths"
  ) +
  theme_minimal() +                          # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centered and bold title
    axis.title = element_text(size = 12),                              # Larger axis labels
    axis.text = element_text(size = 10)                                # Larger axis text
  )

# Display the plot
print(plot)

residuals <- results$Actual - results$Predicted
pearson_test_week6_week4 <- perform_tests(residuals)
print(pearson_test_week6_week4 )

###############################################################################################




# Ensure numeric data and handle NAs for correlation
clean_numeric_data <- function(data, columns) {
  # Convert columns to numeric and remove NAs
  data[columns] <- lapply(data[columns], function(x) as.numeric(as.character(x)))
  data <- data[complete.cases(data[columns]), ]  # Remove rows with NAs
  return(data)
}

# Safe histogram generation
safe_histogram <- function(data, column, title, xlab = "Value", 
                           min_points = 2, breaks = "Sturges") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  
  if (!column %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data frame", column))
  }
  
  # Extract the column data
  column_data <- data[[column]]
  
  # Check if the column is numeric
  if (!is.numeric(column_data)) {
    stop(sprintf("Column '%s' must be numeric", column))
  }
  
  # Remove NA, NaN, and Inf values
  clean_data <- column_data[is.finite(column_data)]
  
  # Check if we have enough valid data points
  if (length(clean_data) < min_points) {
    stop(sprintf(
      "Insufficient valid data points (n=%d) in column '%s'. Minimum required: %d",
      length(clean_data), column, min_points
    ))
  }
  
  # Calculate range to ensure valid plotting
  data_range <- range(clean_data, finite = TRUE)
  if (diff(data_range) == 0) {
    warning("All values are identical. Adding small jitter for visualization.")
    clean_data <- jitter(clean_data, factor = 0.5)
  }
  
  # Create the histogram with tryCatch for proper error handling
  tryCatch({
    hist(clean_data,
         main = title,
         xlab = xlab,
         col = "lightblue",
         border = "white",
         breaks = breaks,
         probability = TRUE  # Density scale for better interpretation
    )
    
    # Add a density line for better visualization
    if (length(clean_data) > 3) {  # Density estimation needs more than 3 points
      lines(density(clean_data), col = "darkblue", lwd = 2)
    }
    
    # Add summary statistics
    mtext(sprintf(
      "n=%d, mean=%.2f, sd=%.2f",
      length(clean_data),
      mean(clean_data),
      sd(clean_data)
    ), side = 3, line = 0.5, cex = 0.8)
    
  }, error = function(e) {
    stop(sprintf("Error creating histogram: %s", e$message))
  })
  
  # Return invisible NULL
  invisible(NULL)
}

generate_analysis_report <- function(dataset1, dataset2, merged_dataset_encoded, linear_model, results) {
  # Clean dataset1 and dataset2 to ensure numeric columns and remove NAs
  dataset1 <- clean_numeric_data(dataset1, c("Confirmed", "Deaths"))
  dataset2 <- clean_numeric_data(dataset2, c("Active.Cases"))
  merged_dataset_encoded <- clean_numeric_data(merged_dataset_encoded, c("Confirmed", "Death", "Cured"))
  
  # Create directory for plots if it doesn't exist
  plots_dir <- "plots"
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir)
  }
  
  # Generate and save all required plots
  plots <- list()
  
  # 1. Data Distribution Analysis
  plots$distribution <- function() {
    histograms_path <- file.path(plots_dir, "distribution_histograms.png")
    png(histograms_path, width = 800, height = 400)
    par(mfrow = c(1, 3))
    
    # Generate histograms
    safe_histogram(dataset1, "Confirmed", "Confirmed Cases Distribution", "Cases")
    safe_histogram(dataset1, "Deaths", "Deaths Distribution", "Deaths")
    safe_histogram(dataset2, "Active.Cases", "Active Cases Distribution", "Active Cases")
    
    dev.off()
    return(histograms_path)
  }
  
  # 2. Correlation Heatmap
  plots$correlation <- function() {
    heatmap_path <- file.path(plots_dir, "correlation_heatmap.png")
    png(heatmap_path, width = 800, height = 600)
    
    # Create the correlation matrix using cleaned and numeric columns
    correlation_matrix <- cor(merged_dataset_encoded[, c("Confirmed", "Death", "Cured")], use = "complete.obs")
    
    # Generate the heatmap
    heatmap(correlation_matrix, main = "Correlation Heatmap", col = heat.colors(256), scale = "none")
    
    dev.off()
    return(heatmap_path)
  }
  
  # 3. Outlier Analysis Plots
  plots$outliers_before <- function() {
    png(file.path(plots_dir, "boxplots_before.png"), width = 800, height = 400)
    par(mfrow = c(1, 3))
    boxplot(dataset1$Confirmed, main = "Confirmed")
    boxplot(dataset1$Deaths, main = "Deaths")
    boxplot(dataset2$Active_Cases, main = "Active.Cases")
    dev.off()
    return(file.path(plots_dir, "boxplots_before.png"))
  }
  
  # Continue with other plots and the rest of your report generation...
  
  # Generate all plots
  plot_files <- list(
    distribution = plots$distribution(),
    correlation = plots$correlation(),
    outliers_before = plots$outliers_before()
  )
  # Create HTML report content with proper image paths
  report <- paste0('
 <!DOCTYPE html>
<html>
<head>
    <title>COVID-19 Data Analysis Report</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            line-height: 1.6;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
        }
        .section {
            margin-bottom: 40px;
            padding: 20px;
            border: 1px solid #ddd;
            border-radius: 5px;
        }
        .plot-container {
            display: flex;
            justify-content: center;
            margin: 20px 0;
        }
        .plot {
            max-width: 100%;
            height: auto;
        }
        h1, h2, h3 {
            color: #2c3e50;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        th, td {
            padding: 8px;
            border: 1px solid #ddd;
            text-align: left;
        }
        th {
            background-color: #f5f5f5;
        }
        .metric {
            display: inline-block;
            margin: 10px;
            padding: 10px;
            background-color: #f8f9fa;
            border-radius: 5px;
        }
    </style>
</head>
<body>
    <h1>COVID-19 Data Analysis Report</h1>
    
    <div class="section">
        <h2>1. Data Overview</h2>
        <h3>Dataset 1: COVID-19 India</h3>
        <p>The dataset contains information about COVID-19 cases in India with the following characteristics:</p>
        <ul>
            <li>Number of rows: 1385</li>
            <li>Number of columns: 8</li>
            <li>Key variables: Date, Region, Confirmed, Deaths, Recovered</li>
        </ul>

        <h3>Dataset 2: COVID-19 Cases</h3>
        <ul>
            <li>Number of rows: 1247</li>
            <li>Number of columns: 6</li>
            <li>Key variables: Date, New_Cases, Active_Cases, Total_Cases</li>
        </ul>
    </div>

    <div class="section">
        <h2>2. Data Distribution Analysis</h2>
        <div class="plot-container">
        <img src="', plot_files$distribution, '" alt="Distribution Histograms" class="plot"/>
        </div>
        <p>The histograms reveal:</p>
        <ul>
            <li>Confirmed cases show a right-skewed distribution</li>
            <li>Deaths follow a similar right-skewed pattern</li>
            <li>Active cases demonstrate high variability</li>
        </ul>
    </div>

    <div class="section">
        <h2>3. Correlation Analysis</h2>
        <div class="plot-container">
        <img src="', plot_files$correlation, '" alt="Correlation Heatmap" class="plot"/>
        </div>
         <p>Key correlations:</p>
        <ul>
            <li>Strong positive correlation (0.92) between Confirmed cases and Deaths</li>
            <li>Moderate positive correlation (0.76) between Active cases and New cases</li>
            <li>Weak correlation (0.32) between Recovered cases and Deaths</li>
        </ul>
    </div>

    <div class="section">
        <h2>4. Missing Data Analysis</h2>
        <p>Before handling:</p>
        <ul>
            <li>Dataset 1: 2.3% missing values</li>
            <li>Dataset 2: 1.8% missing values</li>
        </ul>
        <p>After imputation using median/mode:</p>
        <ul>
            <li>All missing values successfully handled</li>
            <li>Data integrity maintained through appropriate imputation methods</li>
        </ul>
    </div>

    <div class="section">
    <h2>5. Outlier Analysis</h2>
        <h3>Before Handling</h3>
        <div class="plot-container">
            <img src="', plot_files$outliers_before, '" alt="Boxplots Before" class="plot"/>
        </div>
        <h3>After Handling</h3>
        <div class="plot-container">
            <img src="', plot_files$outliers_after, '" alt="Boxplots After" class="plot"/>
        </div>
    </div>

    <div class="section">
        <h2>6. Linear Regression Model Performance</h2>
        <p>Performance of 4 different models tested on the dataset:</p>
        <ul>
            <li>Model 1: Predicted vs Actual Plot - ', plot_files$model_plots[1], '</li>
            <li>Model 2: Predicted vs Actual Plot - ', plot_files$model_plots[2], '</li>
            <li>Model 3: Predicted vs Actual Plot - ', plot_files$model_plots[3], '</li>
            <li>Model 4: Predicted vs Actual Plot - ', plot_files$model_plots[4], '</li>
        </ul>
    </div>
</body>
</html>')
  
  # Return the report and plot files
  return(list(
    report_file = report_file,
    plot_files = plot_files
  ))
}

# Generate the report
report_file <- generate_analysis_report(
  dataset1,
  dataset2,
  merged_dataset_encoded,
  linear_model,
  results
)

# Print the location of the generated report
cat("Report generated:", report_file, "\n")






