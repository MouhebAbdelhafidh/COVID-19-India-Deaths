##############################   Importing Libraries   ##############################

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("reshape2")
##############################   Loading datasets   ##############################

dataset1 <- read_csv("covid_19_india.csv")
dataset2 <- read_csv("COVID-19 Cases(22-04-2021).csv")

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

# Standardize the Date format in dataset1
#dataset1$Date <- as.Date(dataset1$Date, format = "%m/%d/%Y")  # Adjust format based on dataset1
# Standardize the Date format in dataset2
#dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")  # Adjust format based on dataset2
data_overview(dataset1)
data_overview(dataset2)
#data_overview(merged_dataset)
##############################   Data Exploration   ##############################
# Deleting ID column
dataset1 <- subset(dataset1, select = -Sno)
dataset2 <- subset(dataset2, select = -`S. No.`)

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

#plot_histograms(dataset1)
#plot_histograms(dataset2)

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

#create_boxplots(dataset1)
#create_boxplots(dataset2)

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

#bivariate_analysis(dataset1)	
#bivariate_analysis(dataset2)

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
#create_correlation_heatmap(dataset1)
#create_correlation_heatmap(dataset2)

##############################   Chek for outliers    ##################################

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

#show_outliers_boxplot(dataset1)
#show_outliers_boxplot(dataset2)

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
############################   Merging datasets   ###########################

merged_dataset <- merge(dataset1, dataset2, by = "Date", all = TRUE)

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
}
show_outliers_boxplot(merged_dataset)
merged_dataset <- handling_outliers(merged_dataset)
show_outliers_boxplot(merged_dataset)
