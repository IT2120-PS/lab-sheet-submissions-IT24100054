setwd("C:\\Users\\it24100054\\Desktop\\IT24100054")
getwd()

branch_data <-read.table("Exercise.txt",header=TRUE,sep=",")

boxplot(branch_data$Sales_X1,
        main = "Boxplot for Sales",
        ylab = "Sales",
        col = "lightblue",
        horizontal = FALSE)

# Five-Number Summary for Advertising
cat("Five-Number Summary for Advertising:\n")
print(summary(branch_data$Advertising_X2))

# IQR for Advertising
cat("IQR for Advertising:\n")
print(IQR(branch_data$Advertising_X2))

# Function to find outliers based on IQR
find_outliers <- function(x) {
  # Calculate the 1st and 3rd quartiles
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  
  # Calculate the IQR (Interquartile Range)
  IQR_val <- Q3 - Q1
  
  # Calculate the lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  # Return the values that are outliers (outside the bounds)
  x[x < lower_bound | x > upper_bound]
}

# Example usage: Find outliers in Advertising_X2 column
outliers <- find_outliers(branch_data$Advertising_X2)
cat("Outliers in Advertising_X2:", outliers, "\n")

