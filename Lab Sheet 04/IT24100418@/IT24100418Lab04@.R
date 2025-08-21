setwd("C:/Users/IT24100418/Desktop/Lab 04-20250821")
branch_data <- read.csv("Exercise.txt", header = TRUE)
head(branch_data)

str(branch_data)

boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales")

str(branch_data$Branch.Sales_X1)
summary(branch_data$Branch.Sales_X1)
sum(is.na(branch_data$Branch.Sales_X1))
head(branch_data$Branch.Sales_X1)


five_num <- fivenum(branch_data$Advertising_X2)
five_num  

iqr_advertising <- IQR(branch_data$Advertising_X2)
iqr_advertising


find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

outliers_years <- find_outliers(branch_data$Years_X3)
outliers_years
