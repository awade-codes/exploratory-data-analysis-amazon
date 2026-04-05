#===========================================
# Amazon Product Analysis 
# Author: Alyson Wade 
# Course: Foundations in Data Analytics (IT527) - Purdue Global
# Date: April 2026
#===========================================

# Set Working Directory
# getwd() # Find working directory if unknown
setwd("C:/Users/alyso/Desktop/IT527")
 
# Install Packages (Run once if not installed)
#install.packages("readxl")
#install.packages("psych")
#install.packages("ggplot2")

library(readxl)
library(psych)
library(ggplot2)

options(scipen = 999) # Stop viewing everything in scientific notation

# READ in Data 
my_data_frame <- read_excel("amazon.xlsx") 

summary(my_data_frame) # Check Data Summary

# Strip currency symbols (₹), commas, and % signs — convert to numeric
my_data_frame$discounted_price    <- as.numeric(gsub("[^0-9.]", "", my_data_frame$discounted_price))
my_data_frame$actual_price        <- as.numeric(gsub("[^0-9.]", "", my_data_frame$actual_price))
my_data_frame$discount_percentage <- as.numeric(gsub("[^0-9.]", "", my_data_frame$discount_percentage))
my_data_frame$rating_count        <- as.numeric(gsub("[^0-9.]", "", my_data_frame$rating_count))
my_data_frame$rating              <- as.numeric(my_data_frame$rating)

# Verify cleaning worked
str(my_data_frame)        
summary(my_data_frame) 

# Analyze Numeric Values from my data set
metrics <- my_data_frame[, c("discounted_price", "actual_price", "discount_percentage", "rating", "rating_count")]

# Descriptive Statistics
View(describe(metrics))

# Category-level breakdown
category_metrics <- describeBy(metrics, group = my_data_frame$category,mat = TRUE)
View(category_metrics)

# Plot 1: Discount Percentage vs Rating
ggplot(my_data_frame, aes(x = discount_percentage, y = rating)) +
  geom_point(color = "steelblue", alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # this is the trend line
  labs(title = "Discounted Percentage vs Rating",
       x = "Discounted Percentage",
       y = "Rating") +
  theme_minimal()

# Plot 2:r Discounted Price vs Rating 
ggplot(my_data_frame, aes(x = discounted_price, y = rating)) +
  geom_point(color = "steelblue", alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # 👈 this is the trend line
  labs(title = "Discounted Price vs Rating",
       x = "Discounted Price (₹)",
       y = "Rating") +
  theme_minimal()