#################################################
## Exploratory Data Analysis and Visualization
## Final Project
##
## 1. Housing Data
## 1.1. Data summary, oddities, and outliers
## 1.2. Data cleaning
## 1.3. One-variable visuals
## 1.4. Two-variable visuals
## 2. School Data
## 2.1. Data summary, oddities, and outliers
## 2.2. Data cleaning
## 2.3. One-variable visuals
## 2.4. Two-variable visuals
## 3. Merged Data
## 3.1. Data cleaning and summary
## 3.2. Two-variable visuals
## 3.3. Analysis
## 4. Sensitivity Analysis
#################################################

# setting working directory
getwd()
setwd("/Users/jess/Documents/UCLA/Fall2023 Exploratory Data Analysis/Final Assignment")

# Load any libraries that may be necessary
library(ggplot2)
library(dplyr)
library(MASS)
library(reshape2)
library(e1071)
library(car)
library(corrplot)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Housing dataset

## 1. Data summary, oddities and outliers

# read housing file into R
housing <- read.csv("housing.csv", header=T)
head(housing)

# Data summary, oddities, and outliers
summary(housing)

#from summary can see:
# outlier in beds - max is 999 where 3rd Qu is 4
# outlier in baths - max is 25 and 3rd Qu is 2.5
# NA's in sqft = 2
# NA's in lot size = 20
# Mistake and an outlier in year - year 2111 has not happened yet
#sold price has an outlier - miin is 664 and 1st Qu is 974500  

# I plan to delete the NA's and outliers as they are a small part of the dataset and are mistakes

#after cleaning I will check for oddities in the character variables 
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.1. Data Checking and Cleaning:

# Changing characters to factors 
housing$neighborhood <- as.factor(housing$neighborhood)
housing$type <- as.factor(housing$type)
housing$levels <- as.factor(housing$levels)
housing$cooling <- as.factor(housing$cooling)
housing$heating <- as.factor(housing$heating)
housing$fireplace <- as.factor(housing$fireplace)
housing$elementary <- as.factor(housing$elementary)
housing$middle <- as.factor(housing$middle)
housing$high <- as.factor(housing$high)

summary(housing)

# levels, cooling, heating and fireplace all have null values - I will remove these

# town house and townhouse are the same - need to be renamed
# condo and condominium are the same - need to be renamed

# Checking the current levels to update them
levels(housing$type)

# Replace levels
levels(housing$type)[levels(housing$type) == "condominium"] <- "condo"
levels(housing$type)[levels(housing$type) == "town house"] <- "townhouse"

levels(housing$type)

# Removing rows with NULL factor values

# Remove rows in levels where type is "?"
housing <- housing[housing$levels != "?", ]

# Convert "levels" to a factor
housing$levels <- factor(housing$levels)

# Check the updated levels
levels(housing$levels)

# Remove rows in cooling where type is ""
housing <- housing[housing$cooling != "", ]

# Convert "cooling" to a factor
housing$cooling <- factor(housing$cooling)

# Check the updated cooling for type
levels(housing$cooling)

# Remove rows in heating where type is ""
housing <- housing[housing$heating != "", ]

# Convert "cooling" to a factor
housing$heating <- factor(housing$heating)

# Check the updated cooling for type
levels(housing$heating)

# Remove rows in fireplace where type is ""
housing <- housing[housing$fireplace != "", ]

# Convert "cooling" to a factor
housing$fireplace <- factor(housing$fireplace)

# Check the updated cooling for type
levels(housing$fireplace)

summary(housing)

# removing NA's in number variables

# Remove rows where sqft is NA
housing <- housing[!is.na(housing$sqft), ]
summary(housing)

# Remove rows where lotsize is NA
housing <- housing[!is.na(housing$lotsize), ]
summary(housing)

# checking for and removing outliers

# BATHS outliers
# Calculate IQR
baths_iqr <- IQR(housing$baths)

# Define the lower and upper bounds
lower_bound <- quantile(housing$baths)[2] - 1.5 * baths_iqr
upper_bound <- quantile(housing$baths)[4] + 1.5 * baths_iqr

# Identify outliers
outliers <- housing$baths < lower_bound | housing$baths > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing[outliers, ]

# View the subset
print(outliers_subset)

# I only choose to remove the extreme outlier of 25 in line "250" as the other outliers have 6 bedrooms and 5 baths makes sense
# and 25 bathrooms is clearly a capturing error

# Remove rows where baths is equal to 25
housing <- housing[housing$baths != 25, ]

summary(housing$baths)

summary(housing)

# BEDS outliers
# Calculate IQR
beds_iqr <- IQR(housing$beds)

# Define the lower and upper bounds
lower_bound <- quantile(housing$beds)[2] - 1.5 * beds_iqr
upper_bound <- quantile(housing$beds)[4] + 1.5 * beds_iqr

# Identify outliers
outliers <- housing$beds < lower_bound | housing$beds > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing[outliers, ]

# View the subset
print(outliers_subset)

# I chose to not remove outliers in beds as they are not extreme and are possible to have 1 and 6 bedroom houses

# SQFT outliers
# Calculate IQR
sqft_iqr <- IQR(housing$sqft)

# Define the lower and upper bounds
lower_bound <- quantile(housing$sqft)[2] - 1.5 * sqft_iqr
upper_bound <- quantile(housing$sqft)[4] + 1.5 * sqft_iqr

# Identify outliers
outliers <- housing$sqft < lower_bound | housing$sqft > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing[outliers, ]

# View the subset
print(outliers_subset)

# I chose to keep all sqft outliers as they are all 6 bedroom houses so they are not extreme and are not
# capturing errors

# SQFT outliers
# Calculate IQR
sqft_iqr <- IQR(housing$sqft)

# Define the lower and upper bounds
lower_bound <- quantile(housing$sqft)[2] - 1.5 * sqft_iqr
upper_bound <- quantile(housing$sqft)[4] + 1.5 * sqft_iqr

# Identify outliers
outliers <- housing$sqft < lower_bound | housing$sqft > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing[outliers, ]

# View the subset
print(outliers_subset)

# I choose to not remove outliers as they are not extreme and make sense with the other dimensions
# of each propert

# lotsize outliers
# Calculate IQR
lotsize_iqr <- IQR(housing$lotsize)

# Define the lower and upper bounds
lower_bound <- quantile(housing$lotsize)[2] - 1.5 * lotsize_iqr
upper_bound <- quantile(housing$lotsize)[4] + 1.5 * lotsize_iqr

# Identify outliers
outliers <- housing$lotsize < lower_bound | housing$lotsize > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing[outliers, ]

# View the subset
print(outliers_subset)

# I choose not to remove outliers as their other variables make sense for the size of the lot

# Year outliers
# Calculate IQR
year_iqr <- IQR(housing$year)

# Define the lower and upper bounds
lower_bound <- quantile(housing$year)[2] - 1.5 * year_iqr
upper_bound <- quantile(housing$year)[4] + 1.5 * year_iqr

# Identify outliers
outliers <- housing$year < lower_bound | housing$year > upper_bound

# Remove outliers
housing <- housing[!outliers, ]

summary(housing$year)

# I chose to remove the year outliers as they were data capturing errors and impossible for houses
#to be built in 1495 or 2111

# soldprice outliers
# Calculate IQR
soldprice_iqr <- IQR(housing$soldprice)

# Define the lower and upper bounds
lower_bound <- quantile(housing$soldprice)[2] - 1.5 * soldprice_iqr
upper_bound <- quantile(housing$soldprice)[4] + 1.5 * soldprice_iqr

# Identify outliers
outliers <- housing$soldprice < lower_bound | housing$soldprice > upper_bound

# Remove outliers
housing <- housing[!outliers, ]

summary(housing$year)
summary(housing)
# I chose to remove the soldprice outlier as it was an extreme outlier
# and clearly a data capturing error

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.3. One-variable visuals

# Create a histogram of soldprice
hist(housing$soldprice, main = "Histogram of Sold Price", xlab = "Sold Price", col = "skyblue")

# Create a histogram of sqft
hist(housing$sqft, main = "Histogram of Squarefoot of Houses", xlab ="Sqft", ylab = "Count", col = "pink")

# boxplot of year built
boxplot(housing$year, main = "Boxplot of Year Built", ylab = "Year", col = "lightgreen")

# boxplot of number of bedrooms
boxplot(housing$beds, main = "Boxplot of Number of Bedrooms", ylab = "Number of Bedrooms", col = "yellow", ylim = c(min(housing$beds) - 1, max(housing$beds) + 1))

# Create a bar plot of housing types
par(mar = c(7, 4, 4, 2) + 0.1) 
barplot(table(housing$type), main = "Bar Plot of Property Types", xlab = "Property Type", col = "orange", cex.names = 0.6)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.4. Two-variable visuals

# Scatterplot of sqft vs. soldprice
plot(housing$sqft, housing$soldprice, xlab = "Square Footage", ylab = "Sold Price", main = "Scatterplot of Sqft vs. Sold Price", col = "blue", pch = 20)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##======================================================================================================

## 2. School Data

## 2.1. Data summary, oddities, and outliers

# read schools file into R
school <- read.csv("schools.csv", header=T)
head(school)

# Data summary, oddities, and outliers
summary(school)

# cannot see immidiate outliers for ratiing from the summary - the data looks mostly clean and we don't want
# to remove whole lines from this data as we will lose a school and its information

# it is possible there are outliers in size

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.2. Data cleaning

# changing school to factor

school$school <- as.factor(school$school)

# size outliers

summary(school$size)

# Calculate IQR
size_iqr <- IQR(school$size)

# Define the lower and upper bounds
lower_bound <- quantile(school$size)[2] - 1.5 * size_iqr
upper_bound <- quantile(school$size)[4] + 1.5 * size_iqr

# Identify outliers
outliers <- school$size < lower_bound | school$size > upper_bound

# View a subset of lines with outliers
outliers_subset <- school[outliers, ]

# View the subset
print(outliers_subset)

# as the upper bound is 1106.25, I will not remove these outliers as they are not exteme outliers

summary(school)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.3. One-variable visuals

# Boxplot of school sizes
boxplot(school$size, main = "Boxplot of School Sizes", xlab = "School Size", col = "lightblue")

# Create a histogram of rating
hist(school$rating, main = "Histogram of Rating", xlab = "Rating", ylab = "Count", col = "coral")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.4. Two-variable visuals

# Scatter plot of school size vs. rating with red color and plus signs
plot(jitter(school$size), jitter(school$rating), 
     xlab = "School Size", ylab = "School Rating", 
     main = "Jittered Scatterplot of School Size vs. Rating", col = "red", pch = "+")


##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##======================================================================================================

## 3. Merged Data

## 3.1. Data cleaning and summary

# Left merge on the "school" variable
merged_data <- merge(housing, school, by.x = "high", by.y = "school", all.x = TRUE)

# Check the merged dataset
head(merged_data)

summary(merged_data)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 3.2. Two-variable visuals

# Jittered scatter plot of rating vs. soldprice with a different color
plot(jitter(merged_data$rating), jitter(merged_data$soldprice), 
     xlab = "School Rating", ylab = "Sold Price", 
     main = "Jittered Scatterplot of Rating vs. Sold Price", col = "darkblue")

# high density plot
sp(merged_data$soldprice, merged_data$rating, jitter = list(x=2,y=2))

sunflowerplot(merged_data$soldprice, merged_data$rating)
smoothScatter(merged_data$rating, merged_data$soldprice)

# High density plot with custom x and y-axis labels
sp(merged_data$soldprice, merged_data$rating, jitter = list(x = 2, y = 2), main = "High-Density plot of Scool Rating vs Sold Price", xlab = "Sold Price", ylab = "School Rating")

# Sunflower plot with custom x and y-axis labels
sunflowerplot(merged_data$soldprice, merged_data$rating, xlab = "Sold Price", ylab = "School Rating")

# Smooth scatter plot with custom x and y-axis labels
smoothScatter(merged_data$rating, merged_data$soldprice, main = "High-Density plot of Scool Rating vs Sold Price", xlab = "School Rating", ylab = "Sold Price")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3.3. Analysis

#checking for correlation of variables in the merged data set
summary(merged_data)

correlation_matrix <- cor(merged_data [, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "size", "rating")])
correlation_matrix

# Subset the merged dataset
subset_merged_data <- merged_data[, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "size", "rating")]

# View the subsetted dataset
head(subset_merged_data)

# correlation matrix
cormat <- cor(subset_merged_data)

# correlation plot
corrplot(cormat, method = "color", type = "lower")

#there is a high correlation between beds and baths, sqft and bed, sqft and beds, lotsize and sqft
# and soldprice and rating

#the correlation between soldprice and rating is the most interesting to me

# Linear regression model of sold price and rating
model <- lm(soldprice ~ rating, data = merged_data)

# Summary of the regression model
summary(model)

# Scatter plot of rating vs. soldprice with regression line
plot(merged_data$rating, merged_data$soldprice, 
     xlab = "School Rating", ylab = "Sold Price", 
     main = "Scatterplot with Regression Line: Rating vs. Sold Price")

# Add regression line to the plot
abline(model, col = "red")

# Add text to the graph
text(6.2, 1100000, labels = "y = 67862X + 846956", col = "blue")

# checking diagnostics
# Set up a 2x2 grid layout
layout(matrix(1:4, ncol = 2))

# Generate diagnostic plots
plot(model)

# Reset the layout to the default
layout(1)

#clustering

# Select relevant columns for clustering
clustering_data_properties <- merged_data[, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "rating")]

# Standardize the data
scaled_data_properties <- scale(clustering_data_properties)

# K-means clustering with 3 clusters (you can adjust the number of clusters)
set.seed(314)  # For reproducibility
kmeans_result_properties <- kmeans(scaled_data_properties, centers = 3)

# Add cluster information to the dataset
merged_data$cluster_properties <- as.factor(kmeans_result_properties$cluster)

# Scatter plot with clusters colored
ggplot(merged_data, aes(x = sqft, y = soldprice, color = cluster_properties)) +
  geom_point(alpha = 0.7) +
  labs(x = "Square Footage", y = "Sold Price", title = "Property Clustering: Sqft vs. Sold Price") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal()

##======================================================================================================

# 4. Sensitivity analysis

# read housing file into R
housing2 <- read.csv("housing.csv", header=T)
head(housing2)

# Data summary, oddities, and outliers
summary(housing2)

# Changing characters to factors 
housing2$neighborhood <- as.factor(housing2$neighborhood)
housing2$type <- as.factor(housing2$type)
housing2$levels <- as.factor(housing2$levels)
housing2$cooling <- as.factor(housing2$cooling)
housing2$heating <- as.factor(housing2$heating)
housing2$fireplace <- as.factor(housing2$fireplace)
housing2$elementary <- as.factor(housing2$elementary)
housing2$middle <- as.factor(housing2$middle)
housing2$high <- as.factor(housing2$high)

# town house and townhouse are the same - need to be renamed
# condo and condominium are the same - need to be renamed

# Checking the current levels to update them
levels(housing$type)

# Replace levels
levels(housing2$type)[levels(housing2$type) == "condominium"] <- "condo"
levels(housing2$type)[levels(housing2$type) == "town house"] <- "townhouse"

#using deduction to replace empties in factors

# Using deduction to replace empties in factors

summary(housing2)

#levels:
summary(housing2$levels)

#there are most 1 level houses so we will make the '?'= 1

# Convert 'levels' to character
housing2$levels <- as.character(housing2$levels)

# Replace '?' with '1'
housing2$levels[housing2$levels == '?'] <- '1'

# Convert 'levels' to factor
housing2$levels <- as.factor(housing2$levels)

summary(housing2$levels)


#cooling
#there are most No cooling houses so we will make the empties = No

#covert to a character
housing2$cooling <- as.character(housing2$cooling)

# Replace empty cells with 'No' in the 'cooling' column
housing2$cooling[housing2$cooling == ''] <- 'No'

# Convert 'cooling' to factor
housing2$cooling <- as.factor(housing2$cooling)

# Summary of the updated 'cooling' column
summary(housing2$cooling)

#heating
#there are most No heating houses so we will make the empties = No

#covert to a character
housing2$heating <- as.character(housing2$heating)

# Replace empty cells with 'No' in the 'heating' column
housing2$heating[housing2$heating == ''] <- 'No'

# Convert to factor
housing2$heating <- as.factor(housing2$heating)

# Summary of the updated 'heating' column
summary(housing2$heating)

#fireplaces
#there are mostly No fireplaces houses so we will make the empties = No

#covert to a character
housing2$fireplace <- as.character(housing2$fireplace)

# Replace empty cells with 'No' in the 'fireplace' column
housing2$fireplace[housing2$fireplace == ''] <- 'No'

# Convert to factor
housing2$fireplace <- as.factor(housing2$fireplace)

# Summary of the updated 'fireplace' column
summary(housing2$fireplace)

# imputing NA's in number variables

summary(housing2)

# fill in missing values in the total_bedrooms column with the median
#there are extreme outliers so I will use median so the mean does not skew the data

# Impute missing values in sqft and lotsize using median imputation

housing22dat <- housing2
housing22dat$sqft[is.na(housing22dat$sqft)] <- median(housing22dat$sqft, na.rm = T)
housing22dat$lotsize[is.na(housing22dat$lotsize)] <- median(housing22dat$lotsize, na.rm = T)
summary(housing22dat)

housing2 <- housing22dat

summary(housing2)
# checking for and removing outliers

# BATHS outliers
# Calculate IQR
baths_iqr <- IQR(housing2$baths)

# Define the lower and upper bounds
lower_bound <- quantile(housing2$baths)[2] - 1.5 * baths_iqr
upper_bound <- quantile(housing2$baths)[4] + 1.5 * baths_iqr

# Identify outliers
outliers <- housing2$baths < lower_bound | housing2$baths > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing2[outliers, ]

# View the subset
print(outliers_subset)

boxplot(housing2$baths)

# upper bound = 4.75, therefore I will round up to 5 and keep all outliers = 5
# for outlier "25" value, i will update it to the mean of the  baths variable. This is 2.001 so I 
# will round to 2

# Replace 'baths' values equal to 25 with 2
housing2$baths[housing2$baths == 25] <- 2

summary(housing2$baths)

summary(housing2)

# BEDS outliers
# Calculate IQR
beds_iqr <- IQR(housing2$beds)

# Define the lower and upper bounds
lower_bound <- quantile(housing2$baths)[2] - 1.5 * beds_iqr
upper_bound <- quantile(housing2$baths)[4] + 1.5 * beds_iqr

# Identify outliers
outliers <- housing2$beds < lower_bound | housing2$beds > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing2[outliers, ]

# View the subset
print(outliers_subset)

summary(housing2$beds)

# 5 and 6 bedrooms are possible so I will updating the extreme outlier of 999. I will make it the median of the dataset = 4

# Replace 'baths' values equal to 999 with 4
housing2$beds[housing2$beds == 999] <- 4

summary(housing2$beds)

# year outliers
# Calculate IQR
year_iqr <- IQR(housing2$year)

# Define the lower and upper bounds
lower_bound <- quantile(housing2$year)[2] - 1.5 * year_iqr
upper_bound <- quantile(housing2$year)[4] + 1.5 * year_iqr

tail(housing2$year)
# Identify outliers
outliers <- housing2$year < lower_bound | housing2$year > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing2[outliers, ]

# View the subset
outliers_subset <- housing2[outliers, ]
outliers_subset

# Subset data where 'neighborhood' is equal to 'Orange'
subset_orange <- housing2[housing2$neighborhood == 'Orange', ]

# View the subset of data
summary(subset_orange)

# Subset data where 'neighborhood' is equal to 'Silver'
subset_silver <- housing2[housing2$neighborhood == 'Silver', ]

# View the subset of data
summary(subset_silver)

# for 2111 - I will make it the median of the orange neigbourhood
# for 1498 - I will make it the median of the silver neigbourhood

housing2$year[housing2$year == 2111] <- 1980
housing2$year[housing2$year == 1495] <- 1959  

boxplot(housing2$year)

# soldprice outliers
# Calculate IQR
soldprice_iqr <- IQR(housing2$soldprice)

# Define the lower and upper bounds
lower_bound <- quantile(housing2$soldprice)[2] - 1.5 * soldprice_iqr
upper_bound <- quantile(housing2$soldprice)[4] + 1.5 * soldprice_iqr

# Identify outliers
outliers <- housing2$soldprice < lower_bound | housing2$soldprice > upper_bound

# View a subset of lines with outliers
outliers_subset <- housing2[outliers, ]

# View the subset
print(outliers_subset)

# I chose to change the the soldprice outlier as it was an extreme outlier to the median
summary(housing2$soldprice)

housing2$soldprice[housing2$soldprice == 664] <- 1244858 

summary(housing2$soldprice)

# re bring in school data, changes made when cleaning were converting school to a factor

# read schools file into R
school2 <- read.csv("schools.csv", header=T)
head(school2)

# changing school to factor
school2$school <- as.factor(school2$school)

# re-merging all cleaned data

## 3.1. Data cleaning and summary

# Left merge on the "school" variable
merged_data2 <- merge(housing2, school2, by.x = "high", by.y = "school", all.x = TRUE)

# Check the merged dataset
head(merged_data2)

summary(merged_data2)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Re-running the analysis

# Analysis

#checking for correlation of variables in the merged data set
summary(merged_data2)

correlation_matrix2 <- cor(merged_data2 [, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "size", "rating")])
correlation_matrix2

# Subset the merged dataset
subset_merged_data2 <- merged_data2[, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "size", "rating")]

# View the subsetted dataset
head(subset_merged_data2)

# correlation matrix
cormat2 <- cor(subset_merged_data2)

# correlation plot
corrplot(cormat, method = "color", type = "lower")

#there is a high correlation between beds and baths, sqft and bed, sqft and beds, lotsize and sqft
# and soldprice and rating

#the correlation between soldprice and rating is the most interesting to me

# Linear regression model of sold price and rating
model2 <- lm(soldprice ~ rating, data = merged_data2)

# Summary of the regression model
summary(model2)

# Scatter plot of rating vs. soldprice with regression line

# Set the main title font size to be smaller
par(cex.main = 0.9)

plot(merged_data2$rating, merged_data2$soldprice, 
     xlab = "School Rating", ylab = "Sold Price", 
     main = "Sensitivity Analysis: Scatterplot with Regression Line: Rating vs. Sold Price")

# Add regression line to the plot
abline(model2, col = "red")

# Add text to the graph
text(6.2, 1100000, labels = "y = 68263X + 842098", col = "blue")


# checking diagnostics
# Set up a 2x2 grid layout
layout(matrix(1:4, ncol = 2))

# Generate diagnostic plots
plot(model)

# Reset the layout to the default
layout(1)

#clustering

# Select relevant columns for clustering
clustering_data_properties2 <- merged_data2[, c("beds", "baths", "sqft", "lotsize", "year", "soldprice", "rating")]

# Standardize the data
scaled_data_properties2 <- scale(clustering_data_properties2)

# K-means clustering with 3 clusters
set.seed(314)  # For reproducibility
kmeans_result_properties2 <- kmeans(scaled_data_properties2, centers = 3)

# Add cluster information to the dataset
merged_data2$cluster_properties2 <- as.factor(kmeans_result_properties2$cluster)

# Scatter plot with clusters colored
ggplot(merged_data2, aes(x = sqft, y = soldprice, color = cluster_properties2)) +
  geom_point(alpha = 0.7) +
  labs(x = "Square Footage", y = "Sold Price", title = "Sensitivity Analysis: Property Clustering: Sqft vs. Sold Price") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal()

