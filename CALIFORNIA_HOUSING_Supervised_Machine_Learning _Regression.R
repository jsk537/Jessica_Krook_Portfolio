# 1. Access the Data Set
# 2. EDA and Data Visualization
# 3. Data Transformation
# 4. Create Training and Test Sets
# 5. Supervised Machine Learning - Regression
# 6. Evaluating Model Performance

# 1. Access the Data Set

# setting working directory
getwd()
setwd("/Users/jess/Data Science/Project")

# Load any libraries that may be necessary

# b. 
# read file into R
housing <- read.csv("housing.csv", header=T)

# c. 
# class ocean_proximity as a factor
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

summary(housing$ocean_proximity)

# <1H OCEAN     INLAND     ISLAND   NEAR BAY NEAR OCEAN 
# 9136       6551          5       2290       2658 

# 2. EDA and Data Visualization

# a. 
head(housing)

# longitude latitude housing_median_age total_rooms total_bedrooms population households
# 1   -122.23    37.88                 41         880            129        322        126
# 2   -122.22    37.86                 21        7099           1106       2401       1138
# 3   -122.24    37.85                 52        1467            190        496        177
# 4   -122.25    37.85                 52        1274            235        558        219
# 5   -122.25    37.85                 52        1627            280        565        259
# 6   -122.25    37.85                 52         919            213        413        193
# median_income median_house_value ocean_proximity
# 1        8.3252             452600        NEAR BAY
# 2        8.3014             358500        NEAR BAY
# 3        7.2574             352100        NEAR BAY
# 4        5.6431             341300        NEAR BAY
# 5        3.8462             342200        NEAR BAY
# 6        4.0368             269700        NEAR BAY

tail(housing)

# longitude latitude housing_median_age total_rooms total_bedrooms population households
# 20635   -121.56    39.27                 28        2332            395       1041        344
# 20636   -121.09    39.48                 25        1665            374        845        330
# 20637   -121.21    39.49                 18         697            150        356        114
# 20638   -121.22    39.43                 17        2254            485       1007        433
# 20639   -121.32    39.43                 18        1860            409        741        349
# 20640   -121.24    39.37                 16        2785            616       1387        530
# median_income median_house_value ocean_proximity
# 20635        3.7125             116800          INLAND
# 20636        1.5603              78100          INLAND
# 20637        2.5568              77100          INLAND
# 20638        1.7000              92300          INLAND
# 20639        1.8672              84700          INLAND
# 20640        2.3886              89400          INLAND

# b. 
summary(housing)

# longitude         latitude     housing_median_age  total_rooms    total_bedrooms  
# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0  
# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0  
# Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0  
# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9  
# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0  
# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0  
# NA's   :207     
#    population      households     median_income     median_house_value   ocean_proximity
#  Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
#  1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
#  Median : 1166   Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
#  Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
#  3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
#  Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001    


# Perform a correlation analysis on numeric variables in the data frame.

# c. 

correlation_matrix <- cor(housing[, c("longitude", "latitude", "housing_median_age", "total_rooms", "total_bedrooms", "population", "households", "median_income", "median_house_value")])
correlation_matrix

#                      longitude    latitude   housing_median_age total_rooms  total_bedrooms    population  households     median_income     median_house_value
# longitude            1.00000000 -0.92466443   -0.10819681        0.04456798       NA           0.099773223    0.05531009   -0.015175865        -0.04596662
# latitude            -0.92466443  1.00000000    0.01117267       -0.03609960       NA          -0.108784747 -  0.07103543   -0.079809127        -0.14416028
# housing_median_age  -0.10819681  0.01117267    1.00000000       -0.36126220       NA          -0.296244240   -0.30291601   -0.119033990         0.10562341
# total_rooms          0.04456798 -0.03609960   -0.36126220        1.00000000       NA           0.857125973    0.91848449    0.198049645         0.13415311
# total_bedrooms              NA          NA          NA              NA            1              NA               NA             NA                 NA
# population           0.09977322 -0.10878475   -0.29624424        0.85712597       NA           1.000000000    0.90722227    0.004834346        -0.02464968
# households           0.05531009 -0.07103543   -0.30291601        0.91848449       NA           0.907222266    1.00000000    0.013033052         0.06584265
# median_income       -0.01517587 -0.07980913   -0.11903399        0.19804965       NA           0.004834346    0.01303305    1.000000000         0.68807521
# median_house_value  -0.04596662 -0.14416028    0.10562341        0.13415311       NA.         -0.024649679    0.06584265    0.688075208         1.00000000

library(corrplot)

# Exclude rows with NAs in the specified columns
housing_subset <- na.omit(housing[, c(1:9)])

# correlation matrix
cormat <- cor(housing_subset)

# correlation plot

corrplot(cormat, method = "color", type = "lower")


# d. 
# Create histograms for each numeric variable
par(mfrow = c(3, 3), mar = c(4, 1,3, 1))

hist(housing$longitude, main="Histogram - Longitude", xlab="Longitude", col="skyblue")
hist(housing$latitude, main="Histogram - Latitude", xlab="Latitude", col="purple")
hist(housing$housing_median_age, main="Histogram - Housing Median Age", xlab="Housing Median Age", col="lightgreen")
hist(housing$total_rooms, main="Histogram - Total Rooms", xlab="Total Rooms", col="orchid")
hist(housing$total_bedrooms, main="Histogram - Total Bedrooms", xlab="Total Bedrooms", col="darkgreen")
hist(housing$population, main="Histogram - Population", xlab="Population", col="pink")
hist(housing$households, main="Histogram - Households", xlab="Households", col="yellow")
hist(housing$median_income, main="Histogram - Median Income", xlab="Median Income", col="orange")
hist(housing$median_house_value, main="Histogram - Median House Value", xlab="Median House Value", col="blue")


# e. 
# Produce boxplots for each numeric variable.

par(mfrow = c(3, 3), mar = c(4, 2, 1, 1))

boxplot(housing$longitude, main="Boxplot - Longitude", xlab="Longitude", col="skyblue")
boxplot(housing$latitude, main="Boxplot - Latitude", xlab="Latitude", col="purple")
boxplot(housing$housing_median_age, main="Boxplot - Housing Median Age", xlab="Housing Median Age", col="lightgreen")
boxplot(housing$total_rooms, main="Boxplot - Total Rooms", xlab="Total Rooms", col="orchid")
boxplot(housing$total_bedrooms, main="Boxplot - Total Bedrooms", xlab="Total Bedrooms", col="darkgreen")
boxplot(housing$population, main="Boxplot - Population", xlab="Population", col="pink")
boxplot(housing$households, main="Boxplot - Households", xlab="Households", col="yellow")
boxplot(housing$median_income, main="Boxplot - Median Income", xlab="Median Income", col="orange")
boxplot(housing$median_house_value, main="Boxplot - Median House Value", xlab="Median House Value", col="blue")


# f.
# Boxplots for housing_median_age, median income and median_house_value with respect to ocean_proximity

par(mfrow = c(3, 1), mar = c(4, 1, 2, 1) + 0.1)

boxplot(housing$housing_median_age ~ housing$ocean_proximity, main="Housing Median Age by Ocean Proximity", xlab="Ocean Proximity", ylab="Housing Median Age", col=c("skyblue", "lightgreen", "pink", "orange", "purple"))
boxplot(housing$median_income ~ housing$ocean_proximity, main="Median Income by Ocean Proximity", xlab="Ocean Proximity", ylab="Median Income", col=c("skyblue", "lightgreen", "pink", "orange", "purple"))
boxplot(housing$median_house_value ~ housing$ocean_proximity, main="Median House Value by Ocean Proximity", xlab="Ocean Proximity", ylab="Median House Value", col=c("skyblue", "lightgreen", "pink", "orange", "purple"))

# Reset plotting area for future plots
par(mfrow = c(1, 1)

# 3. Data Transformation

# a.

#install.packages("e1071")
library(e1071)

# fill in missing values in the total_bedrooms column with the median
housing2 <- impute(housing[,1:8], what='median')

class(housing2)
#[1] "matrix" "array"

# Replace original variables with imputed variables
housing$total_bedrooms <- housing2[,5]

# View the summary of the fixed data frame
summary(housing)

# b. 

# Create binary categorical variables for each type of ocean_proximity
ocean_proximity_binary <- model.matrix(~ 0 + factor(housing$ocean_proximity), data = housing)

# Adjust the column names
colnames(ocean_proximity_binary) <- c( "<1H OCEAN", "INLAND", "ISLAND", "NEAR BAY", "NEAR OCEAN")

# Combine the binary variables with the original data frame
housing <- cbind(housing, ocean_proximity_binary)

# Remove the original ocean_proximity variable
housing <- housing[, !names(housing) %in% "ocean_proximity"]

# View the modified data frame
head(housing)

# > head(housing)
# longitude latitude housing_median_age total_rooms total_bedrooms population households median_income
# 1   -122.23    37.88                 41         880            129        322        126        8.3252
# 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014
# 3   -122.24    37.85                 52        1467            190        496        177        7.2574
# 4   -122.25    37.85                 52        1274            235        558        219        5.6431
# 5   -122.25    37.85                 52        1627            280        565        259        3.8462
# 6   -122.25    37.85                 52         919            213        413        193        4.0368
# median_house_value <1H OCEAN INLAND ISLAND NEAR BAY NEAR OCEAN
# 1             452600         0      0      0        1          0
# 2             358500         0      0      0        1          0
# 3             352100         0      0      0        1          0
# 4             341300         0      0      0        1          0
# 5             342200         0      0      0        1          0
# 6             269700         0      0      0        1          0
# > 

# c

# Calculate mean_bedrooms and mean_rooms for each group defined by households
housing$mean_bedrooms <- ave(housing$total_bedrooms, housing$households, FUN = mean)
housing$mean_rooms <- ave(housing$total_rooms, housing$households, FUN = mean)

# View the modified data frame
head(housing)

# > head(housing)
# longitude latitude housing_median_age total_rooms total_bedrooms population households median_income
# 1   -122.23    37.88                 41         880            129        322        126        8.3252
# 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014
# 3   -122.24    37.85                 52        1467            190        496        177        7.2574
# 4   -122.25    37.85                 52        1274            235        558        219        5.6431
# 5   -122.25    37.85                 52        1627            280        565        259        3.8462
# 6   -122.25    37.85                 52         919            213        413        193        4.0368
# median_house_value <1H OCEAN INLAND ISLAND NEAR BAY NEAR OCEAN mean_bedrooms mean_rooms
# 1             452600         0      0      0        1          0      150.2000    730.400
# 2             358500         0      0      0        1          0     1188.0000   6942.000
# 3             352100         0      0      0        1          0      194.5714   1039.714
# 4             341300         0      0      0        1          0      261.1026   1391.256
# 5             342200         0      0      0        1          0      289.6000   1466.825
# 6             269700         0      0      0        1          0      204.1053   1010.000
# > 

# Remove the original total_bedrooms and total_rooms variables
housing <- housing[, !names(housing) %in% c("total_bedrooms", "total_rooms")]

# View the modified data frame
head(housing)

# > head(housing)
# longitude latitude housing_median_age population households median_income median_house_value <1H OCEAN
# 1   -122.23    37.88                 41        322        126        8.3252             452600         0
# 2   -122.22    37.86                 21       2401       1138        8.3014             358500         0
# 3   -122.24    37.85                 52        496        177        7.2574             352100         0
# 4   -122.25    37.85                 52        558        219        5.6431             341300         0
# 5   -122.25    37.85                 52        565        259        3.8462             342200         0
# 6   -122.25    37.85                 52        413        193        4.0368             269700         0
# INLAND ISLAND NEAR BAY NEAR OCEAN mean_bedrooms mean_rooms
# 1      0      0        1          0      150.2000    730.400
# 2      0      0        1          0     1188.0000   6942.000
# 3      0      0        1          0      194.5714   1039.714
# 4      0      0        1          0      261.1026   1391.256
# 5      0      0        1          0      289.6000   1466.825
# 6      0      0        1          0      204.1053   1010.000
# > 
# d

# Perform z-score standardization using R's built-in 
# scale() function.

# Specify the columns for z-score standardization
columns_to_scale <- c(1:6, 13, 14)

# Perform z-score standardization using the scale() function
housing[, columns_to_scale] <- scale(housing[, columns_to_scale])

# rename housing to housing_cleaned
cleaned_housing <- housing

# View the modified data frame
head(cleaned_housing)

# > head(cleaned_housing)
# longitude latitude housing_median_age population households median_income median_house_value <1H OCEAN
# 1 -1.327803 1.052523          0.9821189 -0.9744050 -0.9770092    2.34470896             452600         0
# 2 -1.322812 1.043159         -0.6070042  0.8614180  1.6699206    2.33218146             358500         0
# 3 -1.332794 1.038478          1.8561366 -0.8207575 -0.8436165    1.78265622             352100         0
# 4 -1.337785 1.038478          1.8561366 -0.7660095 -0.7337637    0.93294491             341300         0
# 5 -1.337785 1.038478          1.8561366 -0.7598283 -0.6291419   -0.01288068             342200         0
# 6 -1.337785 1.038478          1.8561366 -0.8940491 -0.8017678    0.08744452             269700         0
# INLAND ISLAND NEAR BAY NEAR OCEAN mean_bedrooms mean_rooms
# 1      0      0        1          0    -0.9382641 -0.9178320
# 2      0      0        1          0     1.5801856  2.0743563
# 3      0      0        1          0    -0.8305871 -0.7688323
# 4      0      0        1          0    -0.6691347 -0.5994910
# 5      0      0        1          0    -0.5999794 -0.5630889
# 6      0      0        1          0    -0.8074512 -0.7831459
# > 

# 4. Create Training and Test Sets

# a
# Create a random sample index for the cleaned_housing data frame

n <- nrow(cleaned_housing) 
ntrain <- round(n*0.7)  
set.seed(314)    

tindex <- sample(n, ntrain)  

# b 
# Create a training of 70% of the rows 
train <- cleaned_housing[tindex,]  

# c 
# Create a of 30% of the rows of the
test <- cleaned_housing[-tindex,]  

# 5. Supervised machine learning - Regression

# Set seed for reproducibility
set.seed(314)

# Separate predictors (train_x) and response variable (train_y) for the training set
train_x <- train[, names(train) != "median_house_value"]
train_y <- train$median_house_value

# Load the randomForest package
library(randomForest)

# Train the random forest model
rf <- randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE)

# Printing a summary of the trained model
print(rf)
# 
# > print(rf)
# 
# Call:
#   randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# Mean of squared residuals: 2502214147
# % Var explained: 81.42
# > 

# See the different metrics computed by the algorithm.
names(rf)

# > names(rf)
# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
# [6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
# [11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
# [16] "test"            "inbag"          
# > 

# 6. Evaluating Model Performance

# a.
# Calculating RMSE for the last tree
last_tree_mse <- rf$mse[length(rf$mse)]
rmse <- sqrt(last_tree_mse)

# Printing the RMSE
print(rmse)

# > print(rmse)
# [1] 50022.14
# > 

# b. 
# create a test set to test the model

# Separate predictors (test_x) and response variable (test_y) for the test set
test_x <- test[, names(test) != "median_house_value"]
test_y <- test$median_house_value

# Train the random forest model on the test set
rf_test_model <- randomForest(x = test_x, y = test_y, ntree = 500, importance = TRUE)

# Print the metrics computed by the algorithm
names(rf_test_model)

# > names(rf_test_model)
# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
# [6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
# [11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
# [16] "test"            "inbag"          
# > 

# Predicting median house values on the test set
predicted_values <- predict(rf_test_model, type="class")

# c.
# Calculating RMSE for the test set
rmse_test <- sqrt(mean((predicted_values - test_y)^2))
               
                                  
# d. 
# Comparing training and test set RMSE
print(rmse)
print(rmse_test)

difference <- rmse_test - rmse
print(difference)

# >  print(rmse)
# [1] 50022.14
# > print(rmse_test)
# [1] 51684.38
# > print(difference)
# [1] 1622.246

# e.

# Variable Importance Plot

var_importance_plot <- varImpPlot(rf_test_model)

# Trying to create a better model:

# make new test models
 
# Extracting importance scores
importance_scores <- var_importance_plot[, "%IncMSE"]
importance_scores

# > importance_scores
# longitude           latitude housing_median_age         population         households 
# 51.63645           47.12905           61.34041           48.33079           31.10172 
# median_income          <1H OCEAN             INLAND             ISLAND           NEAR BAY 
# 133.79909           19.94565           43.07180            0.00000           12.23045 
# NEAR OCEAN      mean_bedrooms         mean_rooms 
# 14.47054           31.80760           29.48459 
# > 

# Selecting features with importance scores above the chosen threshold (40)
important_features <- names(importance_scores[importance_scores > 40])
important_features

# > important_features
# [1] "longitude"          "latitude"           "housing_median_age" "population"        
# [5] "median_income"      "INLAND"            
# > 

# Subsetting the data based on important features
subset_cleaned_housing <- cleaned_housing[, c("longitude", "latitude", "housing_median_age", "population", "median_income", "INLAND",  "median_house_value")]

n <- nrow(subset_cleaned_housing) 
ntrain <- round(n*0.7)  
set.seed(314)    

tindex <- sample(n, ntrain)  

# Create a new training of 70% of the rows 
newtrain <- subset_cleaned_housing[tindex,]  

# c 
# Create a new 30% of the rows of the
newtest <- subset_cleaned_housing[-tindex,]  


# Set seed for reproducibility
set.seed(314)

# Separate predictors (train_x) and response variable (train_y) for the training set
newtrain_x <- newtrain[, names(newtrain) != "median_house_value"]
newtrain_y <- newtrain$median_house_value
 
# Train the random forest model on the training set
rf_model_subset <- randomForest(x = newtrain_x, y = newtrain_y, ntree = 500, importance = TRUE)
 
# Print the metrics computed by the algorithm
names(rf_model_subset)

# > names(rf_model_subset)
# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
# [6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
# [11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
# [16] "test"            "inbag"     

# Calculating RMSE for the last tree
newlast_tree_mse <- rf_model_subset$mse[length(rf_model_subset$mse)]
newrmse <- sqrt(newlast_tree_mse)

# Printing the RMSE
print(newrmse)
 
# Separate predictors (test_x) and response variable (test_y) for the test set
newtest_x <- newtest[, names(newtest) != "median_house_value"]
newtest_y <- newtest$median_house_value

# Train the random forest model on the test set
newrf_test_model <- randomForest(x = newtest_x, y = newtest_y, ntree = 500, importance = TRUE)

# Print the metrics computed by the algorithm
names(newrf_test_model)

# > names(newrf_test_model)
# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
# [6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
# [11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
# [16] "test"            "inbag"          
# > 

# Predicting median house values on the test set
new_predicted_values <- predict(newrf_test_model, type="class")

# c.
# Calculating RMSE for the test set
new_rmse_test <- sqrt(mean((new_predicted_values - test_y)^2))
                  
# Comparing training and test set RMSE
print(newrmse)
print(new_rmse_test)

difference <- new_rmse_test - newrmse
print(difference)

# >  print(rmse)
# [1] 49972.18
# > print(new_rmse_test)
# [1] 50995.03
# > print(difference)
# [1] 1022.851
# > 

# Variable importance plot of new model
var_importance_plot <- varImpPlot(newrf_test_model)
