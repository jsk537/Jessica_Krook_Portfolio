# Advanced Analytics for Organisational Impact

###############################################################################

## - what is the impact on sales per product 
## - the reliability of the data 
## - any possible relationship(s) in sales between North America,Europe, and 
## global sales

################################################################################

# EDA using R

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
# Import the tidyverse package.
install.packages('tidyverse')
library('tidyverse')

# Determine the working directory.
getwd() 

# Change the current directory.
setwd(dir = '/Users/jess/Documents/LSE/COURSE 3/assignment/Data') 

# Import the sales data set.
sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
view(sales)

head(sales)

# Create a new data frame (sales2) from a subset of the sales data frame.
# Remove unnecessary columns. 

sales2 <- sales[,c('Ranking','Product','Platform','NA_Sales','EU_Sales','Global_Sales')]

# View the data frame.
view(sales2)


# View the descriptive statistics.
summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Creating scatterplot: 
#Relationship between Global Sales and Platform
qplot(Global_Sales, Platform, colour=Platform, data=sales2)


## Warning message: due to tidyverse updating the library, plots will continue to run as normal
## https://www.tidyverse.org/blog/2022/11/ggplot2-3-4-0/ 
## this warning message is ok

#Relationship Between Global Sales and Product
qplot(Global_Sales, Product, colour=Platform, data=sales2)


## 2b) Histograms
# Create histograms.
# First pass the x-variable, then specify the data source. 
qplot(Global_Sales, data=sales2)

qplot(EU_Sales, data=sales2)

qplot(NA_Sales, data=sales2)

# Creating less bins
qplot(Global_Sales, bins=15, data=sales2)

qplot(EU_Sales, bins=15, data=sales2)

qplot(NA_Sales, bins=15, data=sales2)

## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales,
      Platform,
      colour=Platform,
      data=sales2,
      geom='boxplot')

qplot(NA_Sales,
      Platform,
      colour=Platform,
      data=sales2,
      geom='boxplot')

qplot(EU_Sales,
      Platform,
      colour=Platform,
      data=sales2,
      geom='boxplot')

###############################################################################

# 3. Observations and insights

## The most expensive product is for the Wii Platform and is an outlier for Wii and also other products.
## It is difficult to group the products based on platform as most platforms have outliers. 
## This also makes it difficult to predict the prices of products and also predict who will buy products.
## Going forward, we should gather data such a units sold so we can get better insights per product on popularity and price. 
## Both of these will aid in creating meaningful insights for Turtle Games 

###############################################################################
###############################################################################


# Cleaning and manipulating data using R

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales2)

# Check output: Determine the min, max, and mean and median values of Sales in North America.

mean(sales2$NA_Sales)
median(sales2$NA_Sales)

min (sales2$NA_Sales)
max (sales2$NA_Sales)

# Check output: Determine the min, max, and mean values  of Sales in Europe.

mean(sales2$EU_Sales)
median(sales2$EU_Sales)

min (sales2$EU_Sales)
max (sales2$EU_Sales)

# Check output: Determine the min, max, and mean values of Global Sales.

mean(sales2$Global_Sales)
median(sales2$Global_Sales)

min (sales2$Global_Sales)
max (sales2$Global_Sales)

# View the descriptive statistics.
summary(sales2)

# View the descriptive statistics of the Sales columns
summary(sales2$NA_Sales)
summary(sales2$EU_Sales)
summary(sales2$Global_Sales)

## The summaries show that sales for North America are higher than Sales for Europe and account for a larger portion of the total Global Sales.
## We could look at focusing marketing efforts on North American consumers
## If we could link individual customer location data to sales, we can find trends and make more accurate predictions per region

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

# Import tidyverse of dplyr for the group_by function.
library(dplyr)

# Group by sum of multiple columns
agg_tbl <- sales2 %>% group_by(Product) %>% 
  summarise(sum_nasales = sum(NA_Sales),
            sum_eusales= sum(EU_Sales),
            sum_gsales= sum(Global_Sales),
            .groups = 'drop')
agg_tbl

# Convert tibble to df
aggsales <- agg_tbl %>% as.data.frame()
# View the data frame.
aggsales

# Explore the data frame.
summary(aggsales)


## 2b) Determine which plot is the best to compare game sales.

# Import the necessary libraries.
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Create scatterplots.

## Determining the relationship between Global Sales and Product labeling each product and grouping them into colours for each platform
p <- ggplot(sales2,
       mapping=aes(x=Product, y=Global_Sales,col=Platform,label =Product)) +
  geom_point() +
  # Add a title and subtitle.
  labs(title="Relationship between Product and Global Sales")
p + geom_text(check_overlap = TRUE)

## Determining the relationship between North American and Product labeling each product and grouping them into colours for each platform

p2 <- ggplot(sales2,
            mapping=aes(x=Product, y=NA_Sales,col=Platform,label =Product)) +
  geom_point() +
  # Add a title and subtitle.
  labs(title="Relationship between Product and North American Sales")
p2 + geom_text(check_overlap = TRUE)


## Determining the relationship between European Sales and Product labeling each product and grouping them into colours for each platform

p3 <- ggplot(sales2,
             mapping=aes(x=Product, y=EU_Sales,col=Platform,label =Product)) +
  geom_point() +
  # Add a title and subtitle.
  labs(title="Relationship between Product and European Sales")
p3 + geom_text(check_overlap = TRUE)

## We can see that product 107 is the most popular product in all 3 regions.
## The popularity Product 123 in North American Sales pushes it to be popular in Global Sales, even though it is not easily visible in EU sales
## This further shows we should have two strategies for the different regions as the popularity of products is different
## However, product 195 shows up as a popular product in both regions, so we can see there are products that will be popular in both regions
## We should view the most popular products and gather information on the type of products to predict future sales of similar products.

# Create histograms.

#Histogram of Global Sales categorised by Platform
ggplot(sales2, aes(Global_Sales)) +
geom_histogram(aes(fill=Platform),binwidth = 0.5) +
  labs(title="Histogram of Global Sales")

#Histogram of North American Sales categorised by Platform
ggplot(sales2, aes(NA_Sales)) +
geom_histogram(aes(fill=Platform),binwidth = 0.5) +
  labs(title="Histogram of North American Sales")

#Histogram of European Sales categorised by Platform
ggplot(sales2, aes(EU_Sales)) +
  geom_histogram(aes(fill=Platform),binwidth = 0.5) +
  labs(title="Histogram of European Sales") 

#These histograms show most products sit in the same price category with some outliers. 
#From this we can see that Wii products are an outlier in price, they are more expensive in each sales category

# Create boxplots.
# boxplot 
g <- ggplot(sales2, aes(Product, Global_Sales))
g + geom_boxplot()+
  labs(title="Box plot of Global Sales")

g <- ggplot(sales2, aes(Product, NA_Sales))
g + geom_boxplot()+
  labs(title="Box plot of North American Sales")

g <- ggplot(sales2, aes(Product, EU_Sales))
g + geom_boxplot()+
  labs(title="Box plot of European Sales")

#these boxplots show outliers
#None of these graphs show a good picture of Sales, I would rather group by another column as Product is too granular

#Grouping the Data By Platform
# Group by sum of multiple columns
agg_tbl <- sales2 %>% group_by(Platform) %>% 
  summarise(sum_nasales = sum(NA_Sales),
            sum_eusales= sum(EU_Sales),
            sum_gsales= sum(Global_Sales),
            count_product = sum(Product > 0),
            .groups = 'drop')
agg_tbl

# Convert tibble to df
aggsalesp <- agg_tbl %>% as.data.frame()
# View the data frame.
aggsalesp

# Explore the data frame.
summary(aggsalesp)

#Other examples of plots to visualise the data:

# Boxplot of EU sales grouped by platform
g <- ggplot(sales2, aes(Platform, Global_Sales))
g + geom_boxplot()+
  labs(title="Box plot of Global Sales")

#Bar Plot of Total sales grouped by Platform
g <- ggplot(sales2, aes(Platform, col=Platform))
g + geom_bar(aes(fill=Product), width = 1) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Sales by Platform")

## I am ignoring the warning because the output is giving me the separation of platforms I want to read the chart better

#Plotting the relationship between global sales and platform on a product level
p <- ggplot(sales2,
            mapping=aes(x=Global_Sales, y=Platform,col=Platform,label =Product)) +
  geom_point() +
  # Add a title and subtitle.
  labs(title="Relationship between Platform and Global Sales")
p + geom_text(check_overlap = TRUE)

## From analysing these graphs we can see that the X360 Platform accounts for the most products but the sales are not the highest sales by platform, that is Wii.
## We should have a marketing campaign to push X360 products to drive their sales up.
## We should also evaluate if we should sell products from platforms such as PSV and 2600 and their global sales are low for the products and they do not have many products

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
q <- qqnorm(sales2$Global_Sales)
# Add a reference line:
qqline(sales2$Global_Sales, col='red')

qqnorm(sales2$NA_Sales)
# Add a reference line:
qqline(sales2$NA_Sales, col='red')

qqnorm(sales2$EU_Sales)
# Add a reference line:
qqline(sales2$EU_Sales, col='red')

## We can see that all sales have products that stray from the reference line, this shows the data is not normal

## 3b) Perform Shapiro-Wilk test

# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test for Global Sales:

shapiro.test((sales2$Global_Sales))

# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distributed.

# Perform Shapiro-Wilk test for North American Sales:
shapiro.test((sales2$NA_Sales))
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distributed.

# Perform Shapiro-Wilk test for European Sales:
shapiro.test((sales2$EU_Sales))
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distributed.


## 3c) Determine Skewness and Kurtosis

# Skewness of Global Sales.
skewness(sales2$Global_Sales)
# Our output suggests a high positive skewness 

# Skewness of North American Sales.
skewness(sales2$NA_Sales)
# Our output suggests a high positive skewness 

# Skewness of European Sales.
skewness(sales2$EU_Sales)
# Our output suggests a high positive skewness 

#We can see that all sales are highly positively skewed

#Kurtosis of Global_Sales
kurtosis(sales2$Global_Sales)

kurtosis(sales2$NA_Sales)

kurtosis(sales2$EU_Sales)

#The data is highly skewed indicating the data is not normally distributed

## 3d) Determine correlation
# Determining correlation between Global and North American Sales using Pearson's correlation.
cor(sales2$Global_Sales, sales2$NA_Sales)

#an r value of 0.9 indicates a strong positive correlation

# Determining correlation between Global and European Sales using Pearson's correlation.
cor(sales2$Global_Sales, sales2$EU_Sales)

#an r value of 0.9 indicates a strong positive correlation

# Determining correlation between North American and European Sales using Pearson's correlation.
cor(sales2$NA_Sales, sales2$EU_Sales)

#an r value of 0.7 indicates a moderate positive correlation

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

head(sales)

summary(sales)

agg_tblrelyear <- sales %>% group_by(Year, Publisher, Platform, Product) %>% 
  summarise(sum_nasales = sum(NA_Sales),
            sum_eusales= sum(EU_Sales),
            sum_gsales= sum(Global_Sales),
            .groups = 'drop')
agg_tblrelyear

p <- ggplot(agg_tblrelyear,
            mapping=aes(x=Year, y=sum_gsales,col=Platform, label = Product)) +
  geom_point() +
  # Add a title and subtitle.
  labs(title="Relationship between Product and Global Sales")
p + geom_text(check_overlap = TRUE)

## I have chosen this graph to best suit the data set as it shows the sum og global sales of each product relative to the year is was released
## It also categorises the products by the platform they use
## this can show trends of popular games and also indicates what platform they are linked to
## We can use this graph to gauge the popularity of games and if that has to do with their platform
## This graph shows that there are more products in the higher band of sales (20-40) that belong to the Wii patform than the other platforms in this band

# load package and data
library(ggplot2)
data(mpg, package="ggplot2")
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(agg_tblrelyear, aes(Year, sum_gsales))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="loess", se=F) + 
   scale_x_continuous(breaks=seq(0,2020,5)) +
  labs(y="Sales", 
       x="Year", 
       title="Scatterplot with Showing Global Sales of Products over time")

g2 <- ggplot(agg_tblrelyear, aes(Year, sum_nasales))

# Scatterplot
g2 + geom_point() + 
  geom_smooth(method="loess", se=F) + 
  scale_x_continuous(breaks=seq(0,2020,5)) +
  labs(y="Sales", 
       x="Year", 
       title="Scatterplot with Showing North American Sales of Products over time")


g3 <- ggplot(agg_tblrelyear, aes(Year, sum_eusales))

# Scatterplot
g3 + geom_point() + 
  geom_smooth(method="loess", se=F) + 
  scale_x_continuous(breaks=seq(0,2020,5)) +
  labs(y="Sales", 
       x="Year", 
       title="Scatterplot with Showing European Sales of Products over time")

# These all show that games published earlier have higher revenue (this makes sense), However it also
#indicated that between 2005 and 2010 those games published are more popular than games published later than 2010.
#The price of video games with inflation decreases YoY, so this graph shows that games are becomming cheaper  (source: https://www.gamesindustry.biz/are-video-games-really-more-expensive)


# Creating a data set grouped by Publishers

agg_tblrelpub <- sales %>% group_by(Publisher, Platform) %>% 
  summarise(sum_nasales = sum(NA_Sales),
            sum_eusales= sum(EU_Sales),
            sum_gsales= sum(Global_Sales),
            count_product = sum(Product > 0),
            .groups = 'drop')
agg_tblrelpub

library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(agg_tblrelpub, aes(x=Publisher, y=sum_gsales)) + 
  geom_bar(stat="identity", width=.5, fill="blue") + 
  labs(title="Total Global Sales by Publisher") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Draw plot
ggplot(agg_tblrelpub, aes(x=Publisher, y=sum_eusales)) + 
  geom_bar(stat="identity", width=.5, fill="green4") + 
  labs(title="Total North American Sales by Publisher") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# Draw plot
ggplot(agg_tblrelpub, aes(x=Publisher, y=sum_nasales)) + 
  geom_bar(stat="identity", width=.5, fill="pink") + 
  labs(title="Total North American Sales by Publisher") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

## We can see that Namco Bandai Games in the most popular publisher by sales. 
## Next we could look at the platform sales per Publisher or by Product to see if there is a trend of which Publishers make the most popular games

# Find a correlation.

cor(sales$Product, sales$Global_Sales)
cor(sales$Product, sales$NA_Sales)
cor(sales$Product, sales$EU_Sales)

## These calculations show there is a low negative correlation between Products and Sales values

###############################################################################
###############################################################################

#  Recommendations to the business 

###############################################################################

# 1. Load and explore the data
head(sales2)

# Determine a summary of the data frame.
summary(sales2)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# Create a linear regression model of Global and North American Sales.
model1 <- lm(sales2$Global_Sales ~ sales2$NA_Sales)

# View the summary stats.
summary(model1)

# Create a linear regression model of Global and European Sales.
model2 <- lm(sales2$Global_Sales ~ sales2$EU_Sales)

# View the summary stats.
summary(model2)

# Create a linear regression model of North American and European Sales.
model3 <- lm(sales2$NA_Sales ~ sales2$EU_Sales)

# View the summary stats.
summary(model3)

## 2b) Create a plot (simple linear regression)

# Create a visualisation to determine normality of the Global and North American linear regression data set.
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

# Create a visualisation to determine normality of the Global and European linear regression data set.
qqnorm(residuals(model2))
qqline(residuals(model2), col='red')

# Create a visualisation to determine normality of the North American and European linear regression data set.
qqnorm(residuals(model3))
qqline(residuals(model3), col='red')


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
sales3 <- sales[,c('NA_Sales','EU_Sales','Global_Sales')]

head(sales3)

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (wine) and set 
# character size (cex=2).
corPlot(sales3, cex=2)

## High positive correlation between variables 

# Multiple linear regression model.

# Create a new object and 
# specify the lm function and the variables.
modela = lm(NA_Sales~EU_Sales+Global_Sales, data=sales3)

# Print the summary statistics.
summary(modela)

#Find the correlation between the different variables.
round(cor(sales3),2)

## High positive correlation between variables

#load test data
sales3_test <- sales3

# View the data.
str(sales3_test)
#predict
prediction <- predict(modela, newdata = sales3,
                      interval='confidence')
# Print the object.
prediction


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

## NA_Sales EU_Sales Global_Sales
## 1    34.02    23.80        67.85
## 2    23.85     2.94        33.00
## 3    13.00    10.56        29.37
## 4    12.92     9.03        27.06
## 5     9.24     7.29        25.72
## 6    19.02     1.85        24.81


## fit          lwr           upr
## 1   30.6072353107 29.573476421 31.6409942000
## 2   22.1076559849 21.344801666 22.8705103042
## 3   12.9814802009 12.557738086 13.4052223156
## 4   12.5420616523 12.183292632 12.9008306725
## 5   13.0045313687 12.684715777 13.3243469608
## 6   16.9021004225 16.303461616 17.5007392289

## The model is successful at predicting values.  the predict() output is close with the actual values in the str() output.

## The first predicted value is 30.607; the actual Price for the first data point is 34.02.
## The second predicted value is 22.107; the actual Price for the second data point is 23.85 .

###############################################################################
###############################################################################




