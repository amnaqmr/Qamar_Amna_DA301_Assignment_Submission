# WEEK 4

# Determine your working directory
getwd()

# Install the tidyverse library.
install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse)

# Import a CSV file.
data <- read.csv('turtle_sales.csv', header=T)


# Print the data frame.
data
View(data)

# Sense-check the data set
# Return the structure of the data frame.
str(data)

# Check the type of the data frame.
typeof(data)

# Check the class of the data frame.
class(data)

# Check the dimensions of the data frame
dim(data)

# Summarise the data set.
summary(data)


# Create a new subset by dropping redundant columns inclduing Ranking, yEAR, publisher and Genre.  
sales_data <- subset(data, select = c("Product","Platform","NA_Sales","EU_Sales","Global_Sales"))
sales_data
print(sales_data)

# Check dimensions and summarize the new data set. 
dim(sales_data)
summary(sales_data)



# PLOTS:
# Looking at relationship between NA_Sales and Product using qplot. 

# View the first scatterplot.
qplot(Product,
      NA_Sales,
      colour=Platform,
      data=sales_data)

# I've swapped axes and added jitter to make this clearer.
# Jitter will add 'noise' to a numeric vector. I'll have more data points.
qplot(Product,
      NA_Sales,
      colour=Platform, 
      data=sales_data,
      geom=c('point', 'jitter'))

# I can also use a boxplot.
qplot(NA_Sales,
      Platform,
      colour=Product,
      data=sales_data,
      geom='boxplot')

# I can also plot histograms 
str(sales_data)
NA_Sales <- sales_data$NA_Sales
hist(NA_Sales)

# Looking at relationship between EU_Sales and Product using qplot. 

# View the first scatterplot.
qplot(Product,
      EU_Sales,
      colour=Platform,
      data=sales_data)

# I've swapped axes and added jitter to make this clearer.
# Jitter will add 'noise' to a numeric vector. I'll have more data points.
qplot(Product,
      EU_Sales,
      colour=Platform, 
      data=sales_data,
      geom=c('point', 'jitter'))

# I can also use a boxplot.
qplot(EU_Sales,
      Platform,
      colour=Product,
      data=sales_data,
      geom='boxplot')

# I can also plot histograms 
str(sales_data)
EU_Sales <- sales_data$EU_Sales
hist(EU_Sales)


# Looking at relationship between Global_Sales and Product using qplot. 

# View the first scatterplot.
qplot(Product,
      Global_Sales,
      colour=Platform,
      data=sales_data)

# I've swapped axes and added jitter to make this clearer.
# Jitter will add 'noise' to a numeric vector. I'll have more data points.
qplot(Product,
      Global_Sales,
      colour=Platform, 
      data=sales_data,
      geom=c('point', 'jitter'))

# I can also use a boxplot.
qplot(Global_Sales,
      Platform,
      colour=Product,
      data=sales_data,
      geom='boxplot')

# I can also plot histograms 
str(sales_data)
Global_Sales <- sales_data$Global_Sales
hist(Global_Sales)


# I can also plot histograms for Product
str(sales_data)
Product <- sales_data$Product
hist(Product)

#############################################################################

# WEEK 5: 

# View the dataframe created in week 4
sales_data

# Explore and sense-check the data 
view(sales_data)
as_tibble(sales_data)

# min, max and mean of each sales column
min(NA_Sales)
max(NA_Sales)
mean(NA_Sales)

min(EU_Sales)
max(EU_Sales)
mean(EU_Sales)

min(Global_Sales)
max(Global_Sales)
mean(Global_Sales)

# Descriptive Staistics 
head(sales_data)
str(sales_data)

# Group the data based on product in the dataframe using aggregate 
sales_group <- aggregate(Product~EU_Sales+NA_Sales+Global_Sales, sales_data, sum)
print(sales_group)

# Use Groupby to sum columns by product 
df_sales <- sales_group %>%  group_by(Product) %>% summarise(sum_EU_Sales=sum(EU_Sales),sum_NA_Sales=sum(NA_Sales),sum_Global_Sales=sum(Global_Sales),.groups='drop')
view(df_sales)


# Explore the df-sales dataframe with sum by product for sales 
head(df_sales)
str(df_sales)
as_tibble(df_sales)

# Determine which plot is best to plot sales of games 

# EU_Sales
# View the  scatterplot.
qplot(Product,
      sum_EU_Sales,
      data=df_sales)

# I can also use a boxplot.
qplot(sum_EU_Sales,
      Product,
      data=df_sales,
      geom='boxplot')

# I can also plot histograms 
str(df_sales)
EU_Sales <- df_sales$sum_EU_Sales
hist(EU_Sales)

# NA_Sales
# View the  scatterplot.
qplot(Product,
      sum_NA_Sales,
      data=df_sales)

# I can also use a boxplot.
qplot(sum_NA_Sales,
      Product,
      data=df_sales,
      geom='boxplot')

# I can also plot histograms 
str(df_sales)
NA_Sales <- df_sales$sum_NA_Sales
hist(NA_Sales)

# Global_Sales
# View the  scatterplot.
qplot(Product,
      sum_Global_Sales,
      data=df_sales)

# I can also use a boxplot.
qplot(sum_Global_Sales,
      Product,
      data=df_sales,
      geom='boxplot')

# I can also plot histograms 
str(df_sales)
Global_Sales <- df_sales$sum_Global_Sales
hist(Global_Sales)


# Determine Normality of the Data 

# Plot a Q-Q Plot 
qqnorm(df_sales$sum_Global_Sales)

# add a line
qqline(df_sales$sum_Global_Sales, col='red')


# Install and import Moments for the Shapiro-Wilk test
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test
shapiro.test(df_sales$sum_Global_Sales)

# Now we can check for skewness
skewness(df_sales$sum_Global_Sales)
# Our output suggests a positive skewness.

#Check for kurtosis
kurtosis(df_sales$sum_Global_Sales)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic (excess kurtosis is positive)

# Determine correlation

# The best plot to determine the skewness and kurtosis of the sales data is the histgram.
# It shows a heavy-tailed distribution  of the global sales data. 
str(df_sales)
Global_Sales <- df_sales$sum_Global_Sales
hist(Global_Sales)


#############################################################################

# WEEK 6: 

# Visualise the cleaned and filtered dataset from week 5
head(df_sales)
summary(df_sales)


# Explore and summarise the dataset 
plot(df_sales$Product, df_sales$sum_Global_Sales)

# Transform the data set to limit errors.
# Improve linearity of data set and increase R^2
Sqrtdf_sales<- sqrt(df_sales$sum_Global_Sales)

# Visualise the result of transformed data
plot(df_sales$Product, Sqrtdf_sales)


# Create a linear regression between product and global_sale

# Create a linear regression model.
model1 <- lm(df_sales$sum_Global_Sales ~ df_sales$Product)


# View the summary stats.
summary(model1)


# Create a visualisation to determine normality of data set.
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

# Predictions based on my model: 
# p values: 2.2e-16 means it has a mdoerate effect on the sales. 
# R^2: 0.3722 shows a positive, moderatley strong colleration between the product and the number of sales. 









