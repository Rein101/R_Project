# Data: Mall Customer Data
# Step 1: let's import it in .csv format
Mall_Customers <- read.csv("C:/Users/MR MIGWI/Downloads/Mall_Customers.csv", header=TRUE)
View(Mall_Customers)

# Step 2: processing the data

# descriptive analysis to track outliers
library(dplyr)
glimpse(Mall_Customers)
summary(Mall_Customers)
# let's look for irregularities and misformats
names(Mall_Customers) # let's change those variable names to better ones
# rename individual variables
colnames(Mall_Customers) <- c("customer_id", "gender", "age", "annual_income", "spending_score")
colnames(Mall_Customers)
View(Mall_Customers)
# descriptive analysis

library(ggplot2)
h1 <- ggplot(Mall_Customers, aes(age))+geom_histogram()
h2 <- ggplot(Mall_Customers, aes(annual_income))+geom_histogram()
h3 <- ggplot(Mall_Customers, aes(spending_score))+geom_histogram()
library(gridExtra)
grid.arrange(h1,h2,h3, nrow = 1)
# they all seem to be normally distributed

# step 3: Kmeans cluster algorithm; analysis and usupervised model creating

# let's call the stats package
# Load required libraries
library(stats)

# 1: Let's select relevant variables for clustering (income, spending_score)

scaled_mall_income <- scale(Mall_Customers[, c("annual_income", "spending_score")])

#The scale() method can be used to scale the values in both columns so that the scaled values of the columns
#have the same mean and standard deviation. The columns now have the same mean of 0 and standard deviation of 1.'''

#Let us determine the Number of Clusters (k):to determine the optimal 
#number of clusters. Techniques like the elbow method and gap analysis can be used to find the appropriate 
#value of k.'''
# 2: Elbow Method and gap statistics to determine optimal k
withinss_mall <- sapply(1:10, function(k) kmeans(scaled_mall_income, centers = k)$tot.withinss)
library(NbClust)
fviz_nbclust(scaled_mall_income, kmeans, method = "wss")
# gap statistics
library(cluster)
gap_stat <- clusGap(scaled_mall_income, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
# with both methods, the optimal k = 6

# 3: Kmean cluster results
cluster_results <- kmeans(scaled_mall_income, centers = 6, nstart = 25)
# nstart option attempts multiple initial configurations and reports on the best one. For example, adding 
# nstart = 25 will generate 25 initial configurations.'''
str(cluster_results)
# Analyze and Interpret the Clusters: Let us analyze the characteristics of each 
# cluster to identify the target customers.'''

Mall_Customers$cluster <- cluster_results$cluster

# Summarize the characteristics of each cluster
aggregate(Mall_Customers[, c("annual_income", "spending_score")], 
          by = list(Mall_Customers$cluster), 
          FUN = mean)
# Visualization
# Scatter plot of two features with cluster assignments as colors
library(factoextra)
fviz_cluster(cluster_results, data = scaled_mall_income)
print(cluster_results)

# interpretation : cluster 1 and 2 are the groups of interest, high income hence
# high spending score

## For Age and spending score


# 1: Let's select relevant variables for clustering (income, spending_score)

scaled_mall_age <- scale(Mall_Customers[, c("age", "spending_score")])

#The scale() method can be used to scale the values in both columns so that the scaled values of the columns
#have the same mean and standard deviation. The columns now have the same mean of 0 and standard deviation of 1.'''

#Let us determine the Number of Clusters (k):to determine the optimal 
#number of clusters. Techniques like the elbow method and gap analysis can be used to find the appropriate 
#value of k.'''
# 2: Elbow Method and gap statistics to determine optimal k
withinss_mall1 <- sapply(1:10, function(k) kmeans(scaled_mall_age, centers = k)$tot.withinss)
library(NbClust)
fviz_nbclust(scaled_mall_age, kmeans, method = "wss")
# gap statistics
library(cluster)
gap_stat1 <- clusGap(scaled_mall_age, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat1)
# with both methods, the optimal k = 2

# 3: Kmean cluster results
cluster_results1 <- kmeans(scaled_mall_age, centers = 2, nstart = 25)
# nstart option attempts multiple initial configurations and reports on the best one. For example, adding 
# nstart = 25 will generate 25 initial configurations.'''
str(cluster_results1)
# Analyze and Interpret the Clusters: Let us analyze the characteristics of each 
# cluster to identify the target customers.'''

Mall_Customers$cluster <- cluster_results1$cluster

# Summarize the characteristics of each cluster
aggregate(Mall_Customers[, c("age", "spending_score")], 
          by = list(Mall_Customers$cluster), 
          FUN = mean)
# Visualization
# Scatter plot of two features with cluster assignments as colors
library(factoextra)
fviz_cluster(cluster_results1, data = scaled_mall_age)
print(cluster_results1)

# interpretation: the target audience has an average age of 28, the younger the 
# audience the higher the spending score

## Hence the customers that spend a lot of money in the mall have an average of 
## 28 yrs and earn approximately 78,551.72. that is the target audience



