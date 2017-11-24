# Temp to be moved to chapter4.Rmd.

# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)

## 2. Load Boston data from MASS package
library(MASS)
data('Boston')

str(Boston)
dim(Boston)

# Boston data has 506 observations and 14 variables. It has information about housing in suburbs of Boston.
summary(Boston)


## 3. Graphical overview of the data.

pairs(Boston)

# Histograms of the variables
Boston %>% 
  gather(key=var_name, value = value) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~var_name, scales = "free_x")

# print the correlation matrix
cor_matrix<-cor(Boston) 
cor_matrix %>% round(digits = 2)

# Create color ramp from dark blue to white to red.
colorVector <- c("blue", "white", "red")

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6, col = colorRampPalette(colorVector)(200))

# The pairs plot shows that much of the data does not look like gaussian normally distributed data. The correlation plot shows that there are many high positive correlations between different variables: like industy and NO2 gas level and tax revenue; property taxes and access to radial highways. There are some strong negative correlations ones also like median value of owner-occupied homes and lower status of the population; and between distances to five Boston employment centres and proportion of owner-occupied units built prior to 1940.



## 4. Standardize the dataset. Create crime rate categorical variable. Split data into training and data parts.
# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# change the object to data frame from matrix type.
boston_scaled <- as.data.frame(boston_scaled)

sd(boston_scaled$crim)
# Now all the variables have zero mean and SD of 1. This is recommended for clustering. 


# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime. The bins have either 127 or 126 elements.
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)


# Select 80 % of the data and 20 % of the data and split them into separate datasets.
# number of rows in the Boston dataset 
n <- nrow(Boston)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create training set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)



## 5. Fit linear discriminant analysis on the training set.

# linear discriminant analysis. The . means all of the variables.
lda.fit <- lda(crime ~ ., data = train)

# print the lda.fit object. Note that the first linear discriminant explains 95 % of the model, and the third one only 1.2 %.
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results. Both lines need to be run at the same time. High class of crime rate looks to cluster pretty well with only a few medium high class elements in it, and only one high class element being at LD1 value of ~0.
plot(lda.fit, col = classes, pch = classes, dimen = 2)
lda.arrows(lda.fit, myscale = 1)




## 6. Predict the classes with the LDA model.

# Saving of the crime variable in a vector was done in step 5. Remove the crime variable from test data not necessary either.
# test <- dplyr::select(test, -crime)

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# The categorization of the low and especially high classes of properties was quite successful. The medium low and medium high classes were only about 60 % correct. There was more confusion about the low crime rate properties compared to the high crime rating properties. Having shifting values depending on different runs of the script is annoing.


## 7. Start again with the Boston dataset. Test what number of clusters to run K-means clustering with would make sense.

# Reload Boston dataset.
data("Boston")

# center and standardize variables
boston_scaled <- scale(Boston)

# change the object to data frame from matrix type.
boston_scaled <- as.data.frame(boston_scaled)

# Calculate the Euclidean distances between observations.
dist_eu <- dist(boston_scaled)

# look at the summary of the distances
summary(dist_eu)


# K-means clustering
km <-kmeans(boston_scaled, centers = 3)

# plot the Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)

# Determening how many clusters to have using within cluster sum of squares (WCSS). Set a seed to make iterations give the same result.
set.seed(123)

# determine the number of clusters using the total within sum of squares (twcss)
k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})

# visualize the results. Two or 3 seems reasonable. For visualization purposes 2 is handier so lets go with that.
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(boston_scaled, centers = 2)

# plot the normalized Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)

# Tried to use ggpairs but that didn't manage to color with the clusters. One does get the correlation values with ggpairs though.
ggpairs(boston_scaled)

# Per capita crime rate by town (crime) is correlated most strongly with accessibility to radial highways (rad, 0.63), full-value property-tax rate (tax, 0.58), and lower status of the population (lstat, 0.46). Rad and tax are very highly correlated (0.91), and rad and tax are both correlated somewhat strongly with lstat (~.45). Since the rad and tax are so highly correlated it is hard to disentangle what encourages crime in the area, good connections or wealth as measured by taxes. I would have imagined that lower status would have been negatively correlated with tax. The tax value is likely not a good indicator of the wealth in the area. I find it a bit hard to guess at what these correlations mean. One interesting value is that having higher proportion of blacks by town has a weak negative correlation with per capita crime rate.




## Super bonus, 3D plots.

model_predictors <- dplyr::select(train, -crime)
# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)
# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)

library(plotly)

# Plot with crime classes as color of the training data.
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color = train$crime)

# Plot with clusters of k-means as color of the training data. 
# First one needs to do k-means with 4 clusters to compare the methods.
km3D <-kmeans(boston_scaled, centers = 4)

plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color = km3D$cluster[ind])

# Comparison of the methods using eye-balling: the high crime rate cluster and third cluster of k-means are rather similar. The k-means clustering seems to work better as far as visuals go: less intermingling. The crime classes are more intermingled even to the point where low and medium high crime rate classes are interspersed. Cluster 1 and low somewhat match, but cluster 2 and 4 and medium high and medium low clusters do not match well.

