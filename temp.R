# Temp to be moved to chapter2.Rmd.

# Read data from a local comma separated values file. 
learning2014 <- read.csv("data/learning2014.csv")

# Get dimensions of the data (166 students and 8 observations). Get structure.
dim(learning2014)
str(learning2014)

# The dataset has been collapsed from a dataset measured by Kimmo Vehkalahti. 
# Deep, stra and surf refer to deep, strategic and surface learning values normalized to 1-5.


# Show a graphical overview of the data and show summaries of the variables in the data. X is the numeric order of the students.
library(ggplot2)
library(GGally)

# Text table of the dataset
summary(learning2014)

# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = attitude, y = points))

# define the visualization type (points)
p2 <- p1 + geom_point()

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title and draw the plot
p4 <- p3 + ggtitle("Student's attitude versus exam points")

# add equation and r-squared to the graph after linear model has been done.


# create a more advanced plot matrix with ggpairs()
pp <- ggpairs(learning2014[-1], mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot
pp

shapiro.test(learning2014$deep)
shapiro.test(learning2014$stra)
shapiro.test(learning2014$surf)
shapiro.test(learning2014$points)


# Description of the data: There are more females than males. Mean age of the students is 25.5 years and ranges from 17-55.
# Mean attitude is 3.1. Strategy attributes look like they are roughly normally distributed. However that is not the case for deep and points.
# The normality test was done with Shapiro-Wilks normality test. If it gives p<0.05 then usually normality is not expected to hold.
# Most of the attributes do not correlate much. Biggest correlations are with points and deep learning strategy with both genders.
# Also with males (not females) surface learning had a strong negative correlation with deep learning strategy.



## Perform an explanatory model with a regression model. Choose three variables. Drop non-significant ones and redo.
# Do a linear model.
lModel <- lm(points ~ attitude + stra + surf, data = learning2014)
summary(lModel)

# Surface learning is not even close to significant. The strategic learning is marginally significant. So drop surf.
lModel <- lm(points ~ attitude + stra, data = learning2014)
summary(lModel)

# After dropping surf strategic learning is also p>0.05 so drop that also. Only attitude remains.
lModel <- lm(points ~ attitude, data = learning2014)
summary(lModel)


# Add regression formula to the attitude vs points plot.
# Function to get regression formula.
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

p5 = p4 + annotate("text", x = 2, y = 32, label = lm_eqn(lModel), colour="black", size = 5, parse=TRUE)
p5



# Linear regression can be described with a formula
# dependent = alpha + beta(explanatoryVariable) + noise
# One can have more than one explanatory variables. 
# In this case only attitude remains as a statistically significant explanatory variable.
# In the current model alpha is 11.6 and beta is 3.5. That is each attitude attribute point is worth 3.5 points in the exam.

# Multiple R-squared value is 0.19. Multiple R-squared is known as R-squared. It tells how well the model fits the data.
# A value of 1 means the model perfectly predicts data, whereas 0 means there is no predictive value. 
# The value of 19 % means that roughly 19 percent of the course score in the learning2014 dataset is explained by the attitude rating.
# It can help to think that R-squared is correlation coefficient x correlation coefficient.
# If one was using multiple variables using the Adjusted R-squared value would be recommended.


## Check if model fits well or not. 
# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5.
# Draw three panels in a single figure.
par(mfrow = c(1,3))
# Plot Residuals vs Fitted values (1), Normal QQ-plot (2), and Residuals vs Leverage (5).
plot(lModel, which = c(1,2,5))

# Linear regression model assumes linearly distributed data and normally distributed errors. Errors should also have constant variance. 
# One can display graphs to test how well the model fits the assumption.
# Residuals vs Fitted plot is used to check if variance is constant or not. If the data points are roughly evenly distributed good, if the data has a clear shape then bad.
# Residuals vs Fitted is roughly evenly distributed, so the size of errors should not depend on the explanatory variables.
# Q-Q plot is used to measure whether errors are normally distributed. If points follow the line then normality can be presumed.
# Q-Q plot seems reasonably near the assumption of normal error distribution.
# Residuals vs Leverage plot is used to check for outlier observations. If there are observations with particularly high leverage one should consider removing them from the analysis.
# Leverage plot seems like there is no outlier observation (so no very high leverage observations).


