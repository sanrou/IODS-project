# Code for use in chapter5

## Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(tidyr)



## Load human data into R.
human <- read.csv("data/human.csv", header = TRUE, row.names = 1)

# Look at the data
str(human)
dim(human)

## 1. 
# There are 155 countries in the human dataframe with 8 variables. The variables are:

# eduFemDivM:   Ratio of females to males educated at secondary and higher education levels
# labFemDivM:   Ratio of females to males labour market participation rate (LFPR)
# educExpect:   Expected years of schooling
# lifeExpect:   Life expectancy at birth (years)
# grossIncome:  Gross national income per capita (GNI)
# motherMortality: Maternal mortality ratio (MMR) (deaths per 100 000 live births)
# adolBirthsR:  Adolescent birth rate (ABR) (births per 1 000 women ages 15-19)
# womenParliament: Share of parliamentary seats held by women (% of seats)

# Further information can be found in http://hdr.undp.org/en/content/human-development-index-hdi


## 2. 
# Graphical overview of the data. Show summaries.

summary(human)

# Histograms of the variables
human %>% 
  gather(key=var_name, value = value) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~var_name, scales = "free_x")

# Most of the data are not normally distributed. Adolescent births rate, GNI, maternal mortality are heavily tailed with most of the values being low. Education expectation, women in parliament and women's labour participation values are roughly normally distributed although with high kurtosis values. Education ratio of females to men and life expectancy values have multiple peaks and have complicated distributions.

# Correlations of the variables.
# Create color ramp from dull dark blue to white to dull red.
colorVector <- c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444")

# Print the correlation matrix
corMatrix<-cor(human) 
corMatrix %>% round(digits = 2)

# Visualize the correlation matrix


corrplot(corMatrix, method = "color", col = colorRampPalette(colorVector)(200),
         type = "upper", order = "hclust", number.cex = .8,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 30, # Text label color and rotation
         # Combine with significance
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

# The highest positive correlations were between education expectancy and life expectancy. GNI, education expectation, and life expectancy all had quite strong positive correlations to each other. Maternal mortality was strongly negatively correlated with life expectancy, education expectancy and ratio of female to male education. The variables women in parliament and ratio of women in labour force had low correlations to other variables.


## 3. Perform PCA on non-standardized human data. Examine the results.

# perform principal component analysis (with the SVD method)
pcaHuman <- prcomp(human)

# print out a summary of PCA. One gets quite a few warnings. The first component explains a whopping 99.99 % of the variance.
s <- summary(pcaHuman)
s

# rounded percetanges of variance captured by each PC
pca_pr <- round(100*s$importance[2, ], digits = 1)

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot of the principal component representation and the original variables using the first 2 components. GNI explains looks to explain pretty much all of the first principal component.
biplot(pcaHuman, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])



## 4. Perform PCA on standardized human data. Examine and compare to the non-standardized data.

humanStand <- scale(human)

pcaHumanStand <- prcomp(humanStand)

# print out a summary of PCA. One gets quite a few warnings.
s2 <- summary(pcaHumanStand)
s2

# rounded percetanges of variance captured by each PC. 
pca_pr2 <- round(100*s2$importance[2, ], digits = 1)

# print out the percentages of variance. Now the components explain the data much more diversly. The first one explains 52 % of the variability, with the next 3 components explaining 43 % of the variability. 
pca_pr2

# create object pc_lab to be used as axis labels
pc_lab2 <- paste0(names(pca_pr2), " (", pca_pr2, "%)")

# draw a biplot of the principal component representation and the original variables using the first 2 components. GNI explains looks to explain pretty much all of the first principal component.
biplot(pcaHumanStand, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab2[1], ylab = pc_lab2[2])

# The results are quite different. Before there was only one component of any note. After scaling there are say 3 or 4 significant components. I would guess most of the differences are due to scaling normalizing the data, which is an expected attribute in most analyses. Re-do the histogram of beginning to check this out.

as.data.frame(humanStand) %>% 
  gather(key=var_name, value = value) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~var_name, scales = "free_x")

# No, not that different actually. One major thing at least is that PCA assumes that large values mean more importance. So before the GNI which had way bigger numbers was given the most importance. This does not make too much sense, as the units are different for different variables.


## 5. One can with standardization more easily see the correlations between different variables. PC1 is composed mostly of educational expectation, GNI, ratio of female to male education, life expectancy and maternal mortality. PC2 is composed mostly of women and parliament and females in labour force ratio. The biplots are certainly easier to read after scaling as the different variables are on similar scales instead of wildly different ones. I would imagine that PC1 is mostly the level or resources put into people, like medicine and schooling. PC2 might be some kind of equality measure that measures how well can women attend the working life instead of being home wives. 


## 6. And now for something completely different.

# Load tea dataset from Factominer.
library(FactoMineR)
data(tea)

# Explore the data. The tea dataset has 300 observations and 36 variables.
str(tea)
dim(tea)

# Since there are so many variables one needs to split them for visualization.
gather(tea[1:12]) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")  + geom_bar() + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

gather(tea[13:24]) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")  + geom_bar() + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

gather(tea[25:36]) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")  + geom_bar() + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))


# Perform multiple correspondence analysis on the tea data. Some of the columns seem to give errors, so keep only a subset of variables.
keep_columns <- c("Tea", "how", "sugar", "where", "lunch", "exciting", "price", "Sport")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA variables
plot(mca, invisible=c("ind"), habillage = "quali")

# Dimensions 1 and 2 of MCA correspond mostly to what package people use their tea in, where they drink and the price of the tea. Dimension 3 corresponds somewhat to what kind of tea is drank, and if they add sugar or not. 

# Visualize MCA individuals
plot(mca, invisible=c("var"), habillage = "quali")

# So many people it's hard to see different numbers in the clouds. If one could zoom dynamically or something would be cool. Anyway one can for example see that in the upper right individual 190 and 208 are quite similar in their tea habits, as they are close in the plot.