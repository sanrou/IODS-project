# Santeri Rouhinen, 2017.11.08
# Load learning dataset and wrangle it.
# Link with information about the dataset:
# http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt

# Access libraries
library(dplyr)


# read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# lrn14 has 183 observations with 60 variables (questions of learning strategy and self evaluations)


### Collapse questions into guestion classes. Keep gender, age, attitude and points. Keep only those that have done the exam.
# Remove those that didn't take the exam.
lrn14 <- filter(lrn14, Points >0)

# Scale attitude to 1-5 (in data 10-50)
lrn14$attitude <- lrn14$Attitude/10

## Collapse questions into deep, surface and strategic questions.
# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)


## Keep only a subset of columns
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

# Rename headers so that all names are not capitalized. 
colnames(learning2014) <- c("gender","age","attitude", "deep", "stra", "surf", "points")

# Save data. Commented out on purpose
# write.csv(learning2014, file = "data/learning2014.csv")

# Load data after clearing working environment
# rm(list = ls())
# learning2014 <- read.csv("data/learning2014.csv")


