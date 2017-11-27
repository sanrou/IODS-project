# Script to load Portuguese math and Portuguese language students' information.
# Santeri Rouhinen, 2017.11.17
# Reference link: https://archive.ics.uci.edu/ml/datasets/Student+Performance

# Set working directory. This script is assumed to be in data folder with the student-mat.csv and student-por.csv files. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # works in Rstudio only. This should be data path.
setwd('..')

## Load csvs to data structures
mat <- read.csv("data/student-mat.csv", sep = ";", header = TRUE)
por <- read.csv("data/student-por.csv", sep = ";", header = TRUE)


## Load libraries needed
library(dplyr)



# Explore the data loaded. Both have 33 variables, mat has 395 observations, por 649 observations.
dim(mat)
dim(por)

glimpse(mat)


## Join data sets using attributes to identify who was in both datasets, as IDs are not given. Kind of like internet browser fingerprinting.
joinVariables <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")

math_por <- inner_join(mat, por, by = joinVariables, suffix = c(".mat", ".por"))

# There were 382 students in common. Now there are 53 variables as many columns are duplicated.
dim(math_por)
colnames(math_por)


## Combine duplicated columns so only one remains. Average numerical values, keep first of other values.
# create a new data frame with only the joined columns
alc <- select(math_por, one_of(joinVariables))

# the columns in the datasets which were not used for joining the data. The X %in% Y gives a vector the size of X of T/F is element in X found in Y
notjoined_columns <- colnames(mat)[!colnames(mat) %in% joinVariables]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)



## Create average alcohol consumption column of weekdays and weekends, and a boolean high_use column if use is greater than 2.
alc$alc_use <- (alc$Dalc + alc$Walc)/2
alc$high_use <- alc$alc_use > 2


## Use glimpse to check if data is fine. Note the Dalc, Walc, alc_use, and high_use columns. Looks fine to me. 
# There are 382 observations and 35 variables as there should be.
glimpse(alc)

# Save data to .csv file
write.csv(alc, file = "data/student-alc.csv")

