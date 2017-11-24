# Script to load Human development and Gender inequality datasets.
# Santeri Rouhinen, 2017.11.23
# Reference link:  http://hdr.undp.org/en/content/human-development-index-hdi


# Load datasets

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Explore datasets
dim(hd)
dim(gii)

glimpse(hd)
glimpse(gii)

# They have the data named by countries. Check if the countries are in the same order. Same order.
all.equal(hd$Country, gii$Country)

# There are 195 observations (countries) and hd has 8 variables, gii 10 variables. The country variable is the same in both datasets.


# Rename variables.

newHdNames <- c("hdiRank", "country", "hdiIndex", "lifeExpect", "educExpect", "educYears", "grossIncome", "gniMinusHdi")

newGiiNames <- c("giiRank", "country", "giiIndex", "motherMortality", "birthsRate", "womenParliament", "femaleEducated", "maleEducated", "femaleLabour", "maleLabour")

names(hd)  <- newHdNames
names(gii) <- newGiiNames


# New column from old columns. Adds a column.
# define a new column education ratio by dividing female education by male education
library(dplyr)

gii <- mutate(gii, eduFemDivM = femaleEducated / maleEducated)
gii <- mutate(gii, labFemDivM = femaleLabour / maleLabour)


# Join hd and gii by country. Since it is the only common variable it need not be explicitly set.
human <- inner_join(hd, gii)

# human has 195 observations and 19 variables. 
# Save human as .csv file.
write.csv(human, file = "data/human.csv")