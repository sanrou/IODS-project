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

newGiiNames <- c("giiRank", "country", "giiIndex", "motherMortality", "adolBirthsR", "womenParliament", "femaleEducated", "maleEducated", "femaleLabour", "maleLabour")

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


################ Week 2 of data wrangling.

## Change GNI variable to numeric.
# access the stringr package
library(stringr)

# look at the structure of the gross national income column in 'human'
str(human$grossIncome)

# remove the commas from GNI and print out a numeric version of it
human$grossIncome <- str_replace(human$grossIncome, pattern=",", replace ="") %>% as.numeric


## Exclude unneeded variables. Keep equivalents to "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F"

# List columns in human
colnames(human)

keepVariables <- c("country", "eduFemDivM", "labFemDivM", "educExpect", "lifeExpect", "grossIncome", "motherMortality", "birthsRate", "womenParliament")

human <- select(human, one_of(keepVariables))


## Remove countries with missing values
# filter out all rows with NA values
human <- filter(human, complete.cases(human))


## Remove region observations at the end of the dataframe
# look at the last 10 observations of human
tail(human, n = 10)

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human <- human[1:last, ]


## Define rownames as the country names. One should end up with 155 observations and 8 variables.
# add countries as rownames
rownames(human) <- human$country

# Remove the country variable
human <- select(human, -country)

# Save as .csv file
write.csv(human, file = "data/human.csv")

# Code for loading the file
# human <- read.csv("data/human.csv", header = TRUE, row.names = 1)
