# Code for use in chapter5

## Load human data into R.
human <- read.csv("data/human.csv", header = TRUE, row.names = 1)

# Look at the data
str(human)
dim(human)

## There are 155 countries in the human dataframe with 8 variables. The variables are:
# eduFemDivM:   Ratio of females to males educated at secondary and higher education levels
# labFemDivM:   Ratio of females to males labour market participation rate (LFPR)
# educExpect:   Expected years of schooling
# lifeExpect:   Life expectancy at birth (years)
# grossIncome:  Gross national income per capita (GNI)
# motherMortality: Maternal mortality ratio (MMR) (deaths per 100 000 live births)
# birthsRate:   Adolescent birth rate (ABR) (births per 1 000 women ages 15-19)
# womenParliament: Share of parliamentary seats held by women (% of seats)
 
