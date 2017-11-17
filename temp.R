# Temp to be moved to chapter3.Rmd.

# Load required libraries
library(ggplot2)


# Load data from .csv
alc <- read.csv("data/student-alc.csv")

# Description of the data from the authors (truncated):
# This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. 

# Link to the data and fuller description: https://archive.ics.uci.edu/ml/datasets/Student+Performance

# Column names
colnames(alc)


## Choose four interesting variables and present hypothesis about their relationship with alcohol consumption. I did not select DataCamp ones as the results are known already, except the final grade as that is an obvious one. 
# 19 activities - extra-curricular activities (binary: yes or no) 
# 21 higher - wants to take higher education (binary: yes or no) 
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent) 
# 32 - final grade (numeric: from 0 to 20, output target) 

# 19. I would imagine that activities predicts low alcohol use. Might not, as for example hockey youths in Finland consume a lot of alcohol. Interesting to see if there is any relation to high use. I would guess a marginally significant relation.
# 21. Those with ambition should have less alcohol use if they are serious. Wanting to take higher education though is easy. Again, interesting to see any relation to high use. I would guess a low but still significant relation.
# 24. Having poor family relationship I would imagine predicts high alcohol use. I would guess this to have the strongest relation to high alcohol use of my variable selections. 
# 32. Final grade should have some predictive value, but not very strong. At least as far as I can remember from the DataCamp exercise. This selection is a bit boring.


## Do some exploratory search with the chosen variables and alcohol use. 
# Draw a bar plot of high_use by activities. Gives graphs divided by activities.
g1 <- ggplot(alc, aes(x = high_use))
g1 + geom_bar() + facet_wrap("activities") + ggtitle("Students divided by extra activities (no/yes) to high users (F/T)") + xlab("High alcohol use (False/True)")

# 19 activities. Looks like there is a small effect of students having extra activities having less heavy alcohol users. The difference between active students and non-activity students is not large.


# Draw a bar plot of high education aspirations and high alcohol use.
g2 <- ggplot(alc, aes(x = high_use))
g2 + geom_bar() + facet_wrap("higher") + ggtitle("Students divided by wanting to take higher education (no/yes) to high users (F/T)") + xlab("High alcohol use (False/True)")


# 21. Education. There are very few people not wanting to take higher education, so this variable should be very pointless in future analyses.

alc %>% group_by(higher, high_use) %>% summarise(count = n())

# There are only 18 out of the 382 that are not interested in higher education. A bit surprising. High use was 1:1 in those not wanting to take higher education so at least my hypothesis holds, kind of. The n is so small one should not make statistical inferences. The ratio amongst those wanting to take higher education is about 1:3.5, much less than 1:1.


# 24. Family relationship. 
# Do a histogram of relationship levels by high alcohol use.
g3 <- ggplot(alc, aes(x = high_use))
g3 + geom_bar() + facet_wrap("famrel") + ggtitle("Students divided by family relationships (1-5) to high users (F/T)") + xlab("High alcohol use (False/True)")

# There seems to be more high alcohol users in those reporting poor family relationships. Fortunately those reporting very low relationships are a minority. Hypothesis stands strong.


# 32. Final grade.
# Do boxplots of high and low alcohol users and their final grades (G3).
g4 <- ggplot(alc, aes(x = high_use, y = G3))
g4 + geom_boxplot() + ggtitle("Students' final grades grouped by alcohol use") + xlab("High alcohol use (False/True)") + ylab("Final grade")

# There is a small difference between the high and low users. Unsurpricingly high users of alcohol have worse grades on average than low users. There is large overlap though between the groups.