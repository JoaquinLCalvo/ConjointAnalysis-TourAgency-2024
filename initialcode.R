# Initial code for the conjoint analysis

# Load the packages
library(tidyverse)
library(mlogit)

# Load the data
vacation <- read.csv("CBC_Vacation_data.csv", header = TRUE, sep = ";")

# Check the data
head(vacation)
summary(vacation)

# Number of respondents
length(unique(vacation$resp.id))

# Number of questions per respondent
table(vacation$resp.id) / 4

# Distribution of the response variable
table(vacation$alt[vacation$choice == 1])

# Check the frequencies of attributes
sapply(vacation[, 4:8], table) #it's balanced

# Check the levels of the attributes
sapply(vacation[, 4:8], unique)
# 432 unique combinations of attributes

# Covert to factor
vacation$Price <- as.factor(vacation$Price)
vacation$Duration <- as.factor(vacation$Duration)
vacation$Accommodation <- factor(vacation$Accommodation,
                                 levels = c("budget", "3stars", "5stars"))
vacation$Transport <- as.factor(vacation$Transport)
vacation$Destination <- as.factor(vacation$Destination)

# Create the design matrix
vacation_mlogit <- dfidx(vacation, idx = list(c("ques", "resp.id"), "alt"))

# Estimate model1
model1 <- mlogit(choice ~ Price + Duration + Accommodation +
                   Transport + Destination,
                 data = vacation_mlogit)
summary(model1)

# Fit the model without intercept parameters
model2 <- mlogit(choice ~ Price + Duration + Accommodation +
                   Transport + Destination | -1,
                 data = vacation_mlogit)
summary(model2)

# Likelihood ratio test
lrtest(model2, model1)

# Fit the model without intercept and price as a quantitative variable
model3 <- mlogit(choice ~ as.numeric(as.character(Price)) +
                   Duration + Accommodation + Transport + Destination | -1,
                 data = vacation_mlogit)
summary(model3)
lrtest(model3, model2)
# The null hypothesis for the likelihood ratio test is that
# the simpler model (Model 1) is sufficient,
# and the additional complexity of the more complex model (Model 2)
# does not significantly improve the model fit.
# model 2 is better than model3

# Willingness to pay
coef(model3) / coef(model3)["as.numeric(as.character(Price))"]

# Interpretation
# Duration: On average, an individual is willing to pay up to 1948.53 euros
# more to have a 2-night vacation compared to a 5-night vacation
# and 1312.81 euros more for a 2-night vacation compared to a 10-night vacation.
# Accommodation: Respondents would need about 555.5628 euros in compensation
# to choose a budget accommodation over a 3-star accommodation.
# Similarly, respondents would need 2219.0904 euros in compensation
# to choose a budget accommodation over a 5-star accommodation.
# Transport: Respondents would need approximately 848.05 euros in compensation
# to choose a bus over a plane.
# On average, respondents are willing to pay 386.12 euros more
# to take a bus instead of a train.
# Destination: Respondents would need significant compensation
# (2817.9713 euros) to choose Greece as a destination over Portugal.
# Similarly, respondents would need 1947.5294 euros compensation
# to choose Greece over Spain.
# Less compensation is required to choose Greece over Turkey,
# with a value of 790.8334 euros.

# Summary
# Respondents are willing to pay more for shorter vacations,
# require compensation to choose budget hotels over 3-star or 5-star hotels,
# prefer planes to buses, but are willing to pay more for buses over trains,
# and would need a compensation to choose Greece over Portugal, Spain or Turkey.
