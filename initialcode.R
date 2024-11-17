### Initial code for the conjoint analysis


#### 0- Packages and data loading ####

# Load the packages
library(tidyverse)
library(mlogit)
library(DataExplorer)
library(ggplot2)
library(skimr)
library(dplyr)
library(MASS)

# Load the data
vacation <- read.csv("./CBC_Vacation_data.csv", header = TRUE, sep = ";")
View(vacation)

#### 1- EDA ####

# Check the data
head(vacation)
summary(vacation)
skim(vacation)

# Check for missing values
sum(is.na(vacation))

# Visualize Distributions of Numerical Variables
vacation %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  theme_minimal()
# The variables are not numeric, no point in this plot

# Number of respondents = 250
length(unique(vacation$resp.id))

# Number of questions per respondent = 18
table(vacation$resp.id) / 4

# Distribution of the response variable
table(vacation$alt[vacation$choice == 1]) # A bit left-sweked, right? 
#it's balanced between the 4 options more or less (so no weird behavior)

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

# Visualize Distributions of Categorical Variables
vacation %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_bar(fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white", size = 4) +
  xlab("") +
  ylab("Count") +
  theme_minimal()


# Create the design matrix
vacation_mlogit <- dfidx(vacation, idx = list(c("ques", "resp.id"), "alt"))
vacation_mlogit


#### 2- Modelling: Multinomial logit ####

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
# The null hypothesis for the likelihood ratio test is that the simpler model 
# (Model 1) is sufficient, and the additional complexity of Model 2
# does not significantly improve the model fit.
# Given the extremely low p-value, we reject the null hypothesis and
# We choose model2 over model3

#####################################################################################
# ANALYSIS ON THE FINAL FIXED EFFECTS MODEL (INTERPRETATION OVER POPULATION AVERAGES)
#####################################################################################

# NOTES:
# (Towards a final managerial decision, this analysis should be confronted with 
# the results from the Mixed MNL Model, to arrive to a final conclusion)
# Remember: the following part worth estimates assume "ceteris paribus". So, given 
# that Price does not remain constant among options (e.g. "Plane" costs more than "bus")
# in my opinion we should consider the WTP above the part worths in terms of importance
# towards our final managerial decisions.

# Agree

summary(model2)

# ANALYSIS:
#PRICE: As the price increases, respondents are less attracted to the offers (obvious).
#DURATION: Respondents prefer short 2 nights trips over all, followed by longer 10 days
# trips, the middle option of 5 days is the less liked on average.
#ACCOMODATION: In principle respondents prefer 5 stars over 3 stars and the low-cost hotels
# are in the last position. This is obvious. However, are they willing to pay for this? 
# The final managerial decision should take both into consideration.
#TRANSPORT: Similarly to what we see in accommodation, Plane is the preferred option,
# followed by bus, with train at the last position.
#DESTINATION: The order of preferences, ceteris paribus, would be:
# Portugal --> Spain --> Turkey --> Greece





# Willingness to pay
coef(model3) / coef(model3)["as.numeric(as.character(Price))"]

# Interpretation (PART OF THE INTERPRETATION BELOW WAS DISPUTED ON THE TELEGRAM GROUP - TO BE SOLVED)

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


#NEW (FROM HERE ON, IT'S ONLY JOACO'S CODE AND TEXT)

# Summary
# Given the disparities between the part worths and the WTP tables, it would be 
# fair to assume that we face, on average, a set of respondents with high
# sensitivity towards cost. That is, on abstract they have certain tastes, but when it
# comes the time of paying, their choices change a lot. 

# Defining the function to predict from the MNL
predict.mnl <- function(model, data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model%*%model$coef
  share <- exp(logitUtility)/sum(exp(logitUtility))
  cbind(share, data)
}

# Defining the data frame containing the set of possible designs 
attributes <- list(
  Price = unique(vacation$Price),
  Duration = unique(vacation$Duration),
  Accommodation = unique(vacation$Accommodation),
  Transport = unique(vacation$Transport),
  Destination = unique(vacation$Destination)
)
allDesign <- expand.grid(attributes) 
options(max.print = 3000)
allDesign

# 4 heterogeneous designs for each destination
new.data <- allDesign[c(3, 24, 59, 95,
                        115, 146, 200, 205,
                        231, 256, 279, 298,
                        331, 366, 401, 431), ] 
new.data

predict.mnl(model2, new.data)
# why these vacation packages? how we explain the choice?
# if 2 packages are too similar than shares are wrong


########################################
#### 3- Modelling: Mixed MNL model ####
########################################

model2.rpar <- rep("n", length=length(model2$coef))
names(model2.rpar) <- names(model2$coef)
model2.rpar

# Mixed MNL model with uncorrelated random effects
model2.mixed <- mlogit(choice ~ Price + Duration + Accommodation +
                         Transport + Destination | -1, 
                   data = vacation_mlogit, 
                   panel=TRUE, rpar = model2.rpar, correlation = FALSE)
summary(model2.mixed)

# Visual summary of the distribution of random effects and hence of the level of heterogeneity (ERASE THIS COMMENT IN FINAL VERSION)
plot(model2.mixed)
#TO DO: Analyze this plot and understand why its results are so different from the professor's

#NOTE: Here I leave the code ready to analyze the random effects for each Price variable. 
# We may decide to extend this for all pairs of attribute+level

#1. PRICE 500 (it does not work for the baseline level)
Price500.distr <- rpar(model2.mixed, "Price500")
summary(Price500.distr)
mean(Price500.distr)
med(Price500.distr)
plot(Price500.distr)

#2. PRICE 800
Price800.distr <- rpar(model2.mixed, "Price800")
summary(Price800.distr)
mean(Price800.distr)
med(Price800.distr)
plot(Price800.distr)

#3. PRICE 1500
Price1500.distr <- rpar(model2.mixed, "Price1500")
summary(Price1500.distr)
mean(Price1500.distr)
med(Price1500.distr)
plot(Price1500.distr)

#4. PRICE 2500
Price2500.distr <- rpar(model2.mixed, "Price2500")
summary(Price2500.distr)
mean(Price2500.distr)
med(Price2500.distr)
plot(Price2500.distr)



# Adding correlated random coefficients
model2.mixed2 <- update(model2.mixed, correlation = TRUE)
summary(model2.mixed2)


#NOTE: This should be placed in the final analysis part.
summary(vcov(model2.mixed2, what = "rpar", type = "cor"))

# INTERPRETATION
# 1. There are high correlations between the preferences for the following pairs
# of destinations: Portugal-Spain, Portugal-Turkey, Spain-Turkey. Two possible managerial
# decisions can be extracted from these data points: A. the possibility of offering a trip
# that combines two of these (which would be particularly practical en the case of
# Spain and Portugal). B. Once a client purchases one of those options (say Turkey)
# we should offer him/her another trip to either Spain or Portugal (and repeat the
# same logic for the other correlations between Destinations).
# 2. On standard deviations: A. The only thing we can be absolutely certain about is that 
# there is low variability on people preferring low prices (sd.Price800, low std, high p-value)
# B. ALL the other features contain high variability, confirmed by the low p-values,
# in this context we should: i. Accommodation: split tours between luxury and budget ones, 
# ii. Same with transportation, high variability, iii. Destination: different destinations
# appeal to different types of customers (so here I would decide based on previous Preference Shares).
# 3. The cor.Price1500:Price2500 supports the idea of creating a separate offer for luxury trips
# 4. There is a strong correlation between Transportplane:Transporttrain, could be a good 
# idea to combine these 2 means of transportation on a single tour.

#cor.Price1500:Price2500                      0.018996 *  
#cor.Price2500:Duration10                     0.007341 ** 
#cor.Accommodation5stars:Transporttrain       0.044829 *  
#cor.Transportplane:Transporttrain           7.095e-05 ***
#cor.DestinationPortugal:DestinationSpain    < 2.2e-16 ***
#cor.DestinationPortugal:DestinationTurkey   1.765e-06 ***
#cor.DestinationSpain:DestinationTurkey      < 2.2e-16 ***


# Correlation Matrix
corr_mat <- cov2cor(cov.mlogit(model2.mixed2))
corr_mat

#The professor manually picked the strongly correlated features, but since our matrix 
# is massive, here is a function that does it for us: (DELETE THIS COMMENT IN FINAL VERSION)

# Function to extract strongly correlated features
get_strong_correlations <- function(corr_mat, threshold = 0.7) {
  # Find indices of correlations above threshold or below -threshold
  high_corr_indices <- which(abs(corr_mat) > threshold, arr.ind = TRUE)
  
  # Filter out self-correlations (where row index equals column index)
  high_corr_indices <- high_corr_indices[high_corr_indices[, 1] != high_corr_indices[, 2], ]
  
  # Get the unique feature names from the row and column indices
  features <- unique(c(rownames(corr_mat)[high_corr_indices[, 1]], 
                       colnames(corr_mat)[high_corr_indices[, 2]]))
  
  return(features)
}

strongly_correlated_features <- get_strong_correlations(corr_mat)
print(strongly_correlated_features)

model2.mixed3 <- update(model2.mixed2, correlation = strongly_correlated_features)

#Below, the model with the check mark emoji is the winner
lrtest(model2, model2.mixed) #Fixed effects vs. uncorrelated random effects ✅
lrtest(model2.mixed, model2.mixed2) #Uncorrelated random effects vs. all correlated random effects ✅
lrtest(model2.mixed3, model2.mixed2) #partially correlated random effects vs. all correlated random effects ✅

# Winner: all correlated random effects MNL (model2.mixed2)

################################
### SIMULATING PREFERENCE SHARES
################################


predict.mixed.mnl <- function(model, data, nresp=1000) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}

set.seed(47)
predict.mixed.mnl(model2.mixed2, data=new.data)


#########################
### 4- FINAL ANALYSIS ###
#########################


###################
# Sensitivity Chart
###################

# TO-DO: The current sensitivity chart uses model2. Model2.mixed2 is the best fit, but 
# I couldn't make it work with the mixed effects MNL. If someone were to be able would be great.

sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
base.data <- new.data[1,]
competitor.data <- new.data[-1,]
tradeoff <- sensitivity.mnl(model2, attributes, base.data, competitor.data)

# Define labels with both attribute names and values for clarity
tradeoff$labels <- paste0(rep(names(attributes), sapply(attributes, length)),
                          "\n", tradeoff$level)

# Plot with enhanced readability 
# (perhaps we shouldcrop the variable names towards the final version to improve the visualization)
barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$labels, las=2, 
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.1, 0.11), cex.names=0.7)
grid(nx=NA, ny=NULL)

# INTERPRETATION
#The main insights that we can derive from the Sensitivity Chart are about the 
#Destination. Having Portugal as a Destination would increase the Preference Share
#by around 4%, followed by Spain with an increase of around 2% in the PS. While 
#a vacation tour that were to include a 5-stars accommodation would be preferred 
#by a non-negligible amount of respondents, the previous conclusions on the cost 
#sensitivity of our sample led us to dismiss possible takeaways from that part 
#of the visual.


####################################################################################
### EXTRA: Would be nice to create clusters with consumer segments
### We did not cover this in classes, but IMO would be like the last
### mile of this analytics process. Not that we establish the different
### offers, but a clustering method to do it by itself and we just supervise it.
####################################################################################













########################################################
### ASSESSING THE EFFECTS OF INDIVIDUAL-LEVEL PREDICTORS
########################################################

PW.ind <- fitted(model2.mixed2, type = "parameters")
head(PW.ind)


########################################################################
#TO-DO
#BELOW'S CODE IS JUST COPIED FROM PROFESSOR, WASN'T ADAPTED YET


# We can use merge() to include the individual-level variable "carpool" 
carpool.data <- unique(minivan[,c(1,4)])
names(PW.ind)[1] <- "resp.id"
PW.ind <- merge(PW.ind, carpool.data, by="resp.id")

# Let's focus on the seat8 random effect 
library(lattice)
histogram(~ seat8 | carpool, data=PW.ind)
boxplot(seat8 ~ carpool, data=PW.ind)
by(PW.ind$seat8, PW.ind$carpool, mean)
t.test(seat8 ~ carpool, data=PW.ind) # heterogeneity about preference for 8-seats is at least partially
# significantly explained by carpool
########################################################################










# MANAGERIAL DECISION: 




