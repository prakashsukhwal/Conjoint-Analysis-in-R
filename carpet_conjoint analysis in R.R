# Background: 

# A Carpet Cleaner wants to market a new service, 
# hence it would like to examine the influence of 5 key factors on consumer preference (details provided below). 
# The goal of performing the conjoint analysis is to develop a model for customer preference based on these five factors

# All 10 respondents rated the ten profiles by providing a score between 1 and 10.
pacman::p_load(tidyverse, readxl, radiant)

setwd("D:/work/courses/2023/SPMF/additional")
carpt = read_excel('Carpet_pref_transformed_v1.xls')

# we need one respondent per row
# all atributes as factors
carpt = carpt %>% 
  rename("profile" = "card") %>% # rename card to profile
  mutate(profile = factor(profile), 
         respondent = factor(respondent),  # factorize identifiers
         package = factor(package), 
         brand = factor(brand), price = factor(price), 
         seal = factor(seal),
         money = factor(money)) # factorize the ice cream attributes

# Goal: We use conjoint analysis is to estimate the extent to which each attribute level affects the rating. 
# To do this, the carpet service could create

3*3*3*2*2 # 108 different combinations for people to rate

# this is not easy to run in a field experiment so we need a representative sample
# How we go from a full factorial (108) to a fractional design (> 108 combinations)

# The doe (design of experiments) function from the radiant package will help us decide on study designs. 
# Radiant is an R package for business analytics. 

# Now, to use doe we need to input the information 
# about our attributes and their levels in a specific way
# column-name; attribute_lvl1; attribute_lvl2; etc.

# attribute1, attribute2, etc. are vectors with one element in which we first provide the name of the attribute followed by a semi-colon and then provide all the levels of the attributes separated by semi-colons
attribute1 = "package; A*; B*; C*"
attribute2 = "brand; K2R; Glory; Bissell"
attribute3 = "price; 1.19; 1.39; 1.59"
attribute4 = "seal; No; Yes" # Good House Keeping Seal
attribute5 = "money; No; Yes" # Money back guarantee

# now combine these different attributes
attributes = c(attribute1, attribute2, 
               attribute3, attribute4, 
               attribute5)
summary(doe(attributes, seed = 2000000))
# Seed: fix random number generator, see explanation below

# For each experimental design, 
# it shows the D-efficiency of the design — a measure of how cleanly we will be able to estimate the effects of interest after running the experiment (higher scores are better) — 
# and whether or not the design is balanced — whether each level is included in the same number of trials or profiles. 
# Ideally, we’re looking for balanced designs with a high D-efficiency (above 0.80 is considered reasonable). 
# We see two candidates, an experimental design with 108 profiles, 
# which is just the full factorial design,or a design with 18 profiles.
# Let’s have a look at the design with 18 profiles:
summary(doe(attributes, seed = 2000000, trials = 18))
# Under Partial factorial design (or fractional factorial design), 
# we find the profiles to run in an experiment with 18 instead of 108 profiles.
# Note: There will always be some correlations in fractional factorial designs. 
# It means that some combinations of attribute levels will be more prevalent than others. 
# Only in a full factorial design will all attributes be uncorrelated or orthogonal.


# The Conjoint regression results are simply the results of a multiple linear regression:
str(carpt)
respondent1 = carpt %>% 
              filter(respondent == '1')

# save the conjoint analysis in an object, 
# because we'll use it as input to summary(), plot(), and predict() later on
conjoint_respondent1 = conjoint(respondent1, 
                                rvar = "rating", 
                                evar = c("package","brand","price","seal", "money")) 

summary(conjoint_respondent1)

# The part-worths and the regression coefficients give the same information:
# compared to the reference level (the first level of an attribute; you’ll see that the part-worths are always zero for this level), 
# how much does each attribute level increase or decrease the rating of service?
# We can plot these results:
plot(conjoint_respondent1)

# Predicting the ratings (utilities) of the different services is easy in R.
# First, let’s make sure we have a dataset with the different profiles that were tested:
profiles = carpt %>% 
  filter(respondent == "1") %>% 
  select(package,brand,price,seal, money)

profiles
# Then, we ask the predict function to 
# predict the ratings of the profiles based on the regression function
predict(conjoint_respondent1, profiles)
# predict the ratings for the profiles based on the conjoint analysis

# BUT it’s more interesting to get predictions for service
# that the respondent has not rated.
# For this, we need the profiles for all possible combinations. 
# We can create these profiles with the expand.grid function. 
# The expand.grid function takes two or more vectors and creates every possible combination of elements of those vectors:

# Let’s do this for all our attribute levels:

# now create all the profiles
profiles.all = expand.grid(levels(carpt$package),
                           levels(carpt$brand),
                           levels(carpt$price),
                           levels(carpt$seal),
                           levels(carpt$money)) %>% 
                  rename("package" = "Var1", 
                         "brand" = "Var2", 
                         "price" = "Var3", 
                         "seal" = "Var4",
                         "money" = "Var5")
# rename the variables created by expand.grid 
# (don't forget this, otherwise predict won't know where to look for each attribute)

# predict the ratings of all profiles
predict(conjoint_respondent1, profiles.all) %>% 
  arrange(desc(Prediction)) 
# show the ice creams with the highest predicted rating on top

# Now, let’s carry out the conjoint analysis on the full dataset 
# to get an idea of which services the 10 respondents, 
# on average, liked the most and how important each attribute is:


conjoint_allrespondents = conjoint(carpt, 
                                rvar = "rating", 
                                evar = c("package","brand","price","seal", "money")) 
# same as before, but different dataset.

summary(conjoint_allrespondents) 

plot(conjoint_allrespondents)

# Predict Utilities 
# Let’s predict the ratings of all possible services

predict(conjoint_allrespondents, profiles.all) %>% # check previous sections for profiles.all
  arrange(desc(Prediction)) # show the options with the highest predicted rating on top

summary(lm(rating ~ package + brand + price + seal+money, data = carpt))



