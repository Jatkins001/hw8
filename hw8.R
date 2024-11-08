
"In the first part (Lab 7), we will use basic OLS to estimate some models of a 0/1 y-variable"


library(ggplot2)
library(tidyverse)
library(haven)

setwd("/Users/Jermaineatkins/Desktop/R/") 
load("Acs.2021.couples.RData")
summary(acs2021_couples)
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age
acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")

acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college")

acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]

acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric

"Let me fix up a couple of the variables with somewhat mysterious coding"

acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")

acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0",
                                       "Mexican" = "1",
                                       "Puerto Rican" = "2",
                                       "Cuban" = "3",
                                       "Other" = "4")




## With 0/1 y-variable
"I'll look at what factors relate to a partner being older and I'll choose to consider 
traditional pairs, where a man and woman are married and he is placed as householder 
(in the olden days, would be called 'head of household'). I'll create a dummy variable for 
if the man is more than 5 years older than the woman. You can pick a different number than 5!" 

trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )


trad_data$he_more_than_10yrs_than_her <- as.numeric(trad_data$age_diff < -10)


"Note the variable name.

All the math underlying is just concerned with which of the x-variables make 
the y-variable more likely to be a higher number. In this case it's ok, I've set it up for you,
but in general you want to confirm which factor answer is one and which is zero.

For instance,"
table(trad_data$he_more_than_10yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))

"shows that a one corresponds to 'he is older by 5 or more years' and zero corresponds to 'not'. 
But a different person could estimate a model where the dependent variable is 'he is *not* older 
by 5 or more years' and that would have opposite signs for the estimated coefficients! Either model 
could be sensible, as long as you're clear about which one the computer is estimating. Be paranoid 
and check.

You can estimate models something like this (once you figure out what subset of data you'll use)"

#Adding independent variable region as difference in age will be viewed different in different areas across the globe
#can help understand how factors such as income, region, race and age influence the age difference between partners of traditional couples 
model1 <- lm(he_more_than_10yrs_than_her ~ REGION + h_race + AGE, data = trad_data)
summary(model1)

library(car)
#F-TEST
(H0) - "Region has zero impact on age difference"
(H1) - "Region has significance impact"

Anova(model1, type = "II")
"The low Pvalues for all three variables indicate that they are all important factors in predicting the age 
difference between male and female partners."

traddata2 <- trad_data %>%
  select(REGION, h_race, AGE) %>%
  distinct() %>%
  mutate(predicted_prob = predict(model1, newdata = ., type = "response"))

# Create the scatterplot
ggplot(traddata2, aes(x = REGION, y = predicted_prob, color = REGION)) +
  geom_point(size = 3) +
  facet_wrap(~ cut(AGE, breaks = c(0, 30, 40, 50, 60, Inf), labels = c("Under 30", "30-40", "40-50", "50-60", "60+"))) +
  labs(
    title = "Predicted Probability of Having More Than 10 Years by Region",
    x = "Region",
    y = "Predicted Probability",
    color = "Region"
  ) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

"The graph is divided into 6 different groups representing different ages of individuals before becoming a couple.
The different age groups show different patterns in predicted probabilities. the data seems to dip as age 
goes beyond 30yrs. This implies that the impact of region on the outcome variable may be stronger as we age 
This graph also shows that the predicted probabilities somewhat vary among the different regions.
Regions like the pacific division and mountain division have a higher probability of the age difference being 10yrs+.
"




