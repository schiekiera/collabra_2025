# ============================================================================== #
#  SCRIPT 2: Analysis Script for Meta-Review of Positive Results in Clinical Psychology
#  Description: This script performs the main and exploratory results for a dataset of clinical psychology papers
# ============================================================================== #


##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

## remove workspace
rm(list = ls())

## set working directory
path = "path/to/data/"
setwd(path)

# Load packages:
library(readxl)
library(tidyverse)
library(report)
library(FSA)


# df_sample: sampled studies
## dataframe with informations on the n = 354 sampled studies
df_sample <-
  read_excel(
    "00_Anonymized_Data/Anonymized_Psych_Results_Extraction_Final_save.xlsx"
  )

# included: included studies
##  description =   Dataframe with informations on the n = 300 sampled and included studies
##  procedure   =   Exclude studies with the "exclude"-tag from df_sample$support
##  explanation =   The "exclude"-tag indicates studies which do not report a hypothesis or are not quantitative 
included <- df_sample[df_sample$support != "exclude",]
included <- df_sample[df_sample$support != "exclude", ]

## df_ind: dataframe with informations on the n = 1,151 individuals
df_ind <-
  tibble(read_excel(
    paste0(
      path,
      "00_Anonymized_Data/Anonymized_Psych_Individuals.xlsx"
    )
  ))

##==============================================================================##
## 2. Main confirmatory analysis:
##==============================================================================##

###########
##       ##
##  H 1  ##
##       ##
###########

# H1. Primary hypothesis (publication strength): The rate of positive results 
# in low-output research chairs in German clinical psychology are lower than 
# that in high-output research chairs.


## Definition:
  ## low-output research chairs   ==  first quartile of publication strength
  ## high-output research chairs  ==  last quartile of publication strength


## 2.1 Calculate group sizes and positive result rates in both groups

# Calculate the number of included studies from the first quartile of publication strength (= 150)
n.q1 <- length(which(included$quartile == 1))
# Calculate the number of included studies from the last quartile of publication strength (= 150)
n.q4 <- length(which(included$quartile == 4))

# Calculate the number of studies from the first quartile of publication strength with support (= 150)
n.support.q1 <- length(which(included$quartile == 1 & included$support_binary == 1))
# Calculate the number of included studies from the last quartile of publication strength (= 150)
n.support.q4 <- length(which(included$quartile == 4 & included$support_binary == 1))

# Positive result rate of studies from the first quartile of publication strength = .09
prop.support.q1 <- n.support.q1/n.q1
# Positive result rate of studies from the last quartile of publication strength = .89333
prop.support.q4 <- n.support.q4/n.q4



## 2.2 Proportions test to test the following hypothesis:

# The rate of positive results in low-output research chairs will be 
# statistically *lower* than in high-output research chairs

proptestresult <- prop.test(x = c(n.support.q1, n.support.q4), 
                            n = c(n.q1, n.q4), alternative = "less")
print(proptestresult)


# Calculate 95% confidence intervals for the positive result rate
q1.binom <- binom.test(x = n.support.q1, n = n.q1)
q4.binom <- binom.test(x = n.support.q4, n = n.q4)


# Summary statistics
## Summary statistics for Q1: low-output
n.support.q1
prop.support.q1
min(q1.binom$conf.int) # lower end of CI for first quartile of publication strength
max(q1.binom$conf.int) # upper end of CI for first quartile of publication strength
## Summary statistics for Q4: high output
n.support.q4
prop.support.q4
min(q4.binom$conf.int) # lower end of CI for last quartile of publication strength
max(q4.binom$conf.int) # upper end of CI for last quartile of publication strength


#################################
# overall positive results rate #
#################################
round(table(included$support_binary)/nrow(included)*100,2)[2]


## 2.3 Create a plot for the main analysis (Figure 2 in the manuscript)

# Create a data frame to use for plotting the main result
mainplot.df <- data.frame(quartile = c("Q1", "Q1", "Q4", "Q4"), 
                          support = c("supported", "not supported", 
                                      "supported", "not supported"),
                          value = c(prop.support.q1*100, (1-prop.support.q1)*100, 
                                    prop.support.q4*100, (1-prop.support.q4)*100),
                          lower = c(min(q1.binom$conf.int)*100, NA,
                                    min(q4.binom$conf.int)*100, NA),
                          upper = c(max(q1.binom$conf.int)*100, NA,
                                    max(q4.binom$conf.int)*100, NA))

# Reorder the group levels so that Q1 come first, Q4 second
mainplot.df$quartile <- factor(mainplot.df$quartile, levels = c("Q1", "Q4"))

# Plot the main result
mainplot <-
  ggplot(mainplot.df, aes(x = quartile, y = value, fill = support)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_fill_manual(values = c("#86D7B9", "#239169"),
                    name = "result type") +
  annotate("text",
           label = paste("90.00%"),
           x = 1,
           y = 105) +
  annotate("text",
           label = paste("89.33%"),
           x = 2,
           y = 105) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.05,
                size = 0.5) +
  scale_x_discrete(
    breaks = waiver(),
    labels = c("Q1" = "Q1",
               "Q4" = "Q4"),
    name = NULL
  ) +
  scale_y_continuous(
    lim = c(0, 110),
    breaks = c(seq(0, 100, 10)),
    minor_breaks = c(seq(0, 100, 5)),
    name = "% of papers",
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 14),
    legend.margin = margin(0, 0, 0,-5.5)
  ) +
  coord_fixed(ratio = 0.03)

mainplot


##==============================================================================##
## 2.5 Sensitivity analysis for H1:

## Considering that some clinical psychology research groups consist of only one
## person, computing the ratio of quantitative-empirical publications to the number
## of academic staff per chair as an index of publication strength could disadvantage 
## large groups and favor one-person groups. Therefore, in a sensitivity analysis,
## we will test whether there is a difference when we exclude (a) all one-person
## and (b) all one-person and two-person research groups.

# A) # Excluding all one-person research groups
included_a <- included[which(included$nr_researchers > 1), ]
# set parameters
n.q1_a <- length(which(included_a$quartile == 1))
n.q4_a <- length(which(included_a$quartile == 4))

n.support.q1_a <- length(which(included_a$quartile == 1 & included_a$support_binary == 1))
n.support.q4_a <- length(which(included_a$quartile == 4 & included_a$support_binary == 1))

prop.support.q1_a <- n.support.q1_a/n.q1_a
prop.support.q4_a <- n.support.q4_a/n.q4_a

## Proportions test
proptestresult_a <- prop.test(x = c(n.support.q1_a, n.support.q4_a), 
                              n = c(n.q1_a, n.q4_a), alternative = "less")
print(proptestresult_a)


# B) # Excluding all one-person and two-person research groups
included_b <- included[which(included$nr_researchers > 2), ]
# set parameters
n.q1_b <- length(which(included_b$quartile == 1))
n.q4_b <- length(which(included_b$quartile == 4))

n.support.q1_b <- length(which(included_b$quartile == 1 & included_b$support_binary == 1))
n.support.q4_b <- length(which(included_b$quartile == 4 & included_b$support_binary == 1))

prop.support.q1_b <- n.support.q1_b/n.q1_b
prop.support.q4_b <- n.support.q4_b/n.q4_b

## Proportions test
proptestresult_b <- prop.test(x = c(n.support.q1_b, n.support.q4_b), 
                              n = c(n.q1_b, n.q4_b), alternative = "less")
print(proptestresult_b)

## difference
round((proptestresult_a$estimate[2]-proptestresult_a$estimate[1])*100,2)
round((proptestresult_b$estimate[2]-proptestresult_b$estimate[1])*100,2)



##==============================================================================##
## 3. Secondary Hypothesis H2:

###########
##       ##
##  H 2  ##
##       ##
###########

# H2. Secondary hypothesis (group paper count): Higher publication counts of 
# research chairs are associated with higher positive results rates.


## Secondary hypothesis: Furthermore, we test with a 5% alpha level whether higher 
## publication counts of research chairs are associated with higher positive results
## rates using logistic regression. The outcome is binary (support =1; no support = 0)
## and the predictor is metric (nr. of papers). Thus, we hypothesize a positive
## relationship between outcome and predictor. Fanelli (2012) used this method to
## predict the result as a function of publication year. 

# y = support_binary
# x = group_paper_count
# data = included
add_hypo1 <- glm(data = included, support_binary ~ group_paper_count, family = binomial(link = "logit"))

# 1. summary statistics
report(add_hypo1)
summary(add_hypo1)

# 2. Model significance
# Fit the null model (intercept only)
null_model <- glm(data = included, support_binary ~ 1, family = binomial(link = "logit"))

# Perform the likelihood ratio test
lrt <- anova(null_model, add_hypo1, test = "Chisq")

# Display results
print(lrt)

# 3. Odds Ratios and 95% Confidence Intervals
# To calculate Odds Ratios and their 95% Confidence Intervals:
exp_coef <- exp(coef(add_hypo1)) # Odds Ratios
conf_int <- exp(confint(add_hypo1)) # 95% Confidence Intervals

# Combine results for a clean display
OR_results <- cbind(Odds_Ratio = exp_coef, `2.5%` = conf_int[, 1], `97.5%` = conf_int[, 2])

# Print results
print(OR_results)


##==============================================================================##
## 4. Exploratory Analyses

##########################################################
### L O G I S T I C   R E G R E S S I O N   M O D E L S ##
##########################################################

add_hypo2 <- glm(data = included, support_binary ~ group_paper_count + paper_count + as.numeric(year) + nr_researchers, family = binomial(link = "logit"))
# 1. summary statistics
report(add_hypo2)
summary(add_hypo2)

# 2. Model significance
# Fit the null model (intercept only)
# Perform the likelihood ratio test
lrt2 <- anova(null_model, add_hypo2, test = "Chisq")

# Display results
print(lrt2)

# 3. Odds Ratios and 95% Confidence Intervals
# To calculate Odds Ratios and their 95% Confidence Intervals:
exp_coef2 <- exp(coef(add_hypo2)) # Odds Ratios
conf_int2 <- exp(confint(add_hypo2)) # 95% Confidence Intervals

# Combine results for a clean display
OR_results2 <- cbind(Odds_Ratio = exp_coef2, `2.5%` = conf_int2[, 1], `97.5%` = conf_int2[, 2])

# Print results
print(OR_results2)


## print the support rate for each year
df_sample %>%
  group_by(year)  %>%
  summarise(count = mean(support_binary, na.rm = TRUE))

#################################################
## P A R T I A L  O R  F U L L   S U P P O R T ##
#################################################

##==============================================================================##
## Chi-Square Test: Distribution of (full, partial, none) across Q1 vs. Q4
##==============================================================================##

# 1. Subset data to Q1 and Q4 only
df_2x3 <- included %>%
  filter(quartile %in% c(1, 4))

# 2. Create a 2×3 contingency table
#    Rows = Q1 vs. Q4
#    Columns = "support" vs. "partial support" vs. "no support"
table_2x3 <- table(df_2x3$quartile, df_2x3$support)

# 3. Perform chi-square test of independence
chi_result <- chisq.test(table_2x3)

# 4. Print results
table_2x3
chi_result



##==============================================================================##
## 4. Exploratory analysis:
##    Compare the positive result rate of SRs to Fanelli (2010) and Scheel et al. (2021)
##==============================================================================##

# Number of Psychology papers in Fanelli (2010, from Fig. 1)
n.Fanelli <- 141

# Fanelli (2010) reports the positive result rate for Psychiatry&Psychology as
# 91.5% (p. 3). This is likely a rounded number since 141*0.915 = 129.015. For
# a more precise analysis, we assume that 129 out of 141 papers in Fanelli's 
# sample had positive results, giving a positive result rate of 129/141 = 0.9148936.
# In the following, we'll use this more precise number (129/141) in place of the
# 91.5% reported in the paper.
n.support.Fanelli <- 129
n.Fanelli <- 141

## Scheel (2021) reports the positive result rate for Standard Psychology papers
## to be 146 out of 152 SRs , meaning that the positive result rate was
##  96.05% for Standard Psychology papers (95% CI = [91.61, 98.54]; see Fig. 2).

n.support.Scheel <- 146
n.Scheel <- 152

## our sample
n.support.sample<-n.support.q1+n.support.q4
n.sample<-n.q1+n.q4

## proportions
prop.support.Fanelli <- round((n.support.Fanelli/n.Fanelli)*100,2)
prop.support.Scheel <- round((n.support.Scheel/n.Scheel)*100,2)
prop.support.sample <- round((n.support.sample/n.sample)*100,2)

diff.Fanelli <- prop.support.sample - prop.support.Fanelli
diff.Scheel <- prop.support.sample - prop.support.Scheel


## 4.1 Proportions test to test if the positive result rate of Psychology
##     papers in Fanelli (2010) is different from our study (two-sided test)

prop.support.sample
prop.support.Fanelli
diff.Fanelli

Fanelli.prop_test <- prop.test(x = c(n.support.Fanelli, n.support.sample), 
             n = c(n.Fanelli, n.sample), alternative = "two.sided")
print(Fanelli.prop_test)

## 4.2 Proportions test to test if the positive result rate of Standard Psychology
##     papers (nor RR) in Scheel et al (2010) is different from our study (two-sided tes)
prop.support.sample
prop.support.Scheel
diff.Scheel

Scheel.prop_test <- prop.test(x = c(n.support.Scheel, n.support.sample), 
             n = c(n.Scheel, n.sample), alternative = "two.sided")
print(Scheel.prop_test)


## 5. Individual publication output by qualification level
# report summary statistics for paper count by qualification level
report(df_ind[df_ind$qualification_level == "Predoc", ]$paper_count)
report(df_ind[df_ind$qualification_level == "Postdoc", ]$paper_count)
report(df_ind[df_ind$qualification_level == "Professor", ]$paper_count)

# Conduct Kruskal-Wallis Test
kruskal_test <- kruskal.test(paper_count ~ factor(qualification_level), data = df_ind)
kruskal_test

# Load the necessary library

# Perform Dunn's test with Bonferroni adjustment
dunn_test <- dunnTest(paper_count ~ factor(qualification_level), data = df_ind, method = "bonferroni")

# Display results
dunn_test

