
# ============================================================================== #
#  SCRIPT 4: Abstract-Level Analysis Script for Meta-Review of Positive Results in Clinical Psychology
#  Description: This script performs abstract-level analysis of the full corpus of studies in the meta-review of positive results in clinical psychology.
# ============================================================================== #


##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

## remove workspace
rm(list = ls())

# Load packages:
library(tidyverse)
library(report)
library(MASS)
library(effects)
library(readxl)
library(reshape2)


################################################
#                                              #
# A B S T R A C T  L E V E L  A N A L Y S I S. #
#                                              #
################################################


# df_corpus: full corpus
## Description: dataframe with informations on the n = 2,280 studies in the full corpus
url_corpus <- "https://raw.githubusercontent.com/schiekiera/collabra_2025/refs/heads/main/data/Anonymized_Psych_Full_Corpus_save.csv"
df_corpus <- read.csv(url_corpus)

# Print the number of rows in the dataframe
print(nrow(df_corpus))

# drop all na from covidence_support_binary and covidence_support
df_corpus <- df_corpus[!is.na(df_corpus$covidence_support_binary),]

# Print the number of rows in the dataframe after exclusion
print(nrow(df_corpus))

####################################################
## 1. D E S C R I P T I V E   S T A T I S T I C S ##
####################################################

## Overall positive results  rate
round(table(df_corpus$covidence_support_binary)/nrow(df_corpus)*100,2)[2]

# Calculate the number of df_corpus studies per quartile
n.q1 <- length(which(df_corpus$quartile == 1))
n.q2 <- length(which(df_corpus$quartile == 2))
n.q3 <- length(which(df_corpus$quartile == 3))
n.q4 <- length(which(df_corpus$quartile == 4))

# Calculate the number of studies
n.support.q1 <- length(which(df_corpus$quartile == 1 & df_corpus$covidence_support_binary == 1))
n.support.q2 <- length(which(df_corpus$quartile == 2 & df_corpus$covidence_support_binary == 1))
n.support.q3 <- length(which(df_corpus$quartile == 3 & df_corpus$covidence_support_binary == 1))
n.support.q4 <- length(which(df_corpus$quartile == 4 & df_corpus$covidence_support_binary == 1))

# Abstract-level positive result rate of studies 
prop.support.q1 <- n.support.q1/n.q1
prop.support.q2 <- n.support.q2/n.q2
prop.support.q3 <- n.support.q3/n.q3
prop.support.q4 <- n.support.q4/n.q4

# Abstract-level: Calculate 95% confidence intervals for the positive result rate
q1.binom <- binom.test(x = n.support.q1, n = n.q1)
q2.binom <- binom.test(x = n.support.q2, n = n.q2)
q3.binom <- binom.test(x = n.support.q3, n = n.q3)
q4.binom <- binom.test(x = n.support.q4, n = n.q4)


# Create a dataframe for summary statistics
summary_table <- data.frame(
  Quartile = c("Q1", "Q2", "Q3", "Q4"),
  n_Support = c(n.support.q1, n.support.q2, n.support.q3, n.support.q4),
  Percentage_Support = round(100*c(prop.support.q1, prop.support.q2, prop.support.q3, prop.support.q4),1),
  CI_Lower = round(100*c(min(q1.binom$conf.int), min(q2.binom$conf.int), min(q3.binom$conf.int), min(q4.binom$conf.int)),1),
  CI_Upper = round(100*c(max(q1.binom$conf.int), max(q2.binom$conf.int), max(q3.binom$conf.int), max(q4.binom$conf.int)),1)
)

# Print the dataframe
print(summary_table)


#############################################
## 2. L O G I S T I C  R E G R E S S I O N ##
#############################################

# logistic regression x = df_corpus$publication_strength y = df_corpus$covidence_support_binary
df_corpus$publication_strength <- as.numeric(df_corpus$publication_strength)
df_corpus$covidence_support_binary <- as.numeric(df_corpus$covidence_support_binary)

# 1. Analysis: logistic regression
model_binary <- glm(covidence_support_binary ~ publication_strength, data = df_corpus, family = binomial(link = "logit"))
summary(model_binary)
report(model_binary)

# 2. Model significance
# Fit the null model (intercept only)
null_model_covidence <- glm(data = df_corpus, covidence_support_binary ~ 1, family = binomial(link = "logit"))

# Perform the likelihood ratio test
lrt_null_model_covidence <- anova(null_model_covidence, model_binary, test = "Chisq")

# Display results
print(lrt_null_model_covidence)

# 3. Odds Ratios and 95% Confidence Intervals
# To calculate Odds Ratios and their 95% Confidence Intervals:
exp_coef_covidence <- exp(coef(model_binary)) # Odds Ratios
conf_int_covidence <- exp(confint(model_binary)) # 95% Confidence Intervals

# Combine results for a clean display
OR_results_covidence <- cbind(Odds_Ratio = exp_coef_covidence, `2.5%` = conf_int_covidence[, 1], `97.5%` = conf_int_covidence[, 2])

# Print results
print(OR_results_covidence)


########################################
## 3. P L O T   T H E   R E S U L T S ##
########################################

# Create a data frame for plotting the main result
abstract_level_plot.df <- data.frame(
  quartile = c("Q1", "Q1", "Q2", "Q2", "Q3", "Q3", "Q4", "Q4"), 
  support = c("supported", "not supported", 
              "supported", "not supported",
              "supported", "not supported", 
              "supported", "not supported"),
  value = c(
    prop.support.q1*100, (1-prop.support.q1)*100, 
    prop.support.q2*100, (1-prop.support.q2)*100, 
    prop.support.q3*100, (1-prop.support.q3)*100, 
    prop.support.q4*100, (1-prop.support.q4)*100
  ),
  lower = c(
    min(q1.binom$conf.int)*100, NA, 
    min(q2.binom$conf.int)*100, NA, 
    min(q3.binom$conf.int)*100, NA, 
    min(q4.binom$conf.int)*100, NA
  ),
  upper = c(
    max(q1.binom$conf.int)*100, NA, 
    max(q2.binom$conf.int)*100, NA, 
    max(q3.binom$conf.int)*100, NA, 
    max(q4.binom$conf.int)*100, NA
  )
)

# Print the dataframe
print(abstract_level_plot.df)

# Reorder the group levels so that Q1 come first, Q4 second
abstract_level_plot.df$quartile <- factor(abstract_level_plot.df$quartile, levels = c("Q1","Q2","Q3", "Q4"))

# Plot the main result
abstract_level_plot <-
  ggplot(abstract_level_plot.df, aes(x = quartile, y = value, fill = support)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  scale_fill_manual(values = c("#86D7B9", "#239169"),
                    name = "result type") +
  annotate("text",
           label = paste0(round(prop.support.q1*100,2), "%"),
           x = 1,
           y = 105) +
  annotate("text",
           label = paste0(round(prop.support.q2*100,2), "%"),
           x = 2,
           y = 105) +
  annotate("text",
           label = paste0(round(prop.support.q3*100,2), "%"),
           x = 3,
           y = 105) +
  annotate("text",
           label = paste0(round(prop.support.q4*100,2), "%"),
           x = 4,
           y = 105) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.05,
                size = 0.5) +
  scale_x_discrete(
    breaks = waiver(),
    labels = c("Q1" = "Q1",
               "Q2" = "Q2",
               "Q3" = "Q3",
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

abstract_level_plot


###################################
# 4. C H I - S Q U A R E  T E S T #
###################################

# Create a contingency table for quartile and support
contingency_table <- table(df_corpus$quartile, df_corpus$covidence_support)

# View the contingency table
print(contingency_table)

# Perform a chi-square test
chi_square_result <- chisq.test(contingency_table)

# Print the result of the chi-square test
print(chi_square_result)

