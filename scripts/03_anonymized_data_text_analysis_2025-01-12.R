# ============================================================================== #
#  SCRIPT 3: Analysis Script for Meta-Review of Positive Results in Clinical Psychology
#  Description: This script performs a brief data anaylsis for a dataset of clinical psychology papers.
# ============================================================================== #

##==============================================================================##
## 1. Setup: load packages and data 
##==============================================================================##

## remove workspace
rm(list = ls())

# Load packages:
library(readxl)
library(tidyverse)
library(report)
library(cld2)


# df_sample: sampled studies
## Description: dataframe with informations on the n = 354 sampled studies
url_sample <- "https://raw.githubusercontent.com/schiekiera/collabra_2025/refs/heads/main/data/Anonymized_Psych_Results_Extraction_Final_save.csv"
df_sample <- read.csv(url_sample)

# included: included studies
##  description =   Dataframe with informations on the n = 300 sampled and included studies
##  procedure   =   Exclude studies with the "exclude"-tag from df_sample$support
##  explanation =   The "exclude"-tag indicates studies which do not report a hypothesis or are not quantitative 
included <- df_sample[df_sample$support != "exclude",]


######################################################################################
# Unfortunately, this script is not executable as all text data is anonymized ##
######################################################################################

# language of abstract
included$language_abstract <- NA
for (i in 1:nrow(included)) {
  included$language_abstract[i] <- detect_language(included$abstract[i])
}
report(included$language_abstract)


# include only papers written in English
included_en <- included[included$language_abstract == "en", ]


##################
### Hypothesis ###
##################

# Combine 'hypothesis_introduction_LS' and 'hypothesis_introduction_JD' columns into 'hypothesis_LS_JD' column
# If 'hypothesis_introduction_LS' is not NA, use it; otherwise, use 'hypothesis_JD'
included_en$hypothesis_LS_JD <- NA
for (i in 1:nrow(included_en)) {
  if (is.na(included_en$hypothesis_introduction_LS[i]) == FALSE) {
    included_en$hypothesis_LS_JD[i] <- included_en$hypothesis_LS[i]
  } else {
    included_en$hypothesis_LS_JD[i] <- included_en$hypothesis_JD[i]
  }
}

# Detect the mention of "hypothes" in 'hypothesis_LS_JD' and create a new binary column 'hypothesis_mentioned'
included_en$hypothesis_mentioned <- NA
for (i in 1:nrow(included_en)) {
  if (str_detect(included_en$hypothesis_LS_JD[i], pattern="hypothes") == TRUE) {
    included_en$hypothesis_mentioned[i] <- TRUE
  } else {
    included_en$hypothesis_mentioned[i] <- FALSE
  }
}
table(included_en$hypothesis_mentioned)

# Detect the specific phrases "test(s/ed) the hypothes" in 'hypothesis_LS_JD' and create a new binary column 'hypothesis_test_mentioned'
included_en$hypothesis_test_mentioned <- NA
for (i in 1:nrow(included_en)) {
  if (str_detect(included_en$hypothesis_LS_JD[i], pattern="test the hypothes|tests the hypothes|tested the hypothes") == TRUE) {
    included_en$hypothesis_test_mentioned[i] <- TRUE
  } else {
    included_en$hypothesis_test_mentioned[i] <- FALSE
  }
}
table(included_en$hypothesis_test_mentioned)


# Detect the words "hypothesize" or "hypothesise" in 'hypothesis_LS_JD' and create a new binary column 'hypothesize'
included_en$hypothesize <- NA
for (i in 1:nrow(included_en)) {
  if (str_detect(included_en$hypothesis_LS_JD[i], pattern="hypothesize|hypothesise") == TRUE) {
    included_en$hypothesize[i] <- TRUE
  } else {
    included_en$hypothesize[i] <- FALSE
  }
}
round(table(included_en$hypothesize)/nrow(included_en)*100,2)


# Detect the word "expect" in 'hypothesis_LS_JD' and create a new binary column 'expect'
included_en$expect <- NA
for (i in 1:nrow(included_en)) {
  if (str_detect(included_en$hypothesis_LS_JD[i], pattern="expect") == TRUE) {
    included_en$expect[i] <- TRUE
  } else {
    included_en$expect[i] <- FALSE
  }
}
table(included_en$expect)


##############
### RESULT ###
##############

# Initialize the 'result_JD' column with NA
included_en$result_JD <- NA

# Combine 'finding_JD' and 'conclusion_JD' into 'result_JD'
for (i in 1:nrow(included_en)) {
  # Correct logical checks and ensure correct grouping of conditions using parentheses
  if (!is.na(included_en$finding_JD[i]) && !is.na(included_en$conclusion_JD[i])) {
    # Join 'finding_JD' and 'conclusion_JD' if both are not NA
    included_en$result_JD[i] <- paste(included_en$finding_JD[i], included_en$conclusion_JD[i], sep = ". ")
  } else if (!is.na(included_en$finding_JD[i]) && is.na(included_en$conclusion_JD[i])) {
    # Use 'finding_JD' if 'conclusion_JD' is NA
    included_en$result_JD[i] <- included_en$finding_JD[i]
  } else if (is.na(included_en$finding_JD[i]) && !is.na(included_en$conclusion_JD[i])) {
    # Use 'conclusion_JD' if 'finding_JD' is NA
    included_en$result_JD[i] <- included_en$conclusion_JD[i]
  } else {
    # Set to NA if both are NA
    included_en$result_JD[i] <- NA
  }
}


# LS
# Initialize the 'result_LS' column with NA
included_en$result_LS <- NA

# Combine 'finding_LS' and 'conclusion_LS' into 'result_LS'
for (i in 1:nrow(included_en)) {
  # Correct logical checks and ensure correct grouping of conditions using parentheses
  if (!is.na(included_en$finding_LS[i]) && !is.na(included_en$conclusion_LS[i])) {
    # Join 'finding_LS' and 'conclusion_LS' if both are not NA
    included_en$result_LS[i] <- paste(included_en$finding_LS[i], included_en$conclusion_LS[i], sep = ". ")
  } else if (!is.na(included_en$finding_LS[i]) && is.na(included_en$conclusion_LS[i])) {
    # Use 'finding_LS' if 'conclusion_LS' is NA
    included_en$result_LS[i] <- included_en$finding_LS[i]
  } else if (is.na(included_en$finding_LS[i]) && !is.na(included_en$conclusion_LS[i])) {
    # Use 'conclusion_LS' if 'finding_LS' is NA
    included_en$result_LS[i] <- included_en$conclusion_LS[i]
  } else {
    # Set to NA if both are NA
    included_en$result_LS[i] <- NA
  }
}

# Combine 'result_LS' and 'result_JD' into 'result_LS_JD'
included_en$result_LS_JD <- NA
for (i in 1:nrow(included_en)) {
  if (is.na(included_en$result_LS[i]) == FALSE) {
    included_en$result_LS_JD[i] <- included_en$result_LS[i]
  } else {
    included_en$result_LS_JD[i] <- included_en$result_JD[i]
  }
}

# Detect the word "significant" in 'result_LS_JD' and create a new binary column 'significant_mentioned'
included_en$significant_mentioned <- NA
for (i in 1:nrow(included_en)) {
  if (str_detect(included_en$result_LS_JD[i], pattern="significant") == TRUE) {
    included_en$significant_mentioned[i] <- TRUE
  } else {
    included_en$significant_mentioned[i] <- FALSE
  }
}

# Significant
# Proportion of "significant" mentioned in the abstract
n.significant.q1 <- length(which(included_en$significant_mentioned == 1 & included_en$quartile == 1))
n.significant.q4 <- length(which(included_en$significant_mentioned == 1 & included_en$quartile == 4))

p.significant<-prop.test(x = c(n.significant.q1, n.significant.q4), 
          n = c(150, 150), alternative = "two.sided")
print(p.significant)

# Hypothesis
n.hypothesis.q1 <- length(which(included_en$hypothesis_mentioned == 1 & included_en$quartile == 1))
n.hypothesis.q4 <- length(which(included_en$hypothesis_mentioned == 1 & included_en$quartile == 4))

p.hypothesis<-prop.test(x = c(n.hypothesis.q1, n.hypothesis.q4), 
          n = c(150, 150), alternative = "two.sided")
print(p.hypothesis)


# Compare support and non-support with hypothesis mentioned
n.hypo.partial_support <- length(which(included_en$support == "no support" & included_en$hypothesis_mentioned == 1))
n.hypo.full_support <- length(which(included_en$support == "support" & included_en$hypothesis_mentioned == 1))
n.partial_support<- length(which(included_en$support == "no support"))
n.full_support<- length(which(included_en$support == "support"))
prop.test(x = c(n.hypo.partial_support, n.hypo.full_support), 
          n = c(n.partial_support, n.full_support), alternative = "two.sided")


# Calculate the number of studies that mention a hypothesis and their support
n.hypo.support <- length(which(included_en$support_binary == 1 & included_en$hypothesis_mentioned == 1))
n.hypo.nosupport <- length(which(included_en$support_binary == 0 & included_en$hypothesis_mentioned == 1))
n.support<- length(which(included_en$support_binary == 1))
n.nosupport<- length(which(included_en$support_binary == 0))
prop.test(x = c(n.hypo.support, n.hypo.nosupport), 
          n = c(n.support, n.nosupport), alternative = "two.sided")


