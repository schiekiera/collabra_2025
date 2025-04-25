# ============================================================================== #
#  SCRIPT 1: Analysis Script for Meta-Review of Positive Results in Clinical Psychology
#  Description: This script performs a descriptive data analysis of datasets of clinical psychology papers
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
library(viridis)
library(cld2)


## exclude "exclude" from df_full

# individuals
## Description: dataframe with informations on the n = 1,151 individuals
df_ind <-
  tibble(read_excel(
    paste0(
      path,
      "00_Anonymized_Data/Anonymized_Psych_Individuals.xlsx"
    )
  ))

# df_groups: research groups
## Description: dataframe with informations on the n = 99 research groups
df_groups <-
  tibble(read_excel(
    paste0(
      path,
      "00_Anonymized_Data/Anonymized_Psych_Articles_per_Group.xlsx"
    )
  ))


# df_corpus: full corpus
## Description: dataframe with informations on the n = 2,280 studies in the full corpus
df_corpus <-
  tibble(read_excel(
    paste0(
      path,
      "00_Anonymized_Data/Anonymized_Psych_Full_Corpus_save.xlsx"
    )
  ))

# df_sample: sampled studies
## Description: dataframe with informations on the n = 354 sampled studies
df_sample <-
  tibble(read_excel(
    paste0(
      path,
      "00_Anonymized_Data/Anonymized_Psych_Results_Extraction_Final_save.xlsx"
    )
  ))


# included: included studies
##  description =   Dataframe with informations on the n = 300 sampled and included studies
##  procedure   =   Exclude studies with the "exclude"-tag from df_sample$support
##  explanation =   The "exclude"-tag indicates studies which do not report a hypothesis or are not quantitative 
included <- df_sample[df_sample$support != "exclude",]



#########################
##                     ##
## INDIVIDUALS: df_ind ##
##                     ##
#########################

# report summary statistics for paper count and qualification level
report(df_ind$paper_count)
report(df_ind$qualification_level)


################################
##                            ##
## RESEARCH GROUPS: df_groups ##
##                            ##
################################

# report summary statistics for number of researchers, number of publications, and publication strength
report(df_groups$nr_researchers)
report(df_groups$group_paper_count)
report(df_groups$publication_strength)
report(factor(df_groups$quartile))

# report summary statistics for publication strength
report(df_groups[df_groups$quartile == 1, ]$publication_strength)
report(df_groups[df_groups$quartile == 2, ]$publication_strength)
report(df_groups[df_groups$quartile == 3, ]$publication_strength)
report(df_groups[df_groups$quartile == 4, ]$publication_strength)

# report summary statistics for the group paper count (number of papers published in a research group)
report(df_groups[df_groups$quartile == 1, ]$group_paper_count)
report(df_groups[df_groups$quartile == 2, ]$group_paper_count)
report(df_groups[df_groups$quartile == 3, ]$group_paper_count)
report(df_groups[df_groups$quartile == 4, ]$group_paper_count)

# report summary statistics for number of researchers in a research group
report(df_groups[df_groups$quartile == 1, ]$nr_researchers)
report(df_groups[df_groups$quartile == 2, ]$nr_researchers)
report(df_groups[df_groups$quartile == 3, ]$nr_researchers)
report(df_groups[df_groups$quartile == 4, ]$nr_researchers)



# ggplot
## Graphical Represention of number of researchers per research group
## as a function of the number of publications per research group 
### x-axis: number of researchers per research group
### y-axis: number of publications per research group
### Parameter "shape" based on "quartile"
### Parameter "color" based on "publication_strength"


# set colors
color <- c(rgb(0.9, 0, 0),
           rgb(0.85, 0.5, 0.5),
           rgb(0.6, 0.5, 0.5),
           rgb(0.5, 0, 0))

## ggplot
plot_groups <- ggplot(
  df_groups,
  aes(
    x = nr_researchers,
    y = group_paper_count,
    color = publication_strength,
    # Color based on publication strength
    shape = factor(quartile)       # Shape based on quartile
  )
) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "viridis") +
  scale_shape_manual(values = c(15, 5, 1, 17),
                     labels = c("Q1", "Q2", "Q3", "Q4")) +
  labs(
    x = "Number of researchers per research group",
    y = "Number of publications per research group",
    color = "Publication strength",
    shape = "Quartiles of publication strength"
  ) + 
  theme_minimal() +
  theme(legend.position = "right")  +
  guides(shape = guide_legend(override.aes = list(size = 3)))

# Print the plot
plot_groups



############################
##                        ##
## FULL CORPUS: df_corpus ##
##                        ##
############################

# report summary statistics for year, journal, and quartile
report(as.numeric(df_corpus$year))
# compute mode of year
as.numeric(names(sort(table(df_corpus$year), decreasing = TRUE)[1]))
median(df_corpus$year)
round(table(df_corpus$year)/nrow(df_corpus)*100,0)

# report(df_corpus$journal) --> anonymized
report(factor(df_corpus$quartile))

##################################################################
# This code section is not executable as the data is anonymized ##
##################################################################
# check language of corpus
# "df_corpus$language <- detect_language(df_corpus$title)" --> anonymized
# if title returns NA, use abstract
# "df_corpus[is.na(df_corpus$language), ]$language <-
#  detect_language(df_corpus[is.na(df_corpus$language), ]$abstract)"
### report language of corpus
# "table(df_corpus$language)"
# "report(df_corpus$language)"


################################
##                            ##
## INCLUDED STUDIES: included ##
##                            ##
################################

# report summary statistics for support, rater, and interrater agreement
table(included$rater)
table(included$interrater_agreement)

interrater <- included[included$rater == "JD/LS", ]
interrater$agreement <-
  interrater$support_orig_LS == interrater$support_orig_JD

agreement <- sum(interrater$agreement) / nrow(interrater)

# function for computing cohen's kappa
k <- function(agreement, expected) {
  (agreement - expected) / (1 - expected)
}
k(agreement, 0.5)



################################
##                            ##
## SAMPLED STUDIES: df_sample ##
##                            ##
################################

## report number of coding rounds, exclusion reasons, and quartile
report(factor(df_sample$quartile))
sort(table(df_sample[df_sample$support == "exclude", ]$exclusion_reason), decreasing = TRUE)
sum(table(df_sample[df_sample$support == "exclude", ]$exclusion_reason))
report(factor(df_sample$coding_round))






