## Does Scientific Productivity Increase the Publication of Positive Results?

Anonymized data and analysis code for the metascientific study **"Does Scientific Productivity Increase the Publication of Positive Results?"** by Louis Schiekiera & Helen Niemeyer, published in *Collabra: Psychology* (2025), 11(1), 137035. [DOI: 10.1525/collabra.137035](https://doi.org/10.1525/collabra.137035) · [PDF](https://online.ucpress.edu/collabra/article-pdf/11/1/137035/874484/collabra_2025_11_1_137035.pdf) · [Preregistration](https://osf.io/46fk9/files/osfstorage/65aea1a9b1f2b50389b0e3f3) · [TLDR](https://schiekiera.github.io/blog/2025/scientific-productivity-positive-results/)


### Table of Contents
- [Overview](#overview)
- [How to use the data](#how-to-use-the-data)
- [Abstract](#abstract)
- [Key findings](#key-findings)
- [Citation](#citation)


### Overview
This repository contains anonymized data and analysis code for a metascientific study on the influence of scientific productivity on the prevalence of positive results in clinical psychology. The corpus consists of 2,280 quantitative-empirical publications first-authored by clinical psychology researchers across 99 research groups at 52 German universities (2013–2022), sourced from PubMed and OpenAlex.


### How to use the data
The scripts are prepared so that the data are loaded directly from this GitHub repository — no manual download is required. All confirmatory analyses (the registered hypotheses) are fully reproducible from the public data. Parts of the data used for the exploratory analyses are anonymized because they contain sensitive information (e.g., researcher names).


### Abstract

**Background.** The overrepresentation of positive results in psychology is often attributed in part to publication bias. However, the impact of research-group output on the prevalence of positive results has not yet been investigated. The present study examines whether German clinical psychology research groups with high versus low publication outputs differ in the prevalence of positive outcomes in their publications.

**Methods.** Scientific productivity was defined as the ratio of quantitative-empirical publications to the number of academic staff per chair. We analyzed publications first-authored by clinical psychology researchers at German universities from 2013 to 2022, sourced from PubMed and OpenAlex. After excluding meta-analyses, reviews, and non-empirical studies, 2,280 empirical studies from 99 research groups were identified. We then randomly sampled and coded 300 papers, evenly split between the highest and lowest output quartiles, and tested the first registered hypothesis.

**Results.** There was no statistically significant difference between the highest and lowest output quartiles, with both reporting approximately 90% positive results. Higher group paper counts were not associated with more positive results. Exploratory abstract-level analyses showed no significant differences in positive-result rates between any of the four output quartiles.

**Conclusion.** Our results suggest a general excess of positive results in clinical psychology. Contrary to our hypothesis, German clinical psychology research groups with high and low publication outputs do not differ in the prevalence of positive outcomes in their publications.


### Key findings

- **Q1 (low output)**: 90.00% positive (95% CI: 84.0–94.3%, *n* = 150).
- **Q4 (high output)**: 89.33% positive (95% CI: 83.3–93.8%, *n* = 150).
- **Q1 vs. Q4**: Δ = −0.67%, *p* = .500 (one-sided proportion test).
- **Continuous predictor**: a logistic regression of positive results on raw group paper count was non-significant (OR = 1.00, *p* = .356).
- **Exploratory abstract-level analysis** (1,990 abstracts, all four quartiles): 97.19% of abstracts reported full or partial support; no significant association between productivity quartile and support category, *χ*²(6) = 6.67, *p* = .353.
- **Comparison to prior work**: the observed full-support rate (89.67%) sits between Fanelli (91.5%) and Scheel et al. (96.05%), and likely reflects this study's broader inclusion criteria (all empirical studies, not only those explicitly "testing hypotheses").


### Citation
If you use the data or the code, please cite the paper as follows:

> Schiekiera, L., & Niemeyer, H. (2025). Does scientific productivity increase the publication of positive results? *Collabra: Psychology, 11*(1), 137035. https://doi.org/10.1525/collabra.137035

BibTeX:

```bibtex
@article{schiekiera2025productivity,
  author  = {Schiekiera, Louis and Niemeyer, Helen},
  title   = {Does Scientific Productivity Increase the Publication of Positive Results?},
  journal = {Collabra: Psychology},
  volume  = {11},
  number  = {1},
  pages   = {137035},
  year    = {2025},
  doi     = {10.1525/collabra.137035}
}
```
