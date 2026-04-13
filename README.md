# Drug-poisoning-classification-code

R code and analytic files for the grouped binomial mixed-effects models and funnel-plot analyses used in the manuscript on interstate variation in official classification of non-homicide drug poisoning deaths.

## Files

- `Table2_Table3_model_analysis.R`  
  Fits the grouped binomial mixed-effects models for the main adjusted heterogeneity analyses (Table 2) and the exploratory structural models adding medicolegal death investigation system type (Table 3). The script derives adjusted odds ratios, 95% confidence intervals, jurisdiction-level variance (τ²), intraclass correlation coefficient (ICC), median odds ratio (MOR), likelihood-ratio results, and late-period structural-model summaries. It also exports manuscript-ready results tables.

- `Table_for_R_model_with_MDI_variables.xlsx`  
  Analytic file used for the mixed-effects models.  
  - Sheet name: `Model Data`

- `Figure2_funnel_plots_main.R`  
  Generates the main-manuscript funnel plots for Figure 2 for 2007–2012, 2013–2018, and 2019–2024.

- `funnel plot data.xlsx`  
  Cleaned input file used for the funnel-plot analyses.  
  - Sheet name: `Funnel_R_Input`

## Software

- R 4.5.3
- readxl
- dplyr
- ggplot2
- ggrepel
- lme4
- writexl
- tibble
- stringr

## Analyses included

### 1. Mixed-effects models (Table 2)

The mixed-effects script fits grouped binomial mixed-effects models with logit link for the conditional probability of suicide certification among non-homicide drug poisoning deaths classified as suicide or undetermined intent.

- **Table 2A**: combined grouped binomial mixed-effects model across all pooled periods, with fixed effects for sex, age group, and study period and a random intercept for jurisdiction.
- **Table 2B**: separate period-specific grouped binomial mixed-effects models for 2007–2012, 2013–2018, and 2019–2024, with fixed effects for sex and age group and a random intercept for jurisdiction. These models summarize adjusted between-jurisdiction heterogeneity using jurisdiction-level variance (τ²), intraclass correlation coefficient (ICC), median odds ratio (MOR), and likelihood-ratio testing.

### 2. Exploratory structural models (Table 3)

The mixed-effects script also fits exploratory structural models that add medicolegal death investigation (MDI) system type to the 2019–2024 grouped binomial mixed-effects model.

- **Primary structural model**: Bureau of Justice Statistics (BJS)-based 3-category coding
- **Sensitivity analyses**:
  - BJS 4-category coding
  - CDC-informed 3-category recode with Texas moved out of the decentralized medical-examiner-only category

Table 3 reports MDI category adjusted odds ratios, 95% confidence intervals, jurisdiction-level variance (τ²), ICC, MOR, percent reduction in τ² relative to the base 2019–2024 model, included strata, and represented jurisdictions.

### 3. Funnel plots (Figure 2 and Supplementary Figure S1)

The funnel-plot script generates period-specific jurisdiction-level funnel plots for 2007–2012, 2013–2018, and 2019–2024. The x-axis is the funnel-plot denominator, defined as suicide plus undetermined-intent drug poisoning deaths, and the y-axis is the conditional suicide proportion, calculated as:

`suicide / (suicide + undetermined)`

The pooled national conditional suicide proportion is shown as the center line, with 95% and 99.8% exact-binomial control limits. The Figure 2 script produces a simplified version for the main manuscript; the supplementary funnel-plot script produces fully labeled versions.

## Notes

- Each R script expects its corresponding input file to be in the same folder as the script unless a full file path is provided in the code.
- The mixed-effects script filters rows using the `Use in model` field in the analytic file and does not impute suppressed counts.
- Observable zero cells are retained.
- The structural-model script uses the same analytic file as Table 2 and therefore does not change the number of usable model rows unless MDI fields are missing.
- The funnel-plot scripts classify jurisdictions outside the 99.8% control limits as outliers.
- In the main Figure 2 funnel plots, jurisdictions outside the 99.8% control limits in all three periods may be highlighted as persistent outliers, depending on the plotting options used in the script.

## Repository purpose

This repository provides the analytic code and input files needed to reproduce the main mixed-effects and funnel-plot analyses from the manuscript using a transparent CDC WONDER-based workflow.
