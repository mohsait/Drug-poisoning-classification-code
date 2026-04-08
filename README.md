# Drug-poisoning-classification-code

R code and analytic files for the grouped binomial mixed-effects model and funnel-plot analyses used in the manuscript on state variation in official classification of non-homicide drug poisoning deaths.

## Files

- `Table2_model_analysis.R`: fits the combined grouped binomial mixed-effects model for Table 2A and the period-specific grouped binomial mixed-effects models for Table 2B; derives adjusted odds ratios, ICC, MOR, and likelihood-ratio results.
- `Table3 model.xlsx`: combined analytic file used for the mixed-effects models.
  - Sheet name: `Model Data`
- `Figure2_funnel_plots_main.R`: generates the main-manuscript funnel plots for Figure 2 for 2007–2012, 2013–2018, and 2019–2024.
- `funnel plot data.xlsx`: cleaned input file used for the funnel-plot analyses.
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

## Analyses included

### 1. Mixed-effects models (Table 2A and Table 2B)

The mixed-effects script fits grouped binomial mixed-effects models with logit link for the conditional probability of suicide certification among non-homicide drug poisoning deaths classified as suicide or undetermined intent.

- **Table 2A**: combined grouped binomial mixed-effects model across all pooled periods, with fixed effects for sex, age group, and study period and a random intercept for state.
- **Table 2B**: separate period-specific grouped binomial mixed-effects models for 2007–2012, 2013–2018, and 2019–2024, with fixed effects for sex and age group and a random intercept for state. These models summarize adjusted between-state heterogeneity within each pooled period using state-level variance (τ²), intraclass correlation coefficient (ICC), median odds ratio (MOR), and likelihood-ratio testing.

### 2. Funnel plots (Figure 2 and Supplementary Figure S1)

The funnel-plot scripts generate period-specific state-level funnel plots for 2007–2012, 2013–2018, and 2019–2024. The x-axis is the funnel-plot denominator, defined as suicide plus undetermined-intent drug poisoning deaths, and the y-axis is the conditional suicide proportion, calculated as suicide / (suicide + undetermined). The pooled national conditional suicide proportion is shown as the center line, with 95% and 99.8% control limits. The Figure 2 script produces a simplified version for the main manuscript, whereas the supplementary funnel-plot script produces fully labeled versions.

## Notes

- Each R script expects its corresponding input file to be in the same folder as the script.
- The mixed-effects script excludes strata marked `Use in model = Yes/No` according to the analytic file and does not impute suppressed counts.
- The funnel-plot scripts classify states outside the 99.8% control limits as outliers.
- In the Figure 2 funnel plots, states outside the 99.8% control limits in all three periods are shown as persistent outliers.
