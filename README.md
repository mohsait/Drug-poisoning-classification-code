# Drug-poisoning-classification-code

R code and analytic files for the manuscript:

**Suicide Versus Undetermined-Intent Classification of U.S. Drug-Poisoning Deaths, 2007–2024: Persistent Jurisdictional Variation Not Accounted for by Observable Jurisdiction-Level Features**

## Repository purpose

This repository provides the analytic code and aggregate CDC WONDER-derived input files used to reproduce the main statistical analyses and figures for a study of U.S. non-homicide drug-poisoning deaths from 2007 through 2024.

The study evaluates jurisdictional variation in official certification of drug-poisoning deaths as suicide versus undetermined intent. The primary outcome was the conditional suicide proportion (CSP):

**CSP = suicide / (suicide + undetermined intent)**

The analyses quantify jurisdictional heterogeneity across three pooled 6-year periods and test whether observable jurisdiction-level features account for residual late-period heterogeneity.

## Files

### Analytic_Model_Data.xlsx

Master analytic workbook used for the statistical analyses.

This file contains aggregate CDC WONDER-derived analytic datasets used for:

- national descriptive summaries;
- combined grouped-binomial mixed-effects model;
- period-specific grouped-binomial mixed-effects models;
- 2019–2024 jurisdiction-level proxy models;
- MDI coding sensitivity analyses;
- certificate-quality proxy sensitivity analyses;
- Maryland-exclusion sensitivity analysis;
- drug-restricted supplementary analyses;
- funnel-plot input data.

All datasets are aggregate, de-identified, CDC WONDER-derived tabulations. No individual-level data are included.

### Main_Model_and_Proxy_Analyses.R

Fits the grouped-binomial mixed-effects models used for the main adjusted heterogeneity analyses and 2019–2024 proxy attenuation analyses.

The script estimates:

- adjusted odds ratios and 95% confidence intervals;
- jurisdiction-level random-intercept variance, τ²;
- intraclass correlation coefficient, ICC;
- median odds ratio, MOR;
- likelihood-ratio tests for the jurisdiction random intercept;
- percentage reduction in τ² after adding jurisdiction-level proxies.

The script supports the manuscript results for:

- Table 2: adjusted jurisdictional heterogeneity models;
- Table 3: attenuation of residual jurisdictional heterogeneity after adding observable jurisdiction-level proxies;
- Supplementary Table S6: MDI structural model details;
- Supplementary Table S7: proxy model details;
- Supplementary Table S9: proxy-denominator sensitivity;
- Supplementary Table S10: Maryland-exclusion sensitivity;
- Supplementary Table S11: drug-restricted analyses.

### Figure2_funnel_plots_main.R

Generates the main-manuscript funnel plots for jurisdiction-level CSP across the three pooled periods:

- 2007–2012;
- 2013–2018;
- 2019–2024.

The x-axis is the funnel-plot denominator, defined as suicide plus undetermined-intent drug-poisoning deaths. The y-axis is CSP.

### Supplementary_Figure_S1_funnel_plots.R

Generates the fully labeled supplementary funnel plots, including:

- poisoning CSP funnel plots for 2007–2012, 2013–2018, and 2019–2024;
- firearm CSP benchmark funnel plot for 2019–2024.

## Software

Analyses were performed in R version 4.5.3.

Required R packages include:

- readxl
- dplyr
- ggplot2
- ggrepel
- lme4
- writexl
- tibble
- stringr

## Analyses included

### 1. Main mixed-effects models

The main mixed-effects script fits grouped-binomial mixed-effects models with logit link for the conditional probability of suicide certification among non-homicide drug-poisoning deaths classified as either suicide or undetermined intent.

The grouped-binomial response was specified as:

- numerator: suicide-certified drug-poisoning deaths;
- denominator: suicide-certified plus undetermined-intent drug-poisoning deaths.

The combined model included:

- sex;
- age group;
- pooled study period;
- jurisdiction random intercept.

Period-specific models were fit separately for:

- 2007–2012;
- 2013–2018;
- 2019–2024.

These models summarize adjusted between-jurisdiction heterogeneity using τ², ICC, MOR, and likelihood-ratio testing.

### 2. Jurisdiction-level proxy models

The 2019–2024 proxy models test whether observable jurisdiction-level features attenuate residual heterogeneity in CSP.

The three observable proxies were:

1. medicolegal death investigation system type;
2. autopsy proportion;
3. drug-specification completeness.

MDI system type was modeled as a categorical jurisdiction-level descriptor. Autopsy proportion and drug-specification completeness were modeled as continuous jurisdiction-level proportions scaled per 10 percentage points.

The following 2019–2024 models were fit:

1. base 2019–2024 model;
2. base model plus MDI system type;
3. base model plus autopsy proportion;
4. base model plus drug-specification completeness;
5. base model plus autopsy proportion and drug-specification completeness;
6. base model plus all three proxies.

Percentage reduction in jurisdiction random-intercept variance was calculated as:

**100 × (τ²_base − τ²_proxy model) / τ²_base**

These attenuation results are interpreted descriptively and not as causal mediation estimates.

### 3. MDI coding sensitivity analyses

MDI sensitivity analyses compare the primary Bureau of Justice Statistics-derived 3-category model with alternative MDI recodings.

The primary MDI coding used:

1. centralized statewide medical examiner system;
2. decentralized medical-examiner-only system;
3. coroner/mixed/other local system.

Sensitivity analyses included:

- BJS 4-category coding;
- CDC-informed 3-category recode moving Texas out of the decentralized medical-examiner-only category.

### 4. Certificate-quality proxy sensitivity analysis

The primary proxy definitions used all eligible non-homicide drug-poisoning deaths as the denominator to reduce dependence between the proxies and the suicide-versus-undetermined outcome.

A sensitivity analysis recalculated:

- autopsy proportion;
- drug-specification completeness;

using only suicide plus undetermined-intent drug-poisoning deaths as the denominator.

### 5. Maryland-exclusion sensitivity analysis

Because Maryland was an extreme low-CSP jurisdiction across all three pooled periods, the combined model and the 2019–2024 model were repeated after excluding Maryland.

This analysis evaluates whether residual heterogeneity was driven primarily by one extreme jurisdiction.

### 6. Drug-restricted supplementary analyses

Drug-restricted analyses were performed by restricting otherwise eligible deaths to selected multiple-cause drug codes one at a time:

- T40.1: heroin;
- T40.2: natural and semisynthetic opioids;
- T40.3: methadone;
- T40.4: synthetic opioids other than methadone;
- T40.5: cocaine;
- T40.6: other and unspecified narcotics;
- T43.6: psychostimulants with abuse potential.

These subsets are not mutually exclusive because deaths with multiple listed drug codes may contribute to more than one subset.

### 7. Funnel plots

Funnel plots were generated for jurisdiction-level CSP estimates by pooled period.

Each poisoning funnel plot uses:

- x-axis: suicide plus undetermined-intent drug-poisoning deaths;
- y-axis: conditional suicide proportion;
- center line: pooled national CSP for the period;
- control limits: exact-binomial 95% and 99.8% limits.

Jurisdictions outside the 99.8% control limits were classified as funnel-plot outliers. Jurisdictions outside the 99.8% limits in all three pooled periods were identified as persistent outliers.

The firearm benchmark plot used an analogous firearm CSP:

**firearm suicide / (firearm suicide + firearm undetermined intent)**

The firearm benchmark was included as contextual comparison only and was not used as a formal mechanism-comparison analysis.

## Notes on data handling

- CDC WONDER-suppressed counts were treated as missing and were not imputed.
- Observable zero cells were retained.
- Model inclusion depended on directly observable jurisdiction-period-sex-age strata.
- A jurisdiction could appear in aggregate descriptive analyses but not in a stratified mixed-effects model if required model strata were suppressed.
- Drug-restricted subsets were interpreted as overlapping robustness analyses rather than independent drug-defined populations.
- All analyses used aggregate, de-identified CDC WONDER-derived tabulations.

## Code availability

The R scripts assume that **Analytic_Model_Data.xlsx** is located in the same folder as the scripts unless a full file path is specified.

## Citation

If using or referencing this repository, please cite the associated manuscript:

**Suicide Versus Undetermined-Intent Classification of U.S. Drug-Poisoning Deaths, 2007–2024: Persistent Jurisdictional Variation Not Accounted for by Observable Jurisdiction-Level Features**
