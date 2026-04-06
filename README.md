# Drug-poisoning-classification-code

R code and analytic file for the combined grouped binomial mixed-effects model used in Table 3 of the manuscript.

## Files
- `Table3_model_analysis.R`: fits the combined mixed-effects model and derives adjusted odds ratios, ICC, MOR, and likelihood-ratio results.
- `Table3 model.xlsx`: combined analytic file used for the model.
  - Sheet name: `Model Data`

## Software
- R 4.5.3
- readxl
- dplyr
- lme4

## Model
The script fits a grouped binomial mixed-effects model with logit link for the conditional probability of suicide certification among non-homicide drug poisoning deaths classified as suicide or undetermined intent.

## Notes
The script expects the Excel file `Table3 model.xlsx` to be in the same folder as the R script.
