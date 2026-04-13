# =========================================================
# Table2_Table3_model_analysis.R
# Grouped binomial mixed-effects models for:
#   - Table 2: main adjusted heterogeneity models
#   - Table 3: exploratory structural models adding
#              medicolegal death investigation (MDI) system type
# =========================================================

# -------------------------
# 0. Packages
# -------------------------
library(readxl)
library(dplyr)
library(lme4)
library(writexl)
library(tibble)
library(stringr)

# -------------------------
# 1. Import and prepare data
# -------------------------

# Update this path if needed, or replace with a simple filename
# if the Excel file is in your working directory.
input_file <- "Table_for_R_model_with_MDI_variables.xlsx"

dat <- read_excel(input_file, sheet = "Model Data") %>%
  rename(
    Age_group = `Age group`,
    Use_in_model = `Use in model`
  ) %>%
  filter(Use_in_model == "Yes") %>%
  mutate(
    Period = factor(Period, levels = c("2007-2012", "2013-2018", "2019-2024")),
    Sex = factor(Sex, levels = c("Female", "Male")),
    Age_group = factor(Age_group, levels = c("15-34", "35-54", "55+")),
    State = factor(State),
    BJS_3sys = factor(BJS_3sys),
    BJS_4sys = factor(BJS_4sys),
    CDC_3sys_TXmixed = factor(CDC_3sys_TXmixed),
    CDC_4sys_TXmixed = factor(CDC_4sys_TXmixed)
  )

# 2019-2024 dataset for Table 3 primary/sensitivity models
dat_2019 <- dat %>%
  filter(Period == "2019-2024") %>%
  mutate(
    BJS_3sys = relevel(BJS_3sys, ref = "Centralized_ME"),
    BJS_4sys = relevel(BJS_4sys, ref = "Centralized_ME"),
    CDC_3sys_TXmixed = relevel(CDC_3sys_TXmixed, ref = "Centralized_ME"),
    CDC_4sys_TXmixed = relevel(CDC_4sys_TXmixed, ref = "Centralized_ME")
  )

# Relevel full dataset too for optional full-period structural sensitivity models
dat <- dat %>%
  mutate(
    BJS_3sys = relevel(BJS_3sys, ref = "Centralized_ME"),
    BJS_4sys = relevel(BJS_4sys, ref = "Centralized_ME"),
    CDC_3sys_TXmixed = relevel(CDC_3sys_TXmixed, ref = "Centralized_ME"),
    CDC_4sys_TXmixed = relevel(CDC_4sys_TXmixed, ref = "Centralized_ME")
  )

# -------------------------
# 2. Helper functions
# -------------------------

format_or_ci <- function(or, lo, hi, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
    or, lo, hi
  )
}

format_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", sprintf("%.3f", p)))
}

get_tau2 <- function(model) {
  as.numeric(VarCorr(model)$State[1])
}

get_icc <- function(model) {
  tau2 <- get_tau2(model)
  tau2 / (tau2 + (pi^2 / 3))
}

get_mor <- function(model) {
  tau2 <- get_tau2(model)
  exp(0.6745 * sqrt(2 * tau2))
}

pct_reduction_tau2 <- function(base_model, new_model) {
  base_tau2 <- get_tau2(base_model)
  new_tau2  <- get_tau2(new_model)
  100 * (base_tau2 - new_tau2) / base_tau2
}

extract_or_ci <- function(model, pattern, ref_label = "1.00") {
  sm <- summary(model)$coefficients
  rn <- rownames(sm)
  keep <- grepl(pattern, rn)

  out <- data.frame(
    term = rn[keep],
    beta = sm[keep, "Estimate"],
    se = sm[keep, "Std. Error"],
    z = sm[keep, "z value"],
    p = sm[keep, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )

  out %>%
    mutate(
      aOR = exp(beta),
      LCL = exp(beta - 1.96 * se),
      UCL = exp(beta + 1.96 * se),
      aOR_CI = format_or_ci(aOR, LCL, UCL, digits = 2),
      p_value = format_p(p)
    )
}

get_period_metrics <- function(data_period) {
  fit_mixed <- glmer(
    cbind(Suicide, Undetermined) ~ Sex + Age_group + (1 | State),
    family = binomial,
    data = data_period,
    nAGQ = 1,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )

  fit_fixed <- glm(
    cbind(Suicide, Undetermined) ~ Sex + Age_group,
    family = binomial,
    data = data_period
  )

  tau2 <- get_tau2(fit_mixed)
  icc  <- get_icc(fit_mixed)
  mor  <- get_mor(fit_mixed)

  lr_stat <- 2 * (as.numeric(logLik(fit_mixed)) - as.numeric(logLik(fit_fixed)))
  lr_df   <- attr(logLik(fit_mixed), "df") - attr(logLik(fit_fixed), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

  list(
    fit = fit_mixed,
    metrics = tibble(
      `Included state-strata (N)` = nrow(data_period),
      `Represented jurisdictions, n` = n_distinct(data_period$State),
      `State-level variance (τ²)` = round(tau2, 3),
      `ICC` = round(icc, 3),
      `MOR` = round(mor, 2),
      `Likelihood-ratio test for state random intercept` = format_p(lr_p)
    )
  )
}

# -------------------------
# 3. Table 2A: Combined model
# -------------------------

fit_combined <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Fixed effects
beta_combined <- fixef(fit_combined)
se_combined <- sqrt(diag(vcov(fit_combined)))

OR_combined  <- exp(beta_combined)
LCL_combined <- exp(beta_combined - 1.96 * se_combined)
UCL_combined <- exp(beta_combined + 1.96 * se_combined)

# Random effects
tau2_combined <- get_tau2(fit_combined)
icc_combined  <- get_icc(fit_combined)
mor_combined  <- get_mor(fit_combined)

# Fixed-effects-only comparison model
fit_combined_fixed <- glm(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period,
  family = binomial,
  data = dat
)

lr_stat_combined <- 2 * (as.numeric(logLik(fit_combined)) - as.numeric(logLik(fit_combined_fixed)))
lr_df_combined   <- attr(logLik(fit_combined), "df") - attr(logLik(fit_combined_fixed), "df")
lr_p_combined    <- pchisq(lr_stat_combined, df = lr_df_combined, lower.tail = FALSE)

table2a <- tibble(
  Model_parameter = c(
    "Fixed effects",
    "Sex",
    "Female (Ref)",
    "Male",
    "Age group",
    "15–34 (Ref)",
    "35–54",
    "55+",
    "Study period",
    "2007–2012 (Ref)",
    "2013–2018",
    "2019–2024",
    "Random effects & heterogeneity",
    "State-level variance (τ²)",
    "ICC",
    "MOR",
    "Model statistics",
    "Included strata, N",
    "Represented jurisdictions, n",
    "Likelihood-ratio test for state random intercept"
  ),
  `Combined model, aOR (95% CI)` = c(
    "",
    "",
    "1.00",
    format_or_ci(OR_combined["SexMale"], LCL_combined["SexMale"], UCL_combined["SexMale"]),
    "",
    "1.00",
    format_or_ci(OR_combined["Age_group35-54"], LCL_combined["Age_group35-54"], UCL_combined["Age_group35-54"]),
    format_or_ci(OR_combined["Age_group55+"], LCL_combined["Age_group55+"], UCL_combined["Age_group55+"]),
    "",
    "1.00",
    format_or_ci(OR_combined["Period2013-2018"], LCL_combined["Period2013-2018"], UCL_combined["Period2013-2018"]),
    format_or_ci(OR_combined["Period2019-2024"], LCL_combined["Period2019-2024"], UCL_combined["Period2019-2024"]),
    "",
    sprintf("%.3f", tau2_combined),
    sprintf("%.3f", icc_combined),
    sprintf("%.2f", mor_combined),
    "",
    as.character(nrow(dat)),
    as.character(n_distinct(dat$State)),
    format_p(lr_p_combined)
  )
)

# -------------------------
# 4. Table 2B: Period-specific models
# -------------------------

res_07012 <- dat %>% filter(Period == "2007-2012") %>% get_period_metrics()
res_1318  <- dat %>% filter(Period == "2013-2018") %>% get_period_metrics()
res_1924  <- dat %>% filter(Period == "2019-2024") %>% get_period_metrics()

table2b <- tibble(
  Measure = c(
    "Included strata, N",
    "Represented jurisdictions, n",
    "State-level variance (τ²)",
    "ICC",
    "MOR",
    "Likelihood-ratio test for state random intercept"
  ),
  `2007–2012` = c(
    res_07012$metrics$`Included state-strata (N)`,
    res_07012$metrics$`Represented jurisdictions, n`,
    res_07012$metrics$`State-level variance (τ²)`,
    res_07012$metrics$ICC,
    res_07012$metrics$MOR,
    res_07012$metrics$`Likelihood-ratio test for state random intercept`
  ),
  `2013–2018` = c(
    res_1318$metrics$`Included state-strata (N)`,
    res_1318$metrics$`Represented jurisdictions, n`,
    res_1318$metrics$`State-level variance (τ²)`,
    res_1318$metrics$ICC,
    res_1318$metrics$MOR,
    res_1318$metrics$`Likelihood-ratio test for state random intercept`
  ),
  `2019–2024` = c(
    res_1924$metrics$`Included state-strata (N)`,
    res_1924$metrics$`Represented jurisdictions, n`,
    res_1924$metrics$`State-level variance (τ²)`,
    res_1924$metrics$ICC,
    res_1924$metrics$MOR,
    res_1924$metrics$`Likelihood-ratio test for state random intercept`
  )
)

# -------------------------
# 5. Table 3: Structural models
# -------------------------

# Base 2019-2024 model for comparison
m_base_2019 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + (1 | State),
  family = binomial,
  data = dat_2019,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Primary structural model: BJS 3-category
m_bjs3_2019 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + BJS_3sys + (1 | State),
  family = binomial,
  data = dat_2019,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Sensitivity 1: BJS 4-category
m_bjs4_2019 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + BJS_4sys + (1 | State),
  family = binomial,
  data = dat_2019,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Sensitivity 2: CDC-informed 3-category recode
m_cdc3_2019 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + CDC_3sys_TXmixed + (1 | State),
  family = binomial,
  data = dat_2019,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Optional: CDC-informed 4-category model (archived output only)
m_cdc4_2019 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + CDC_4sys_TXmixed + (1 | State),
  family = binomial,
  data = dat_2019,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Coefficients with 95% CIs
ors_bjs3_2019 <- extract_or_ci(m_bjs3_2019, "^BJS_3sys")
ors_bjs4_2019 <- extract_or_ci(m_bjs4_2019, "^BJS_4sys")
ors_cdc3_2019 <- extract_or_ci(m_cdc3_2019, "^CDC_3sys_TXmixed")
ors_cdc4_2019 <- extract_or_ci(m_cdc4_2019, "^CDC_4sys_TXmixed")

# Base late-period metrics
tau2_base_2019 <- get_tau2(m_base_2019)
icc_base_2019  <- get_icc(m_base_2019)
mor_base_2019  <- get_mor(m_base_2019)

# Panel A
table3a <- tibble(
  Model_parameter = c(
    "MDI system type",
    "Centralized statewide medical examiner (Ref)",
    "Coroner or mixed/other local system",
    "Decentralized medical examiner only",
    "Random effects & heterogeneity",
    "Jurisdiction-level variance (τ²)",
    "ICC",
    "MOR",
    "% reduction in τ² vs base 2019–2024 model",
    "Model statistics",
    "Included strata, N",
    "Represented jurisdictions, n"
  ),
  `2019–2024 BJS 3-category model, aOR (95% CI) / value` = c(
    "",
    "1.00",
    ors_bjs3_2019$aOR_CI[1],
    ors_bjs3_2019$aOR_CI[2],
    "",
    sprintf("%.3f", get_tau2(m_bjs3_2019)),
    sprintf("%.3f", get_icc(m_bjs3_2019)),
    sprintf("%.2f", get_mor(m_bjs3_2019)),
    sprintf("%.1f%%", pct_reduction_tau2(m_base_2019, m_bjs3_2019)),
    "",
    as.character(nrow(dat_2019)),
    as.character(n_distinct(dat_2019$State))
  )
)

# Panel B
table3b <- tibble(
  Measure = c(
    "Jurisdiction-level variance (τ²)",
    "ICC",
    "MOR",
    "% reduction in τ² vs base 2019–2024 model",
    "Included strata, N",
    "Represented jurisdictions, n"
  ),
  `BJS 4-system coding` = c(
    sprintf("%.3f", get_tau2(m_bjs4_2019)),
    sprintf("%.3f", get_icc(m_bjs4_2019)),
    sprintf("%.2f", get_mor(m_bjs4_2019)),
    sprintf("%.1f%%", pct_reduction_tau2(m_base_2019, m_bjs4_2019)),
    as.character(nrow(dat_2019)),
    as.character(n_distinct(dat_2019$State))
  ),
  `CDC-informed 3-category recode` = c(
    sprintf("%.3f", get_tau2(m_cdc3_2019)),
    sprintf("%.3f", get_icc(m_cdc3_2019)),
    sprintf("%.2f", get_mor(m_cdc3_2019)),
    sprintf("%.1f%%", pct_reduction_tau2(m_base_2019, m_cdc3_2019)),
    as.character(nrow(dat_2019)),
    as.character(n_distinct(dat_2019$State))
  )
)

# -------------------------
# 6. Optional: Full-period structural sensitivity models
# -------------------------

m_base_full <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

m_bjs3_full <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + BJS_3sys + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

m_bjs4_full <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + BJS_4sys + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

m_cdc3_full <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + CDC_3sys_TXmixed + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

supp_full_period <- tibble(
  Model = c("BJS 3-category", "BJS 4-category", "CDC-informed 3-category"),
  `Jurisdiction-level variance (τ²)` = c(
    sprintf("%.3f", get_tau2(m_bjs3_full)),
    sprintf("%.3f", get_tau2(m_bjs4_full)),
    sprintf("%.3f", get_tau2(m_cdc3_full))
  ),
  ICC = c(
    sprintf("%.3f", get_icc(m_bjs3_full)),
    sprintf("%.3f", get_icc(m_bjs4_full)),
    sprintf("%.3f", get_icc(m_cdc3_full))
  ),
  MOR = c(
    sprintf("%.2f", get_mor(m_bjs3_full)),
    sprintf("%.2f", get_mor(m_bjs4_full)),
    sprintf("%.2f", get_mor(m_cdc3_full))
  ),
  `% reduction in τ² vs base full-period model` = c(
    sprintf("%.1f%%", pct_reduction_tau2(m_base_full, m_bjs3_full)),
    sprintf("%.1f%%", pct_reduction_tau2(m_base_full, m_bjs4_full)),
    sprintf("%.1f%%", pct_reduction_tau2(m_base_full, m_cdc3_full))
  ),
  `Included strata, N` = c(nrow(dat), nrow(dat), nrow(dat)),
  `Represented jurisdictions, n` = c(n_distinct(dat$State), n_distinct(dat$State), n_distinct(dat$State))
)

# -------------------------
# 7. Manuscript-ready results text for Table 3
# -------------------------

results_text_table3 <- paste0(
  "In exploratory structural modeling, adding jurisdiction-level medicolegal death investigation (MDI) system type to the 2019–2024 model attenuated between-jurisdiction heterogeneity only modestly. ",
  "In the primary BJS-based 3-category model, the jurisdiction-level variance decreased from ",
  sprintf("%.3f", tau2_base_2019),
  " in the base model to ",
  sprintf("%.3f", get_tau2(m_bjs3_2019)),
  ", corresponding to an ICC of ",
  sprintf("%.3f", get_icc(m_bjs3_2019)),
  ", an MOR of ",
  sprintf("%.2f", get_mor(m_bjs3_2019)),
  ", and a ",
  sprintf("%.1f", pct_reduction_tau2(m_base_2019, m_bjs3_2019)),
  "% reduction in τ². Relative to centralized statewide medical examiner systems, neither coroner or mixed/other local systems (aOR, ",
  ors_bjs3_2019$aOR_CI[1],
  ") nor decentralized medical-examiner-only systems (aOR, ",
  ors_bjs3_2019$aOR_CI[2],
  ") showed a clear association with the conditional suicide proportion. ",
  "Sensitivity analyses yielded similar results. The BJS 4-category model produced the greatest attenuation, but the reduction in heterogeneity remained small (τ², ",
  sprintf("%.3f", get_tau2(m_bjs4_2019)),
  "; ICC, ",
  sprintf("%.3f", get_icc(m_bjs4_2019)),
  "; MOR, ",
  sprintf("%.2f", get_mor(m_bjs4_2019)),
  "; ",
  sprintf("%.1f", pct_reduction_tau2(m_base_2019, m_bjs4_2019)),
  "% reduction in τ²). The CDC-informed Texas recode changed results minimally (τ², ",
  sprintf("%.3f", get_tau2(m_cdc3_2019)),
  "; ICC, ",
  sprintf("%.3f", get_icc(m_cdc3_2019)),
  "; MOR, ",
  sprintf("%.2f", get_mor(m_cdc3_2019)),
  "; ",
  sprintf("%.1f", pct_reduction_tau2(m_base_2019, m_cdc3_2019)),
  "% reduction in τ²). Overall, broad MDI system structure explained little of the observed late-period interstate heterogeneity."
)

writeLines(results_text_table3, "Table3_results_text.txt")

# -------------------------
# 8. Print to console
# -------------------------

cat("\n====================\n")
cat("Table 2A\n")
cat("====================\n")
print(table2a, n = Inf)

cat("\n====================\n")
cat("Table 2B\n")
cat("====================\n")
print(table2b, n = Inf)

cat("\n====================\n")
cat("Table 3A\n")
cat("====================\n")
print(table3a, n = Inf)

cat("\n====================\n")
cat("Table 3B\n")
cat("====================\n")
print(table3b, n = Inf)

cat("\n====================\n")
cat("Base 2019–2024 model for Table 3 comparison\n")
cat("====================\n")
cat("τ² =", round(tau2_base_2019, 3), "\n")
cat("ICC =", round(icc_base_2019, 3), "\n")
cat("MOR =", round(mor_base_2019, 2), "\n")

# -------------------------
# 9. Export results
# -------------------------

write_xlsx(
  list(
    "Table 2A" = table2a,
    "Table 2B" = table2b,
    "Table 3A" = table3a,
    "Table 3B" = table3b,
    "Table 3 ORs BJS 3-system" = ors_bjs3_2019,
    "Table 3 ORs BJS 4-system" = ors_bjs4_2019,
    "Table 3 ORs CDC 3-system" = ors_cdc3_2019,
    "Table 3 ORs CDC 4-system" = ors_cdc4_2019,
    "Supplementary full-period structural sensitivity" = supp_full_period
  ),
  "Table2_Table3_results.xlsx"
)

cat("\nResults exported to: Table2_Table3_results.xlsx\n")
cat("Narrative text exported to: Table3_results_text.txt\n")
