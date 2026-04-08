# =========================================================
# Table2_model_analysis.R
# Grouped binomial mixed-effects models for Table 2A and 2B
# =========================================================

library(readxl)
library(dplyr)
library(lme4)
library(writexl)
library(tibble)

# -------------------------
# 1. Import and prepare data
# -------------------------

dat <- read_excel("Table3 model.xlsx", sheet = "Model Data") %>%
  rename(
    Age_group = `Age group`,
    Use_in_model = `Use in model`
  ) %>%
  filter(Use_in_model == "Yes") %>%
  mutate(
    Period = factor(Period, levels = c("2007-2012", "2013-2018", "2019-2024")),
    Sex = factor(Sex, levels = c("Female", "Male")),
    Age_group = factor(Age_group, levels = c("15-34", "35-54", "55+")),
    State = factor(State)
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

# -------------------------
# 3. Combined model for Table 2A
# -------------------------

fit_combined <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period + (1 | State),
  family = binomial,
  data = dat,
  nAGQ = 1,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Fixed effects
beta <- fixef(fit_combined)
se <- sqrt(diag(vcov(fit_combined)))

OR  <- exp(beta)
LCL <- exp(beta - 1.96 * se)
UCL <- exp(beta + 1.96 * se)

# Random-effect heterogeneity
tau2_combined <- as.data.frame(VarCorr(fit_combined))$vcov[1]
icc_combined  <- tau2_combined / (tau2_combined + (pi^2 / 3))
mor_combined  <- exp(0.6745 * sqrt(2 * tau2_combined))

# Fixed-effects-only comparison model
fit_combined_fixed <- glm(
  cbind(Suicide, Undetermined) ~ Sex + Age_group + Period,
  family = binomial,
  data = dat
)

lr_stat_combined <- 2 * (as.numeric(logLik(fit_combined)) - as.numeric(logLik(fit_combined_fixed)))
lr_df_combined   <- attr(logLik(fit_combined), "df") - attr(logLik(fit_combined_fixed), "df")
lr_p_combined    <- pchisq(lr_stat_combined, df = lr_df_combined, lower.tail = FALSE)

# Build Table 2A
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
    "Intraclass correlation coefficient (ICC)",
    "Median odds ratio (MOR)",
    "Model statistics",
    "Included state-strata (N)",
    "Represented jurisdictions, n",
    "Likelihood-ratio test for state random intercept"
  ),
  `Combined model, aOR (95% CI)` = c(
    "",
    "",
    "1",
    format_or_ci(OR["SexMale"], LCL["SexMale"], UCL["SexMale"]),
    "",
    "1",
    format_or_ci(OR["Age_group35-54"], LCL["Age_group35-54"], UCL["Age_group35-54"]),
    format_or_ci(OR["Age_group55+"], LCL["Age_group55+"], UCL["Age_group55+"]),
    "",
    "1",
    format_or_ci(OR["Period2013-2018"], LCL["Period2013-2018"], UCL["Period2013-2018"]),
    format_or_ci(OR["Period2019-2024"], LCL["Period2019-2024"], UCL["Period2019-2024"]),
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
# 4. Period-specific models for Table 2B
# -------------------------

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

  tau2 <- as.data.frame(VarCorr(fit_mixed))$vcov[1]
  icc  <- tau2 / (tau2 + (pi^2 / 3))
  mor  <- exp(0.6745 * sqrt(2 * tau2))

  lr_stat <- 2 * (as.numeric(logLik(fit_mixed)) - as.numeric(logLik(fit_fixed)))
  lr_df   <- attr(logLik(fit_mixed), "df") - attr(logLik(fit_fixed), "df")
  lr_p    <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

  tibble(
    `Included state-strata (N)` = nrow(data_period),
    `Represented jurisdictions, n` = n_distinct(data_period$State),
    `State-level variance (τ²)` = round(tau2, 3),
    `ICC` = round(icc, 3),
    `MOR` = round(mor, 2),
    `Likelihood-ratio test for state random intercept` = format_p(lr_p)
  )
}

metrics_07012 <- dat %>%
  filter(Period == "2007-2012") %>%
  get_period_metrics()

metrics_1318 <- dat %>%
  filter(Period == "2013-2018") %>%
  get_period_metrics()

metrics_1924 <- dat %>%
  filter(Period == "2019-2024") %>%
  get_period_metrics()

# Wide Table 2B
table2b <- tibble(
  Measure = c(
    "Included state-strata (N)",
    "Represented jurisdictions, n",
    "State-level variance (τ²)",
    "ICC",
    "MOR",
    "Likelihood-ratio test for state random intercept"
  ),
  `2007–2012` = c(
    metrics_07012$`Included state-strata (N)`,
    metrics_07012$`Represented jurisdictions, n`,
    metrics_07012$`State-level variance (τ²)`,
    metrics_07012$ICC,
    metrics_07012$MOR,
    metrics_07012$`Likelihood-ratio test for state random intercept`
  ),
  `2013–2018` = c(
    metrics_1318$`Included state-strata (N)`,
    metrics_1318$`Represented jurisdictions, n`,
    metrics_1318$`State-level variance (τ²)`,
    metrics_1318$ICC,
    metrics_1318$MOR,
    metrics_1318$`Likelihood-ratio test for state random intercept`
  ),
  `2019–2024` = c(
    metrics_1924$`Included state-strata (N)`,
    metrics_1924$`Represented jurisdictions, n`,
    metrics_1924$`State-level variance (τ²)`,
    metrics_1924$ICC,
    metrics_1924$MOR,
    metrics_1924$`Likelihood-ratio test for state random intercept`
  )
)

# -------------------------
# 5. Print results to console
# -------------------------

cat("\n====================\n")
cat("Table 2A\n")
cat("====================\n")
print(table2a, n = Inf)

cat("\n====================\n")
cat("Table 2B\n")
cat("====================\n")
print(table2b, n = Inf)

# -------------------------
# 6. Export results
# -------------------------

write_xlsx(
  list(
    "Table 2A" = table2a,
    "Table 2B" = table2b
  ),
  "Table_2_results.xlsx"
)

cat("\nResults exported to: Table_2_results.xlsx\n")
