options(stringsAsFactors = FALSE)

input_file <- "Analytic_Model_Data.xlsx"
output_dir <- "model_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

required_packages <- c("readxl", "dplyr", "lme4", "writexl", "tibble", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages before running: ", paste(missing_packages, collapse = ", "))
}

library(readxl)
library(dplyr)
library(lme4)
library(writexl)
library(tibble)
library(stringr)

clean_colnames <- function(x) {
  x <- gsub("%", "pct", x)
  x <- gsub("≥", "ge", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

read_clean <- function(sheet) {
  x <- readxl::read_excel(input_file, sheet = sheet, .name_repair = "unique")
  names(x) <- clean_colnames(names(x))
  x
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

fmt_aor <- function(aor, lo, hi, digits = 2) {
  ifelse(is.na(aor), NA_character_, paste0(formatC(aor, format = "f", digits = digits), " (", formatC(lo, format = "f", digits = digits), "–", formatC(hi, format = "f", digits = digits), ")"))
}

prep_model_data <- function(x) {
  required <- c("period", "state", "sex", "age_group", "suicide", "undetermined", "use_in_model")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  x %>%
    mutate(
      suicide = as_num(suicide),
      undetermined = as_num(undetermined),
      denominator = if ("denominator" %in% names(.)) as_num(denominator) else suicide + undetermined,
      period = factor(as.character(period), levels = c("2007-2012", "2013-2018", "2019-2024")),
      state = factor(as.character(state)),
      sex = factor(as.character(sex), levels = c("Female", "Male")),
      age_group = factor(as.character(age_group), levels = c("15-34", "35-54", "ge55", "≥55", "55+")),
      age_group = case_when(
        as.character(age_group) %in% c("ge55", "55+") ~ "≥55",
        TRUE ~ as.character(age_group)
      ),
      age_group = factor(age_group, levels = c("15-34", "35-54", "≥55")),
      use_in_model = as.character(use_in_model)
    ) %>%
    filter(tolower(use_in_model) %in% c("yes", "y", "true", "1")) %>%
    filter(!is.na(suicide), !is.na(undetermined), !is.na(state), !is.na(sex), !is.na(age_group)) %>%
    filter(suicide >= 0, undetermined >= 0, suicide + undetermined > 0) %>%
    droplevels()
}

add_proxy_variables <- function(x) {
  x <- x %>%
    mutate(
      bjs_3sys = if ("bjs_3sys" %in% names(.)) factor(as.character(bjs_3sys)) else NA,
      bjs_4sys = if ("bjs_4sys" %in% names(.)) factor(as.character(bjs_4sys)) else NA,
      cdc_3sys_txmixed = if ("cdc_3sys_txmixed" %in% names(.)) factor(as.character(cdc_3sys_txmixed)) else NA,
      autopsy_prop_all_drug = if ("autopsy_prop_all_drug" %in% names(.)) as_num(autopsy_prop_all_drug) else NA_real_,
      drug_spec_complete_all_drug = if ("drug_spec_complete_all_drug" %in% names(.)) as_num(drug_spec_complete_all_drug) else NA_real_,
      autopsy_prop_su = if ("autopsy_prop_su" %in% names(.)) as_num(autopsy_prop_su) else NA_real_,
      drug_spec_complete_su = if ("drug_spec_complete_su" %in% names(.)) as_num(drug_spec_complete_su) else NA_real_
    )
  if (!"autopsy_prop_all_drug_per10pp" %in% names(x)) x$autopsy_prop_all_drug_per10pp <- x$autopsy_prop_all_drug * 10
  if (!"drug_spec_complete_all_drug_per10pp" %in% names(x)) x$drug_spec_complete_all_drug_per10pp <- x$drug_spec_complete_all_drug * 10
  if (!"autopsy_prop_su_per10pp" %in% names(x)) x$autopsy_prop_su_per10pp <- x$autopsy_prop_su * 10
  if (!"drug_spec_complete_su_per10pp" %in% names(x)) x$drug_spec_complete_su_per10pp <- x$drug_spec_complete_su * 10
  x %>%
    mutate(
      autopsy_prop_all_drug_per10pp = as_num(autopsy_prop_all_drug_per10pp),
      drug_spec_complete_all_drug_per10pp = as_num(drug_spec_complete_all_drug_per10pp),
      autopsy_prop_su_per10pp = as_num(autopsy_prop_su_per10pp),
      drug_spec_complete_su_per10pp = as_num(drug_spec_complete_su_per10pp),
      bjs_3sys = relevel(factor(bjs_3sys), ref = "Centralized_ME"),
      bjs_4sys = relevel(factor(bjs_4sys), ref = "Centralized_ME"),
      cdc_3sys_txmixed = relevel(factor(cdc_3sys_txmixed), ref = "Centralized_ME")
    )
}

fit_glmer <- function(formula, data) {
  glmer(
    formula,
    data = data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
  )
}

fit_glm_fixed <- function(formula, data) {
  glm(formula, data = data, family = binomial(link = "logit"))
}

model_stats <- function(model, label = NA_character_, base_tau2 = NA_real_) {
  tau2 <- as.numeric(VarCorr(model)$state[1, 1])
  icc <- tau2 / (tau2 + (pi^2 / 3))
  mor <- exp(0.6745 * sqrt(2 * tau2))
  reduction <- ifelse(is.na(base_tau2), NA_real_, 100 * (base_tau2 - tau2) / base_tau2)
  mf <- model.frame(model)
  tibble(
    model = label,
    included_strata = nrow(mf),
    represented_jurisdictions = dplyr::n_distinct(mf$state),
    tau2 = tau2,
    icc = icc,
    mor = mor,
    reduction_tau2_pct = reduction,
    logLik = as.numeric(logLik(model)),
    AIC = AIC(model),
    singular_fit = isSingular(model, tol = 1e-4)
  )
}

lrt_random_intercept <- function(mixed_model, fixed_formula, data) {
  fixed_model <- fit_glm_fixed(fixed_formula, data)
  lr <- 2 * (as.numeric(logLik(mixed_model)) - as.numeric(logLik(fixed_model)))
  df <- attr(logLik(mixed_model), "df") - attr(logLik(fixed_model), "df")
  p <- pchisq(lr, df = df, lower.tail = FALSE)
  tibble(lrt_chisq = lr, lrt_df = df, lrt_p = p, lrt_p_formatted = format_p(p))
}

fixed_effects <- function(model) {
  co <- as.data.frame(coef(summary(model)))
  co$term <- rownames(co)
  rownames(co) <- NULL
  names(co) <- clean_colnames(names(co))
  co %>%
    transmute(
      term,
      estimate = estimate,
      std_error = std_error,
      z_value = z_value,
      p_value = pr_z,
      aor = exp(estimate),
      ci_low = exp(estimate - 1.96 * std_error),
      ci_high = exp(estimate + 1.96 * std_error),
      aor_95ci = fmt_aor(aor, ci_low, ci_high)
    )
}

try_mor_profile_ci <- function(model) {
  out <- tryCatch({
    ci <- suppressMessages(confint(model, parm = "theta_", method = "profile", oldNames = FALSE))
    sd_low <- as.numeric(ci[1, 1])
    sd_high <- as.numeric(ci[1, 2])
    tau2_low <- sd_low^2
    tau2_high <- sd_high^2
    tibble(
      tau2_ci_low = tau2_low,
      tau2_ci_high = tau2_high,
      mor_ci_low = exp(0.6745 * sqrt(2 * tau2_low)),
      mor_ci_high = exp(0.6745 * sqrt(2 * tau2_high))
    )
  }, error = function(e) {
    tibble(tau2_ci_low = NA_real_, tau2_ci_high = NA_real_, mor_ci_low = NA_real_, mor_ci_high = NA_real_)
  })
  out
}

proxy_aor_string <- function(model, pattern) {
  fx <- fixed_effects(model) %>% filter(str_detect(term, pattern))
  if (nrow(fx) == 0) return(NA_character_)
  paste0(fx$term, ": ", fx$aor_95ci, collapse = "; ")
}

all_data <- read_clean("Model_Data_All_Proxies_R") %>% prep_model_data() %>% add_proxy_variables()
data_2019 <- read_clean("Model_2019_R") %>% prep_model_data() %>% add_proxy_variables()

combined_model <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + period + (1 | state), all_data)
combined_lrt <- lrt_random_intercept(combined_model, cbind(suicide, undetermined) ~ sex + age_group + period, all_data)
combined_stats <- model_stats(combined_model, "Combined 2007-2024 model") %>% bind_cols(combined_lrt)
combined_mor_ci <- try_mor_profile_ci(combined_model)
combined_stats <- bind_cols(combined_stats, combined_mor_ci)
combined_fixed <- fixed_effects(combined_model) %>% mutate(model = "Combined 2007-2024 model") %>% select(model, everything())

period_models <- lapply(levels(droplevels(all_data$period)), function(p) {
  d <- all_data %>% filter(period == p) %>% droplevels()
  m <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + (1 | state), d)
  lrt <- lrt_random_intercept(m, cbind(suicide, undetermined) ~ sex + age_group, d)
  list(period = p, data = d, model = m, stats = model_stats(m, paste0("Period-specific model ", p)) %>% bind_cols(lrt), fixed = fixed_effects(m) %>% mutate(model = paste0("Period-specific model ", p)))
})
period_stats <- bind_rows(lapply(period_models, `[[`, "stats"))
period_fixed <- bind_rows(lapply(period_models, `[[`, "fixed")) %>% select(model, everything())

table2_fixed_effects <- combined_fixed %>%
  filter(term != "(Intercept)") %>%
  mutate(
    manuscript_term = case_when(
      term == "sexMale" ~ "Male vs female",
      term == "age_group35-54" ~ "Age 35-54 vs 15-34",
      term == "age_group≥55" ~ "Age ≥55 vs 15-34",
      term == "period2013-2018" ~ "2013-2018 vs 2007-2012",
      term == "period2019-2024" ~ "2019-2024 vs 2007-2012",
      TRUE ~ term
    )
  ) %>%
  select(manuscript_term, aor, ci_low, ci_high, aor_95ci, p_value)

table2_heterogeneity <- bind_rows(combined_stats, period_stats) %>%
  transmute(
    model,
    included_strata,
    represented_jurisdictions,
    tau2,
    icc,
    mor,
    mor_ci_low,
    mor_ci_high,
    lrt_chisq,
    lrt_df,
    lrt_p,
    lrt_p_formatted,
    singular_fit
  )

base_2019 <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + (1 | state), data_2019)
base_2019_tau2 <- model_stats(base_2019)$tau2

proxy_models <- list(
  "Base 2019-2024 model" = base_2019,
  "+ MDI system type" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + bjs_3sys + (1 | state), data_2019),
  "+ Drug-specification completeness" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + drug_spec_complete_all_drug_per10pp + (1 | state), data_2019),
  "+ Autopsy proportion" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + autopsy_prop_all_drug_per10pp + (1 | state), data_2019),
  "+ Autopsy proportion + drug-specification completeness" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + autopsy_prop_all_drug_per10pp + drug_spec_complete_all_drug_per10pp + (1 | state), data_2019),
  "+ MDI system type + autopsy proportion + drug-specification completeness" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + bjs_3sys + autopsy_prop_all_drug_per10pp + drug_spec_complete_all_drug_per10pp + (1 | state), data_2019)
)

table3_main <- bind_rows(lapply(names(proxy_models), function(nm) model_stats(proxy_models[[nm]], nm, base_2019_tau2))) %>%
  transmute(
    model,
    tau2,
    mor,
    reduction_tau2_pct
  )

s7_proxy_models <- bind_rows(lapply(names(proxy_models), function(nm) {
  m <- proxy_models[[nm]]
  pattern <- case_when(
    nm == "+ MDI system type" ~ "^bjs_3sys",
    nm == "+ Drug-specification completeness" ~ "drug_spec_complete_all_drug_per10pp",
    nm == "+ Autopsy proportion" ~ "autopsy_prop_all_drug_per10pp",
    nm == "+ Autopsy proportion + drug-specification completeness" ~ "autopsy_prop_all_drug_per10pp|drug_spec_complete_all_drug_per10pp",
    nm == "+ MDI system type + autopsy proportion + drug-specification completeness" ~ "^bjs_3sys|autopsy_prop_all_drug_per10pp|drug_spec_complete_all_drug_per10pp",
    TRUE ~ "^$"
  )
  model_stats(m, nm, base_2019_tau2) %>% mutate(proxy_aor_95ci = proxy_aor_string(m, pattern))
}))

bjs4_model <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + bjs_4sys + (1 | state), data_2019)
cdc3_model <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + cdc_3sys_txmixed + (1 | state), data_2019)

s6_mdi_fixed_effects <- fixed_effects(proxy_models[["+ MDI system type"]]) %>%
  filter(str_detect(term, "^bjs_3sys")) %>%
  mutate(
    mdi_system_type = case_when(
      term == "bjs_3sysCoroner_or_Mixed" ~ "Coroner/mixed/other local system",
      term == "bjs_3sysDecentralized_ME_only" ~ "Decentralized medical-examiner-only system",
      TRUE ~ term
    )
  ) %>%
  select(mdi_system_type, aor, ci_low, ci_high, aor_95ci, p_value)

s6_mdi_fixed_effects <- bind_rows(
  tibble(mdi_system_type = "Centralized statewide medical examiner system", aor = 1, ci_low = NA_real_, ci_high = NA_real_, aor_95ci = "Reference", p_value = NA_real_),
  s6_mdi_fixed_effects
)

s6_mdi_summary <- bind_rows(
  model_stats(base_2019, "Base 2019-2024 model (no MDI)", base_2019_tau2),
  model_stats(proxy_models[["+ MDI system type"]], "BJS 3-category model (primary)", base_2019_tau2),
  model_stats(bjs4_model, "BJS 4-category coding", base_2019_tau2),
  model_stats(cdc3_model, "CDC-informed 3-category recode", base_2019_tau2)
) %>%
  select(model, included_strata, represented_jurisdictions, tau2, icc, mor, reduction_tau2_pct, singular_fit)

s9_models <- list(
  "Base 2019-2024 model" = base_2019,
  "+ Drug-specification completeness using S+U denominator" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + drug_spec_complete_su_per10pp + (1 | state), data_2019),
  "+ Autopsy proportion using S+U denominator" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + autopsy_prop_su_per10pp + (1 | state), data_2019),
  "+ Both certificate-quality proxies using S+U denominator" = fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + autopsy_prop_su_per10pp + drug_spec_complete_su_per10pp + (1 | state), data_2019)
)

s9_proxy_denominator_sensitivity <- bind_rows(lapply(names(s9_models), function(nm) model_stats(s9_models[[nm]], nm, base_2019_tau2))) %>%
  mutate(proxy_aor_95ci = vapply(names(s9_models), function(nm) {
    pattern <- case_when(
      str_detect(nm, "Drug-specification") ~ "drug_spec_complete_su_per10pp",
      str_detect(nm, "Autopsy proportion") ~ "autopsy_prop_su_per10pp",
      str_detect(nm, "Both") ~ "autopsy_prop_su_per10pp|drug_spec_complete_su_per10pp",
      TRUE ~ "^$"
    )
    proxy_aor_string(s9_models[[nm]], pattern)
  }, character(1)))

all_no_md <- all_data %>% filter(state != "Maryland") %>% droplevels()
data_2019_no_md <- data_2019 %>% filter(state != "Maryland") %>% droplevels()
combined_no_md <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + period + (1 | state), all_no_md)
combined_no_md_lrt <- lrt_random_intercept(combined_no_md, cbind(suicide, undetermined) ~ sex + age_group + period, all_no_md)
model_2019_no_md <- fit_glmer(cbind(suicide, undetermined) ~ sex + age_group + (1 | state), data_2019_no_md)
model_2019_no_md_lrt <- lrt_random_intercept(model_2019_no_md, cbind(suicide, undetermined) ~ sex + age_group, data_2019_no_md)

s10_maryland_exclusion <- bind_rows(
  model_stats(combined_model, "Combined model: all jurisdictions") %>% bind_cols(combined_lrt),
  model_stats(combined_no_md, "Combined model: Maryland excluded") %>% bind_cols(combined_no_md_lrt),
  model_stats(base_2019, "2019-2024 model: all jurisdictions") %>% bind_cols(lrt_random_intercept(base_2019, cbind(suicide, undetermined) ~ sex + age_group, data_2019)),
  model_stats(model_2019_no_md, "2019-2024 model: Maryland excluded") %>% bind_cols(model_2019_no_md_lrt)
) %>%
  select(model, included_strata, represented_jurisdictions, tau2, icc, mor, lrt_chisq, lrt_df, lrt_p, lrt_p_formatted, singular_fit)

run_drug_restricted <- function() {
  sheets <- readxl::excel_sheets(input_file)
  candidates <- sheets[str_detect(tolower(sheets), "drug.*restrict|s11")]
  if (length(candidates) == 0) {
    return(tibble(note = "No drug-restricted analytic sheet found in Analytic_Model_Data.xlsx. Add a sheet named S11_Drug_Restricted or Drug_Restricted with columns: drug_subset, period, state, suicide, undetermined, use_in_model or display_status, and optional sex/age_group for model-based analyses."))
  }
  d <- read_clean(candidates[1])
  if (!all(c("drug_subset", "period", "state", "suicide", "undetermined") %in% names(d))) {
    return(tibble(note = paste0("Drug-restricted sheet found (", candidates[1], ") but required columns are missing.")))
  }
  d <- d %>%
    mutate(
      suicide = as_num(suicide),
      undetermined = as_num(undetermined),
      denominator = suicide + undetermined,
      csp = suicide / denominator,
      period = as.character(period),
      state = as.character(state),
      drug_subset = as.character(drug_subset)
    ) %>%
    filter(!is.na(suicide), !is.na(undetermined), denominator > 0)
  d %>%
    group_by(drug_subset, period) %>%
    summarise(
      jurisdictions_displayed = n_distinct(state),
      suicide_deaths = sum(suicide, na.rm = TRUE),
      undetermined_deaths = sum(undetermined, na.rm = TRUE),
      suicide_plus_undetermined = sum(denominator, na.rm = TRUE),
      national_csp = suicide_deaths / suicide_plus_undetermined,
      csp_min = min(csp, na.rm = TRUE),
      csp_max = max(csp, na.rm = TRUE),
      p90_p10_spread = as.numeric(quantile(csp, 0.90, na.rm = TRUE) - quantile(csp, 0.10, na.rm = TRUE)),
      .groups = "drop"
    )
}

s11_drug_restricted <- run_drug_restricted()

outputs <- list(
  Table2_Fixed_Effects = table2_fixed_effects,
  Table2_Heterogeneity = table2_heterogeneity,
  Table3_Main = table3_main,
  S6_MDI_Fixed_Effects = s6_mdi_fixed_effects,
  S6_MDI_Summary = s6_mdi_summary,
  S7_Proxy_Models = s7_proxy_models,
  S9_Proxy_Denominator_Sensitivity = s9_proxy_denominator_sensitivity,
  S10_Maryland_Exclusion = s10_maryland_exclusion,
  S11_Drug_Restricted = s11_drug_restricted
)

writexl::write_xlsx(outputs, file.path(output_dir, "Main_Model_and_Proxy_Analysis_Results.xlsx"))
saveRDS(
  list(
    combined_model = combined_model,
    period_models = lapply(period_models, `[[`, "model"),
    proxy_models = proxy_models,
    bjs4_model = bjs4_model,
    cdc3_model = cdc3_model,
    s9_models = s9_models,
    combined_no_md = combined_no_md,
    model_2019_no_md = model_2019_no_md
  ),
  file.path(output_dir, "Main_Model_and_Proxy_Model_Objects.rds")
)

message("Analysis complete. Outputs written to: ", normalizePath(output_dir))
message("Primary results workbook: ", normalizePath(file.path(output_dir, "Main_Model_and_Proxy_Analysis_Results.xlsx")))
message("Model objects: ", normalizePath(file.path(output_dir, "Main_Model_and_Proxy_Model_Objects.rds")))
