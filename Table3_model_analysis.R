library(readxl)
library(dplyr)
library(lme4)

# Import combined analytic file
dat <- read_excel("Table3 model.xlsx", sheet = "Model Data") %>%
  filter(`Use in model` == "Yes")

# Set reference categories
dat$Sex <- factor(dat$Sex, levels = c("Female", "Male"))
dat$`Age group` <- factor(dat$`Age group`, levels = c("15-34", "35-54", "55+"))
dat$Period <- factor(dat$Period, levels = c("2007-2012", "2013-2018", "2019-2024"))
dat$State <- factor(dat$State)

# Combined grouped binomial mixed-effects model
m1 <- glmer(
  cbind(Suicide, Undetermined) ~ Sex + `Age group` + Period + (1 | State),
  data = dat,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

# Fixed effects as adjusted odds ratios with Wald 95% confidence intervals
or <- exp(fixef(m1))
ci <- exp(confint(m1, parm = names(fixef(m1)), method = "Wald"))

results <- data.frame(
  Term = names(or),
  aOR = round(or, 2),
  CI_low = round(ci[, 1], 2),
  CI_high = round(ci[, 2], 2)
)

# Heterogeneity measures
tau2 <- as.numeric(VarCorr(m1)$State[1])
ICC <- tau2 / (tau2 + (pi^2 / 3))
MOR <- exp(0.6745 * sqrt(2 * tau2))

# Fixed-effects-only comparison model
m0 <- glm(
  cbind(Suicide, Undetermined) ~ Sex + `Age group` + Period,
  data = dat,
  family = binomial(link = "logit")
)

# Likelihood-ratio statistic
LR <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))

# Display outputs
summary(m1)
results
tau2
ICC
MOR
LR
nrow(dat)
length(unique(dat$State))
