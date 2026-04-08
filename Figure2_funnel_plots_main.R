# Figure2_funnel_plots_main.R
# Main-manuscript funnel plots for Figure 2
# Input file: "funnel plot data.xlsx"
# Sheet: "Funnel_R_Input"

# ---------------------------
# 1) Load packages
# ---------------------------
required_packages <- c("readxl", "dplyr", "ggplot2", "ggrepel")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install these packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ---------------------------
# 2) File settings
# ---------------------------
input_file <- "funnel plot data.xlsx"
sheet_name <- "Funnel_R_Input"

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# ---------------------------
# 3) Read data
# ---------------------------
df <- read_excel(input_file, sheet = sheet_name)

required_cols <- c(
  "abbr",
  "period",
  "period_order",
  "suicide",
  "undetermined",
  "funnel_denominator",
  "conditional_suicide_proportion",
  "pooled_national_proportion"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df <- df %>%
  mutate(
    abbr = as.character(abbr),
    period = as.character(period),
    period_order = as.numeric(period_order),
    suicide = as.numeric(suicide),
    undetermined = as.numeric(undetermined),
    funnel_denominator = as.numeric(funnel_denominator),
    conditional_suicide_proportion = as.numeric(conditional_suicide_proportion),
    pooled_national_proportion = as.numeric(pooled_national_proportion)
  )

# ---------------------------
# 4) Basic checks
# ---------------------------
expected_periods <- c("2007-2012", "2013-2018", "2019-2024")

if (!all(expected_periods %in% unique(df$period))) {
  stop("The period column must contain: ", paste(expected_periods, collapse = ", "))
}

bad_denominator <- df %>%
  filter((suicide + undetermined) != funnel_denominator)

if (nrow(bad_denominator) > 0) {
  stop("Some rows have funnel_denominator different from suicide + undetermined.")
}

# ---------------------------
# 5) Helper functions
# ---------------------------
make_funnel_limits <- function(p, n_seq) {
  se <- sqrt(p * (1 - p) / n_seq)

  data.frame(
    n = n_seq,
    center = p,
    lower95 = pmax(0, p - 1.96 * se),
    upper95 = pmin(1, p + 1.96 * se),
    lower998 = pmax(0, p - 3.09 * se),
    upper998 = pmin(1, p + 3.09 * se)
  )
}

flag_outliers <- function(data) {
  data %>%
    group_by(period) %>%
    mutate(
      p = unique(pooled_national_proportion),
      se = sqrt(p * (1 - p) / funnel_denominator),
      lower998 = pmax(0, p - 3.09 * se),
      upper998 = pmin(1, p + 3.09 * se),
      outlier998 = conditional_suicide_proportion < lower998 |
                   conditional_suicide_proportion > upper998
    ) %>%
    ungroup()
}

plot_funnel_main <- function(data, selected_period, panel_letter) {

  df_sub <- data %>% filter(period == selected_period)
  p <- unique(df_sub$pooled_national_proportion)

  if (length(p) != 1) {
    stop("Expected exactly one pooled_national_proportion for period ", selected_period)
  }

  n_seq <- seq(
    from = min(df_sub$funnel_denominator, na.rm = TRUE),
    to   = max(df_sub$funnel_denominator, na.rm = TRUE),
    length.out = 500
  )

  limits <- make_funnel_limits(p, n_seq)

  # Main-manuscript labels only
  label_data <- df_sub %>%
    filter(abbr %in% c("MD", "DC", "CA", "FL", "TX", "MI"))

  ggplot(df_sub, aes(x = funnel_denominator, y = conditional_suicide_proportion)) +
    geom_point(aes(color = outlier_group), size = 2.5) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = abbr, color = outlier_group),
      size = 3,
      max.overlaps = 100,
      box.padding = 0.25,
      point.padding = 0.2,
      show.legend = FALSE
    ) +
    geom_line(data = limits, aes(x = n, y = center), linewidth = 0.8) +
    geom_line(data = limits, aes(x = n, y = lower95), linetype = "dashed", linewidth = 0.6) +
    geom_line(data = limits, aes(x = n, y = upper95), linetype = "dashed", linewidth = 0.6) +
    geom_line(data = limits, aes(x = n, y = lower998), linetype = "dotted", linewidth = 0.6) +
    geom_line(data = limits, aes(x = n, y = upper998), linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(
      values = c(
        "Non-outlier" = "black",
        "Other outlier" = "orange3",
        "Persistent outlier" = "red3"
      ),
      breaks = c("Persistent outlier", "Other outlier", "Non-outlier")
    ) +
    labs(
      title = panel_letter,
      x = "Suicide + undetermined deaths",
      y = "Conditional suicide proportion",
      color = NULL
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# ---------------------------
# 6) Flag outliers
# ---------------------------
df_flagged <- flag_outliers(df)

persistent_states <- df_flagged %>%
  group_by(abbr) %>%
  summarise(n_outlier = sum(outlier998, na.rm = TRUE), .groups = "drop") %>%
  filter(n_outlier == 3) %>%
  pull(abbr)

df_flagged <- df_flagged %>%
  mutate(
    outlier_group = case_when(
      abbr %in% persistent_states & outlier998 ~ "Persistent outlier",
      outlier998 ~ "Other outlier",
      TRUE ~ "Non-outlier"
    ),
    outlier_group = factor(
      outlier_group,
      levels = c("Non-outlier", "Other outlier", "Persistent outlier")
    )
  )

# ---------------------------
# 7) Create plots
# ---------------------------
p1_main <- plot_funnel_main(df_flagged, "2007-2012", "A")
p2_main <- plot_funnel_main(df_flagged, "2013-2018", "B")
p3_main <- plot_funnel_main(df_flagged, "2019-2024", "C")

# Optional preview
print(p1_main)
print(p2_main)
print(p3_main)

# ---------------------------
# 8) Save TIFF files
# ---------------------------
ggsave(
  "Figure2_A_2007_2012_main.tiff",
  plot = p1_main,
  width = 7, height = 6, dpi = 300, compression = "lzw"
)

ggsave(
  "Figure2_B_2013_2018_main.tiff",
  plot = p2_main,
  width = 7, height = 6, dpi = 300, compression = "lzw"
)

ggsave(
  "Figure2_C_2019_2024_main.tiff",
  plot = p3_main,
  width = 7, height = 6, dpi = 300, compression = "lzw"
)

message("Done. Main Figure 2 TIFF files saved in: ", getwd())
