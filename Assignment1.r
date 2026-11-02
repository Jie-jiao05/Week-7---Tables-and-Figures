library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(ggplot2)
library(here)

raw_data <- read.csv(here("Data", "final_data.csv"))

# Preserve all years, including 2000, for the descriptive figure.
data <- raw_data
conflict <- data

# Armed conflict is already lagged in the input data, as supplied.
# Use it directly without shifting again or removing the first year.
table_data <- data |>
  mutate(
    conflict_group = factor(
      armed_conflict,
      levels = c(0, 1),
      labels = c("No conflict", "Conflict")
    )
  )


# Define variables 

outcomes <- c(
  "maternal_mortality",
  "under5_mortality",
  "infant_mortality",
  "neonatal_mortality"
)

covariates <- c(
  "gdp_1000",
  "oecd",
  "pop_dens",
  "urban",
  "age_dep",
  "male_edu",
  "temp",
  "rainfall_1000",
  "earthquake",
  "drought"
)

binary_vars <- c("oecd", "earthquake", "drought")

variables <- c(outcomes, covariates)

variable_labels <- c(
  "Maternal mortality",
  "Under-5 mortality",
  "Infant mortality",
  "Neonatal mortality",
  "GDP per capita (1,000 USD)",
  "OECD member",
  "Population density",
  "Urban population (%)",
  "Age dependency ratio",
  "Male education",
  "Temperature",
  "Rainfall (1,000 units)",
  "Earthquake",
  "Drought"
)


# 4. Define summary functions --------------------------------

summarize_variable <- function(x, name) {

  missing <- sum(is.na(x))
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(c("NA", missing))
  }

  if (name %in% binary_vars) {

    summary <- sprintf(
      "%d (%.1f%%)",
      sum(x == 1),
      100 * mean(x == 1)
    )

  } else if (name %in% outcomes) {

    summary <- sprintf(
      "%.1f [%.1f, %.1f]",
      median(x),
      quantile(x, 0.25),
      quantile(x, 0.75)
    )

  } else {

    summary <- sprintf(
      "%.1f (%.1f)",
      mean(x),
      sd(x)
    )
  }

  c(summary, missing)
}


summarize_group <- function(df, group) {

  result <- lapply(variables, function(v) {

    s <- summarize_variable(df[[v]], v)

    data.frame(
      Variable = v,
      Summary = s[1],
      Missing = as.integer(s[2])
    )

  }) |>
    bind_rows()

  names(result)[2:3] <- c(
    group,
    paste0(group, "_missing")
  )

  result
}


# 5. Generate Table 1 data -----------------------------------

overall <- summarize_group(table_data, "Overall")

no_conflict_summary <- table_data |>
  filter(conflict_group == "No conflict") |>
  summarize_group("No_conflict")

conflict_summary <- table_data |>
  filter(conflict_group == "Conflict") |>
  summarize_group("Conflict")

table1 <- overall |>
  left_join(no_conflict_summary, by = "Variable") |>
  left_join(conflict_summary, by = "Variable") |>
  mutate(
    Variable = variable_labels
  ) |>
  select(
    Variable,
    Overall,
    Overall_missing,
    No_conflict,
    No_conflict_missing,
    Conflict,
    Conflict_missing
  )


# 6. Format Table 1 ------------------------------------------

n_total <- nrow(table_data)

n_no <- sum(table_data$conflict_group == "No conflict", na.rm = TRUE)

n_yes <- sum(table_data$conflict_group == "Conflict", na.rm = TRUE)

ft <- flextable(table1) |>
  set_header_labels(
    Variable = "Variable",
    Overall = "Summary",
    Overall_missing = "Missing",
    No_conflict = "Summary",
    No_conflict_missing = "Missing",
    Conflict = "Summary",
    Conflict_missing = "Missing"
  ) |>
  add_header_row(
    values = c(
      "",
      paste0("Overall (n = ", n_total, ")"),
      paste0("No conflict (n = ", n_no, ")"),
      paste0("Conflict (n = ", n_yes, ")")
    ),
    colwidths = c(1, 2, 2, 2),
    top = TRUE
  ) |>
  theme_booktabs() |>
  bold(part = "header") |>
  align(
    j = 2:7,
    align = "center",
    part = "all"
  ) |>
  fontsize(size = 9, part = "all") |>
  autofit()


# 7. Add notes -----------------------------------------------

ft <- ft |>
  add_footer_lines(
    values = paste(
      "Mortality outcomes: median [Q1, Q3].",
      "Other continuous variables: mean (SD).",
      "Binary variables: n (%).",
      "Primary outcomes: Maternal mortality, Under-5 mortality, Infant mortality, Neonatal mortality.",
      "Conflict status uses the already-lagged input variable."
    )
  )


# 8. Export Table 1 ------------------------------------------

save_as_docx(
  "Table 1. Study Characteristics by Armed Conflict" = ft,
  path = "Table1_Armed_Conflict.docx"
)

# Preview
ft

