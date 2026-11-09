# Run from the project folder. Packages: here, plm, flextable.
library(here)
library(plm)
library(flextable)

finaldata <- read.csv(here("Data", "final_data.csv"))

#MODEL SPECIFICATION


predictors <- c(
  "armed_conflict", "log(gdp_1000)", "oecd", "pop_dens",
  "urban", "age_dep", "male_edu", "temp",
  "rainfall_1000", "earthquake", "drought"
)

# FIXED EFFECTS REGRESSION
# log() applies the required natural-log transformation to GDP.
# The supplied armed_conflict variable is already lagged by one year.

plmmod_maternal <- plm(
  maternal_mortality ~ armed_conflict + log(gdp_1000) + oecd + pop_dens + urban +
    age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata,
  na.action = na.omit
)

plmmod_under5 <- plm(
  under5_mortality ~ armed_conflict + log(gdp_1000) + oecd + pop_dens + urban +
    age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata,
  na.action = na.omit
)

plmmod_infant <- plm(
  infant_mortality ~ armed_conflict + log(gdp_1000) + oecd + pop_dens + urban +
    age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata,
  na.action = na.omit
)

plmmod_neonatal <- plm(
  neonatal_mortality ~ armed_conflict + log(gdp_1000) + oecd + pop_dens + urban +
    age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
  index = c("iso", "year"),
  effect = "twoways",
  model = "within",
  data = finaldata,
  na.action = na.omit
)

# Collect the four fitted models for the results table.
models <- list(
  Maternal = plmmod_maternal,
  `Under-5` = plmmod_under5,
  Infant = plmmod_infant,
  Neonatal = plmmod_neonatal
)

# Country-clustered Huber-White covariance, allowing serial correlation.
# Each outcome uses its own complete-case sample.
robust_vcov <- lapply(models, function(m) {
  plm::vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
})

# Table 2: adjusted coefficients and 95% CIs, binary conflict models only.
labels <- c(
  "Armed conflict (yes vs no; previous year)",
  "Log GDP per capita (natural log)", "OECD member", "Population density",
  "Urban population", "Age dependency ratio", "Male education",
  "Temperature", "Rainfall (1,000 units)", "Earthquake", "Drought"
)
table2 <- data.frame(Variable = c(
  labels, "Country fixed effects", "Year fixed effects",
  "Country-year observations", "Countries", "Years"
), check.names = FALSE)

for (name in names(models)) {
  m <- models[[name]]
  beta <- coef(m)
  se <- sqrt(diag(robust_vcov[[name]]))
  idx <- plm::index(m)
  n_countries <- length(unique(idx[[1]]))
  # Use t critical values with number of country clusters minus one df.
  critical <- qt(0.975, df = n_countries - 1L)
  cells <- vapply(predictors, function(term) {
    if (!term %in% names(beta)) return("Not estimable")
    sprintf("%.2f (%.2f, %.2f)", beta[term],
            beta[term] - critical * se[term],
            beta[term] + critical * se[term])
  }, character(1))
  table2[[name]] <- c(
    cells, "Yes", "Yes", format(nobs(m), big.mark = ","),
    as.character(n_countries),
    paste(range(as.integer(as.character(idx[[2]]))), collapse = "-")
  )
}

ft <- flextable(table2) |>
  set_caption("Table 2. Armed conflict and mortality: binary specification") |>
  set_header_labels(Variable = "Predictor / model information",
                    Maternal = "Maternal mortality",
                    `Under-5` = "Under-5 mortality",
                    Infant = "Infant mortality", Neonatal = "Neonatal mortality") |>
  theme_booktabs() |>
  fontsize(size = 10, part = "all") |>
  bold(i = 1, part = "body") |>
  align(j = 2:5, align = "center", part = "all") |>
  add_footer_lines(values = c(
    "Values are adjusted beta coefficients (95% confidence intervals). Each column is a separate two-way fixed effects plm model.",
    "CIs use country-clustered Arellano/Huber-White HC1 standard errors and t critical values with countries minus one degrees of freedom.",
    "Maternal mortality is per 100,000 live births; child mortality outcomes are per 1,000 live births. Outcomes are not log-transformed.",
    "GDP predictor = ln(GDP per capita in thousands of USD). Conflict is already lagged one year. Missing values are excluded separately for each model.",
    "Source: Data/final_data.csv. Adapted from Jawad et al. (2021), doi:10.1371/journal.pmed.1003810; the GDP transformation and supplied sample can change estimates."
  )) |>
  fontsize(size = 9, part = "footer") |>
  autofit()

output_path <- here("Table2_Binary_Conflict.html")
save_as_html(ft, path = output_path, title = "Table 2: Binary armed conflict")
print(table2, row.names = FALSE)
message("Saved table: ", output_path)
