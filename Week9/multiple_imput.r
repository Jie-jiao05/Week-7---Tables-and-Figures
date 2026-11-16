
library(here)
library(plm)
library(flextable)
library(naniar)
library(dplyr)
# install.packages("mice")
# install.packages("naniar")
#install.packages("texreg")
library(mice)


finaldata <- read.csv(here("Data", "final_data.csv"))

midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

midata$loggdp <- log(midata$gdp_1000)

# mi setting

# Initialize the methods and predictor matrix without running imputation.
mice0 <- mice(
  midata,
  seed = 100,
  m = 10,
  maxit = 0,
  print = F
)

meth <- mice0$method
meth[c(
  "urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
  "maternal_mortality", "infant_mortality", "neonatal_mortality",
  "under5_mortality"
)] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c(
  "urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
  "maternal_mortality", "infant_mortality", "neonatal_mortality",
  "under5_mortality"
), "iso_num"] <- -2

mice.multi.out <- mice(
  midata,
  seed = 100,
  m = 10,
  maxit = 20,
  method = meth,
  predictorMatrix = pred
)

plot(mice.multi.out)


#from the slide for model

fit.mi.matmor <- with(
  mice.multi.out,
  lm(
    maternal_mortality ~ -1 +
      armed_conflict + loggdp + oecd + pop_dens + urban +
      age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
      as.factor(iso_num) + as.factor(year)
  )
)

out.mi.matmor <- pool(fit.mi.matmor)

summary(out.mi.matmor)

# Under-5 mortality
fit.mi.un5mor <- with(
  mice.multi.out,
  lm(
    under5_mortality ~ -1 +
      armed_conflict + loggdp + oecd + pop_dens + urban +
      age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
      as.factor(iso_num) + as.factor(year)
  )
)

out.mi.un5mor <- pool(fit.mi.un5mor)
# summary(out.mi.un5mor)

# Infant mortality
fit.mi.infmor <- with(
  mice.multi.out,
  lm(
    infant_mortality ~ -1 +
      armed_conflict + loggdp + oecd + pop_dens + urban +
      age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
      as.factor(iso_num) + as.factor(year)
  )
)

out.mi.infmor <- pool(fit.mi.infmor)
# summary(out.mi.infmor)

# Neonatal mortality
fit.mi.neomor <- with(
  mice.multi.out,
  lm(
    neonatal_mortality ~ -1 +
      armed_conflict + loggdp + oecd + pop_dens + urban +
      age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
      as.factor(iso_num) + as.factor(year)
  )
)

out.mi.neomor <- pool(fit.mi.neomor)
# summary(out.mi.neomor)



# for var selevtion
comparison_terms <- c(
  "armed_conflict", "loggdp", "oecd", "pop_dens", "urban", "age_dep",
  "male_edu", "temp", "rainfall_1000", "earthquake", "drought"
)

comparison_labels <- c(
  "Armed conflict", "Log GDP per capita", "OECD member", "Population density",
  "Urban population", "Age dependency ratio", "Male education", "Temperature",
  "Rainfall (1,000 units)", "Earthquake", "Drought"
)

comparison_outcomes <- c(
  Maternal = "maternal_mortality",
  `Under-5` = "under5_mortality",
  Infant = "infant_mortality",
  Neonatal = "neonatal_mortality"
)

mi_pooled_models <- list(
  Maternal = out.mi.matmor,
  `Under-5` = out.mi.un5mor,
  Infant = out.mi.infmor,
  Neonatal = out.mi.neomor
)


# Refit for the same Complete case 
cc_data <- finaldata |>
  mutate(loggdp = log(gdp_1000))

cc_comparison_models <- lapply(comparison_outcomes, function(outcome) {
  plm(
    reformulate(comparison_terms, response = outcome),
    data = cc_data,
    index = c("iso", "year"),
    model = "within",
    effect = "twoways",
    na.action = na.omit
  )
})

# isolate  coefficients and CI

comparison_results <- do.call(
  rbind,
  lapply(names(comparison_outcomes), function(outcome) {
    cc_model <- cc_comparison_models[[outcome]]
    cc_beta <- coef(cc_model)[comparison_terms]
    cc_se <- sqrt(diag(plm::vcovHC(
      cc_model, method = "arellano", type = "HC1", cluster = "group"
    )))[comparison_terms]
    n_clusters <- length(unique(plm::index(cc_model)[[1]]))
    cc_critical <- qt(0.975, df = n_clusters - 1L)

    mi_summary <- summary(mi_pooled_models[[outcome]], conf.int = TRUE)
    mi_rows <- mi_summary[match(comparison_terms, mi_summary$term), ]

    # mice versions differ in their confidence-limit column names.
    ci_names <- if (all(c("conf.low", "conf.high") %in% names(mi_rows))) {
      c("conf.low", "conf.high")
    } else {
      c("2.5 %", "97.5 %")
    }
    stopifnot(all(ci_names %in% names(mi_rows)))

    data.frame(
      Outcome = outcome,
      Predictor = comparison_labels,
      CC_estimate = unname(cc_beta),
      CC_lower = unname(cc_beta - cc_critical * cc_se),
      CC_upper = unname(cc_beta + cc_critical * cc_se),
      MI_estimate = mi_rows$estimate,
      MI_lower = mi_rows[[ci_names[1]]],
      MI_upper = mi_rows[[ci_names[2]]],
      row.names = NULL
    )
  })
)

# try to get a table

library(texreg)

# Supply the original 95% CIs so htmlreg does not replace them with normal CIs.(thanks codex)
# CC uses country-clustered CIs; MI uses pooled t intervals.
ci_lower <- list()
ci_upper <- list()

for (outcome in names(comparison_outcomes)) {
  cc_rows <- comparison_results[comparison_results$Outcome == outcome, ]
  mi_rows <- summary(mi_pooled_models[[outcome]])
  mi_critical <- qt(0.975, df = mi_rows$df)

  ci_lower <- c(
    ci_lower,
    list(cc_rows$CC_lower,
         mi_rows$estimate - mi_critical * mi_rows$std.error)
  )
  ci_upper <- c(
    ci_upper,
    list(cc_rows$CC_upper,
         mi_rows$estimate + mi_critical * mi_rows$std.error)
  )
}

htmlreg(
  list(
    cc_comparison_models$Maternal, out.mi.matmor,
    cc_comparison_models$`Under-5`, out.mi.un5mor,
    cc_comparison_models$Infant, out.mi.infmor,
    cc_comparison_models$Neonatal, out.mi.neomor
  ),
  custom.model.names = c(
    "Maternal CC", "Maternal MI",
    "Under-5 CC", "Under-5 MI",
    "Infant CC", "Infant MI",
    "Neonatal CC", "Neonatal MI"
  ),
  custom.coef.map = setNames(as.list(comparison_labels), comparison_terms),
  override.ci.low = ci_lower,
  override.ci.up = ci_upper,
  stars = numeric(0),
  ci.test = NULL,
  digits = 3,
  custom.note = paste(
    "Coefficients with 95% confidence intervals in brackets.",
    "Country and year fixed effects are included in every model but omitted from the table.",
    "CC: country-clustered HC1 intervals. MI: pooled model-based lm intervals.",
    "CC and MI interval widths therefore reflect different variance estimators as well as missing-data handling."
  ),
  file = here("Week9", "CC_MI_comparison_final.html")
)



