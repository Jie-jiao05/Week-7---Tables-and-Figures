# Standalone polished version of Assignment1.r. Run from the project root.
library(dplyr)
library(flextable)
library(officer)
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


# Presentation: grouped headers, section bands, and quiet missing-value columns.
n_total <- nrow(table_data)
n_no <- sum(table_data$conflict_group == "No conflict", na.rm = TRUE)
n_yes <- sum(table_data$conflict_group == "Conflict", na.rm = TRUE)
format_n <- function(x) format(x, big.mark = ",", trim = TRUE)
section_row <- function(label) {
  row <- table1[1, ]
  row[] <- lapply(row, function(x) NA)
  row$Variable <- label
  row
}
display_table <- bind_rows(
  section_row("MORTALITY OUTCOMES"), table1[1:4, ],
  section_row("COVARIATES"), table1[5:nrow(table1), ]
)
navy <- "#203C50"
muted <- "#596875"
ft <- flextable(display_table) |>
  set_header_labels(
    Variable = "Characteristic",
    Overall = "Summary", Overall_missing = "Missing, n",
    No_conflict = "Summary", No_conflict_missing = "Missing, n",
    Conflict = "Summary", Conflict_missing = "Missing, n"
  ) |>
  add_header_row(values = c(
    "Characteristic", paste0("Overall\nN = ", format_n(n_total)),
    paste0("No conflict\nN = ", format_n(n_no)),
    paste0("Conflict\nN = ", format_n(n_yes))
  ), colwidths = c(1, 2, 2, 2)) |>
  merge_v(j = 1, part = "header") |>
  border_remove() |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 10, part = "all") |>
  color(color = navy, part = "all") |>
  bg(i = seq(2, nrow(display_table), by = 2), bg = "#F6F8FA", part = "body") |>
  bg(bg = navy, part = "header") |>
  color(color = "white", part = "header") |>
  bold(part = "header") |>
  align(j = 2:7, align = "center", part = "all") |>
  align(j = 1, align = "left", part = "all") |>
  valign(valign = "center", part = "all") |>
  color(j = c(3, 5, 7), color = muted, part = "body") |>
  fontsize(j = c(3, 5, 7), size = 9, part = "body") |>
  colformat_int(j = c(3, 5, 7), big.mark = ",", na_str = "") |>
  padding(padding = 6, part = "all") |>
  width(j = 1, width = 2.2) |>
  width(j = c(2, 4, 6), width = 1.7) |>
  width(j = c(3, 5, 7), width = 0.75) |>
  hline_bottom(border = fp_border(color = navy, width = 1), part = "body")
for (row in c(1, 6)) {
  ft <- ft |>
    merge_at(i = row, j = 1:7, part = "body") |>
    bg(i = row, bg = "#E5EDF3", part = "body") |>
    bold(i = row, part = "body") |>
    fontsize(i = row, size = 9, part = "body") |>
    align(i = row, align = "left", part = "body")
}
ft <- ft |>
  add_footer_lines(values = c(
    "Values are median [Q1, Q3] for mortality outcomes, mean (SD) for other continuous variables, and n (%) for binary variables.",
    "N denotes country-year observations, not unique countries. Missing, n reports missing values within each group; summaries and percentages use nonmissing values.",
    "Conflict status uses the already-lagged input variable, as supplied; no additional lag or first-year exclusion is applied. Source: final_data.csv."
  )) |>
  fontsize(size = 8.5, part = "footer") |>
  color(color = muted, part = "footer") |>
  align(align = "left", part = "footer") |>
  padding(padding.top = 4, padding.bottom = 3, part = "footer") |>
  set_table_properties(layout = "fixed", opts_word = list(split = FALSE))

# Export to this separate folder, leaving the original table unchanged.
output_dir <- here("polished_table")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
title <- "Table 1. Study characteristics by armed conflict"
subtitle <- paste0(
  min(table_data$year), "-", max(table_data$year), " | ", format_n(n_total),
  " country-year observations | ", n_distinct(table_data$iso), " countries"
)
doc <- read_docx() |>
  body_add_fpar(fpar(ftext(title, fp_text(
    font.size = 16, bold = TRUE, color = navy, font.family = "Arial"
  )))) |>
  body_add_fpar(fpar(ftext(subtitle, fp_text(
    font.size = 10, color = muted, font.family = "Arial"
  )))) |>
  body_add_flextable(ft) |>
  body_set_default_section(prop_section(
    page_size = page_size(width = 8.5, height = 11, orient = "landscape"),
    page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.65, right = 0.65)
  ))
print(doc, target = file.path(output_dir, "Table1_Polished.docx"))
save_as_html(ft, path = file.path(output_dir, "Table1_Polished.html"))
ft

# 8. Polished maternal mortality figures -----------------------------------
library(ggplot2)

# Preserve the assignment's pipeline and endpoint-based country selection.
conflict <- raw_data
maternal_trends <- conflict |>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L])

increasing_countries <- maternal_trends |>
  filter(year == 2017, diff_matmor > 0) |>
  ungroup() |>
  select(iso)
country_names <- c(
  BRN = "Brunei", CAN = "Canada", DOM = "Dominican Republic",
  HTI = "Haiti", JAM = "Jamaica", KWT = "Kuwait", LBN = "Lebanon",
  LBY = "Libya", LCA = "Saint Lucia", MUS = "Mauritius",
  SYR = "Syria", USA = "United States", VEN = "Venezuela"
)
figure_data <- maternal_trends |>
  ungroup() |>
  semi_join(increasing_countries, by = "iso") |>
  mutate(country = unname(country_names[iso]))

# Distinct colors, kept consistent across both figures.
country_colours <- setNames(c(
  "#0072B2", "#D55E00", "#009E73", "#233B53", "#CC79A7",
  "#8C564B", "#7A5195", "#00A6A6", "#E69F00", "#4B61D1",
  "#6B8E23", "#D1495B", "#888888"
), unname(country_names))
endpoints <- figure_data |>
  filter(year %in% c(2000, 2017))

figure_theme <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    text = element_text(colour = navy),
    plot.title = element_text(size = 21, face = "bold", margin = margin(b = 9)),
    plot.subtitle = element_text(size = 11, colour = muted, margin = margin(b = 18)),
    plot.caption = element_text(size = 9, colour = muted, hjust = 0, margin = margin(t = 14)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5EBEF", linewidth = 0.35),
    axis.title = element_text(size = 11),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.text = element_text(colour = muted),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = grid::unit(1.1, "cm"),
    plot.margin = margin(18, 22, 16, 18),
    plot.background = element_rect(fill = "white", colour = NA)
  )

maternal_figure <- ggplot(
  figure_data, aes(year, maternal_mortality, colour = country, group = country)
) +
  geom_line(linewidth = 0.95) +
  geom_point(data = endpoints, size = 2) +
  scale_colour_manual(values = country_colours) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2017),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.06))) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(
    title = "Maternal mortality increased in 13 countries",
    subtitle = "Annual trends, 2000-2017 | Countries selected by their net increase between the two endpoints",
    x = "Year", y = "Maternal mortality",
    caption = paste0(
      "Dots mark 2000 and 2017. An overall increase does not imply an increase every year.\n",
      "Source: final_data.csv. Maternal mortality is unavailable for 2018-2019."
    )
  ) +
  figure_theme

# Companion view: independent y-axis ranges reveal within-country variation.
# Use the combined figure above to compare absolute levels between countries.
maternal_panels <- ggplot(
  figure_data, aes(year, maternal_mortality, colour = country, group = country)
) +
  geom_line(linewidth = 0.85) +
  geom_point(data = endpoints, size = 1.8) +
  facet_wrap(vars(country), ncol = 4, scales = "free_y") +
  scale_colour_manual(values = country_colours) +
  scale_x_continuous(breaks = c(2000, 2010, 2017)) +
  scale_y_continuous(expand = expansion(mult = 0.15), n.breaks = 4) +
  labs(
    title = "A closer look at each country's trend",
    subtitle = "The same 13 countries | Each panel has its own y-axis scale; compare absolute levels using the combined figure",
    x = "Year", y = "Maternal mortality",
    caption = "Source: final_data.csv. Dots mark 2000 and 2017. Maternal mortality is unavailable for 2018-2019."
  ) +
  figure_theme +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", hjust = 0, size = 11, colour = navy),
    panel.spacing = grid::unit(1.2, "lines")
  )

# Save high-resolution PNGs and vector PDFs in the same separate folder.
ggsave(file.path(output_dir, "Figure_Maternal_Mortality.png"),
       maternal_figure, width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "Figure_Maternal_Mortality.pdf"),
       maternal_figure, width = 12, height = 8, bg = "white")
ggsave(file.path(output_dir, "Figure_Country_Panels.png"),
       maternal_panels, width = 13, height = 10, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "Figure_Country_Panels.pdf"),
       maternal_panels, width = 13, height = 10, bg = "white")
print(maternal_figure)
