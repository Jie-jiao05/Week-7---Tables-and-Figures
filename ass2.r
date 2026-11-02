library(here)
library(dplyr)
library(ggplot2)

raw_data <- read.csv(here("Data", "final_data.csv"))

# Use the original data, including the 2000 baseline.
conflict <- raw_data

maternal_trends <- conflict |>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L])

# Select countries with an increase from 2000 to 2017.
# 2017 is the last year with observed maternal mortality in this dataset.
increasing_countries <- maternal_trends |>
  filter(year == 2017, diff_matmor > 0) |>
  ungroup() |>
  select(iso)

# Keep every year for the selected countries.
figure_data <- maternal_trends |>
  ungroup() |>
  semi_join(increasing_countries, by = "iso")

maternal_figure <- ggplot(
  figure_data,
  aes(x = year, y = maternal_mortality, colour = iso, group = iso)
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = setNames(
    grDevices::hcl.colors(nrow(increasing_countries), "Dark 3"),
    increasing_countries$iso
  )) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2017)) +
  labs(
    title = "Trends in maternal mortality, 2000-2017",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality",
    colour = "Country (ISO code)"
  ) +
  theme_minimal(base_size = 12)

print(maternal_figure)
