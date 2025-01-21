source("runtime.R")
library("ggplot2")
library("readr")
library("jsonlite")

my_mpa_json <- read_file("data/sketch.geojson.json")
return <- run_marlin(my_mpa_json)
print(return)
patch_dta <- fromJSON(return)

patch_dta %>%
  filter(year == 100) %>%
  ggplot(aes(x = metric, y = value, fill = scenario)) +
  facet_wrap(~ critter) +
  geom_col(position = "dodge") +
  theme_minimal()

patch_dta %>%
  filter(year >= 45, metric == "catch") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Catch Over Time") +
  theme_minimal()

patch_dta %>%
  filter(year >= 45, metric == "biomass") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Biomass Over Time") +
  theme_minimal()

patch_dta %>%
  filter(year >= 45, metric == "ssb") %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  facet_wrap(~ critter) +
  geom_line(size = 1) +
  labs(title = "Spawning Stock Biomass Over Time") +
  theme_minimal()

