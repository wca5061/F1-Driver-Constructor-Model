# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-14 by @vankesteren
# Contents: Counterfactual predictions

library(tidyverse)
library(cmdstanr)
library(posterior)
# library(firatheme)  # REMOVED

fit    <- read_rds("fit/basic.rds")
f1_dat <- read_rds("dat/f1_dat.rds") |> filter(finished)

# In a race in 2020, how likely is it that Räikkönen in a Mercedes beats Hamilton in an Alfa?
# Note: season number = year - 2013 => 2020 -> 7

# Räikkönen-Mercedes: driver id = 7, team id = 1, season = 7
rai_merc <- fit$draws(c(
  "theta_driver[7]",
  "theta_driver_season[7,7]",
  "theta_team[1]",
  "theta_team_season[1,7]"
), format = "df") |>
  dplyr::select(dplyr::starts_with("theta")) |>
  rowSums()

# Hamilton-Alfa Romeo: driver id = 19, team id = 15, season = 7
ham_alfa <- fit$draws(c(
  "theta_driver[19]",
  "theta_driver_season[19,7]",
  "theta_team[15]",
  "theta_team_season[15,7]"
), format = "df") |>
  dplyr::select(dplyr::starts_with("theta")) |>
  rowSums()

prob <- exp(ham_alfa) / (exp(ham_alfa) + exp(rai_merc))

p <- ggplot(tibble(d = prob), aes(x = d)) +
  geom_density(fill = "steelblue", alpha = 0.8) +    # REPLACED firaCols[4]
  geom_vline(xintercept = mean(prob)) +
  geom_rug(alpha = 0.05) +
  theme_minimal(base_size = 12) +                    # REPLACED theme_fira()
  labs(
    title = "Counterfactual prediction",
    subtitle = "Hamilton in Alfa Romeo versus Räikkönen in Mercedes (2020 season)",
    x = "Posterior Hamilton–Alfa win probability",
    y = "Density"
  )

ggsave("img/plt_counterfactual.png", plot = p, width = 9, height = 4, bg = "white")
