# Phase 1 extension for van Kesteren & Bergkamp F1 model
# Adds: grid, num_stops, sc_laps_share (optional), wet_intensity, power_track
# Fits ROL with fixed effects + random slopes (constructor:power_track ; driver:wet_intensity)
# Timeframe: 2018–2023 (ground-effect era included)
# Output:
#   data/derived/f1_phase1_2018_2023.rds
#   stan/rol_extended.stan (auto-written)
#   fits/rol_extended_fit.rds
#   results/loo_phase1.rds
#   img/phase1_fig_driver_2021like.png, ... (figure analogues)
#
# Run from project root: Rscript code/phase1_extend.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(cmdstanr)
  library(posterior)
  library(loo)
  # For data fetch; if not installed, install.packages("ergastR")
  suppressWarnings(suppressMessages(library(ergastR)))
})

# ---------- Settings ----------
years <- 2018:2023
set.seed(42)

dirs <- c("data/derived", "stan", "fits", "results", "img")
walk(dirs, ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

# Helper: curated "power tracks" (high-speed, low-downforce sensitivity)
power_tracks <- c(
  "monza", "spa", "baku", "jeddah", "silverstone",
  "red_bull_ring", "montréal", "monza_it", "autodromo_nazionale_monza"
)

# Helper: map circuitId/name to a normalized key
norm_key <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace("^_|_$", "")
}

# ---------- 1) Pull core race results from Ergast ----------
message("Fetching results via ergastR…")

fetch_results <- function(yr) {
  # ergastR::results() returns a list; bind to tibble
  out <- try(ergastR::results(season = yr), silent = TRUE)
  if(inherits(out, "try-error") || length(out) == 0) return(tibble())
  as_tibble(out)
}

res_raw <- map_dfr(years, fetch_results)

stopifnot(nrow(res_raw) > 0)

# Keep key variables + derive IDs
res <- res_raw %>%
  transmute(
    season = as.integer(season),
    round  = as.integer(round),
    raceId = paste0(season, "_", round),
    circuit = norm_key(circuitId %||% circuitName),
    driver = norm_key(driverId %||% driverSurname %||% driverFamilyName),
    constructor = norm_key(constructorId %||% constructorName),
    grid = as.integer(grid),
    position = suppressWarnings(as.integer(position)),   # NA for DNFs etc.
    statusText = status,
    finished = !is.na(position),                         # simple finished flag
    laps = as.integer(laps)
  )

# ---------- 2) Pit stop counts (num_stops) ----------
message("Fetching pit stops via ergastR…")

fetch_pits <- function(yr) {
  out <- try(ergastR::pitStops(season = yr), silent = TRUE)
  if(inherits(out, "try-error") || length(out) == 0) return(tibble())
  as_tibble(out)
}

pits_raw <- map_dfr(years, fetch_pits)

pits <- pits_raw %>%
  transmute(
    season = as.integer(season),
    round  = as.integer(round),
    raceId = paste0(season, "_", round),
    driver = norm_key(driverId %||% driverSurname %||% driverFamilyName)
  ) %>%
  count(season, round, raceId, driver, name = "num_stops")

# ---------- 3) Wet intensity (refined) ----------
# If you already have a curated weather file from the parent repo,
# put it at data/external/weather_2014_2023.csv with columns:
#   season, round, raceId, wet_intensity (dry/light/heavy)
# Otherwise we heuristically set dry and leave room to fill later.
weather_path <- "data/external/weather_2014_2023.csv"
if (file.exists(weather_path)) {
  wet <- read_csv(weather_path, show_col_types = FALSE) %>%
    mutate(
      raceId = raceId %||% paste0(season, "_", round),
      wet_intensity = factor(wet_intensity, levels = c("dry","light","heavy"))
    ) %>%
    select(raceId, wet_intensity)
} else {
  warning("No curated weather file found at ", weather_path,
          ". Setting wet_intensity='dry' as a placeholder.")
  wet <- res %>%
    distinct(raceId) %>%
    mutate(wet_intensity = factor("dry", levels = c("dry","light","heavy")))
}

# ---------- 4) Power-track tag ----------
circuits <- res %>% distinct(circuit)
power_df <- circuits %>%
  mutate(power_track = norm_key(circuit) %in% power_tracks) %>%
  transmute(circuit, power_track = as.integer(power_track))

# ---------- 5) Safety car share (optional) ----------
# If you have a curated safety-car file:
# data/external/sc_laps_share_2014_2023.csv with columns: raceId, sc_laps_share in [0,1]
sc_path <- "data/external/sc_laps_share_2014_2023.csv"
if (file.exists(sc_path)) {
  sc <- read_csv(sc_path, show_col_types = FALSE) %>%
    transmute(raceId, sc_laps_share = pmin(pmax(sc_laps_share,0),1))
} else {
  warning("No safety-car share file found at ", sc_path, ". Using NA.")
  sc <- res %>% distinct(raceId) %>% mutate(sc_laps_share = NA_real_)
}

# ---------- 6) Join features ----------
dat <- res %>%
  left_join(pits, by = c("season","round","raceId","driver")) %>%
  left_join(wet,  by = "raceId") %>%
  left_join(power_df, by = "circuit") %>%
  left_join(sc,   by = "raceId") %>%
  mutate(
    num_stops = coalesce(num_stops, 0L),
    grid = coalesce(grid, 20L), # back of grid if missing
    wet_intensity = fct_expand(wet_intensity, c("dry","light","heavy")),
    power_track = as.integer(power_track %in% c(1L, TRUE)),
    # Keep only finishers for the ROL likelihood (as in parent paper)
    keep = finished
  ) %>%
  filter(keep) %>%
  arrange(season, round)

saveRDS(dat, "data/derived/f1_phase1_2018_2023.rds")
message("Saved engineered dataset: data/derived/f1_phase1_2018_2023.rds")

# ---------- 7) Prepare data for Rank-Ordered Logit ----------
# For each race, we need the *ordered* finishers and covariates per competitor.
# We build race-level lists for Stan.

races <- dat %>%
  group_by(raceId) %>%
  arrange(position, .by_group = TRUE) %>%
  summarise(
    season = first(season),
    round  = first(round),
    circuit = first(circuit),
    power_track = first(power_track),
    wet_intensity = first(as.integer(wet_intensity)), # 1=dry,2=light,3=heavy
    sc_laps_share = first(sc_laps_share),
    laps = first(laps),
    drivers = list(driver),
    constructors = list(constructor),
    grid = list(grid),
    num_stops = list(num_stops),
    positions = list(position),
    .groups = "drop"
  )

# Indexing for drivers/constructors
driver_levels <- sort(unique(dat$driver))
team_levels   <- sort(unique(dat$constructor))

idx_map <- function(x, levels) as.integer(factor(x, levels = levels))

stan_list <- local({
  R <- nrow(races)
  comp_list <- map2(races$drivers, races$constructors, ~{
    tibble(d = .x, t = .y)
  })
  N_r <- map_int(comp_list, nrow)

  # Flatten for covariates
  grid_flat      <- unlist(races$grid)
  stops_flat     <- unlist(races$num_stops)

  # For the rank-ordered logit we need, per race, the competitor order by finish
  # We'll pass the order implicitly (data is pre-ordered by position).

  list(
    R = R,
    M = length(driver_levels),
    Tm = length(team_levels),
    N_r = N_r,
    cum_N = c(0L, cumsum(N_r)),
    # competitor indices (flattened)
    comp_driver = idx_map(unlist(races$drivers), driver_levels),
    comp_team   = idx_map(unlist(races$constructors), team_levels),
    # covariates (flattened per competitor)
    grid = as.numeric(grid_flat),
    num_stops = as.numeric(stops_flat),
    # race-level covariates (aligned per race; expanded in Stan)
    wet_intensity = races$wet_intensity,   # 1/2/3
    power_track   = races$power_track,     # 0/1
    # optional
    sc_laps_share = ifelse(is.na(races$sc_laps_share), -1, races$sc_laps_share) # -1 => missing
  )
})

# ---------- 8) Write/compile Stan model with fixed + random slopes ----------
stan_code <- '
data {
  int<lower=1> R;                   // races
  int<lower=1> M;                   // drivers
  int<lower=1> Tm;                  // teams
  int<lower=1> N_r[R];              // competitors per race
  int<lower=0> cum_N[R+1];          // cumsum index
  int<lower=1,upper=M> comp_driver[cum_N[R+1]]; // flattened driver ids
  int<lower=1,upper=Tm> comp_team[cum_N[R+1]];  // flattened team ids
  vector[cum_N[R+1]] grid;          // grid position covariate (per competitor)
  vector[cum_N[R+1]] num_stops;     // pit-stop count (per competitor)
  int<lower=1,upper=3> wet_intensity[R];   // race-level (1=dry,2=light,3=heavy)
  int<lower=0,upper=1> power_track[R];     // race-level (0/1)
  vector[R] sc_laps_share;                 // [-1]=missing else [0,1]
}
transformed data {
  // One-hot wet indicators per race
  // wet1=baseline(dry); we include wet2(light), wet3(heavy) dummies
  vector[R] wet2;
  vector[R] wet3;
  for (r in 1:R) {
    wet2[r] = wet_intensity[r] == 2 ? 1 : 0;
    wet3[r] = wet_intensity[r] == 3 ? 1 : 0;
  }
}
parameters {
  // Random intercepts
  vector[M] theta_driver;              // long-run driver skill
  vector[Tm] theta_team;               // long-run constructor advantage

  // Seasonal/dynamic omitted for Phase 1 (keep parsimony)

  // Fixed effects (global)
  real beta_grid;
  real beta_stops;
  real beta_sc;                        // effect per share of SC laps (if available)

  // Race-level fixed effects
  real beta_wet2;                      // light vs dry
  real beta_wet3;                      // heavy vs dry
  real beta_power;                     // power track vs not

  // Random slopes:
  //   - constructor × power_track
  vector[Tm] b_team_power;             // team-specific power track sensitivity
  //   - driver × wet (one slope; heavy vs not as robust default)
  vector[M] b_driver_wet3;             // driver wet sensitivity (heavy)

  // Hyperpriors
  real<lower=0> sigma_driver;
  real<lower=0> sigma_team;
  real<lower=0> sigma_team_power;
  real<lower=0> sigma_driver_wet3;
}
model {
  // Priors
  theta_driver ~ normal(0, sigma_driver);
  theta_team   ~ normal(0, sigma_team);

  beta_grid  ~ normal(0, 0.5);
  beta_stops ~ normal(0, 0.5);
  beta_sc    ~ normal(0, 0.5);

  beta_wet2  ~ normal(0, 0.5);
  beta_wet3  ~ normal(0, 0.5);
  beta_power ~ normal(0, 0.5);

  b_team_power   ~ normal(0, sigma_team_power);
  b_driver_wet3  ~ normal(0, sigma_driver_wet3);

  sigma_driver ~ normal(0, 1);
  sigma_team   ~ normal(0, 1);
  sigma_team_power  ~ normal(0, 0.5);
  sigma_driver_wet3 ~ normal(0, 0.5);

  // Likelihood: Rank-Ordered Logit (Plackett-Luce form)
  // For each race r, competitors i are already ordered by finish in the data
  // Latent utility per competitor j in race r:
  // u = driver + team + X*beta + random slopes (team×power, driver×wet heavy)
  for (r in 1:R) {
    int s = cum_N[r] + 1;
    int e = cum_N[r+1];
    // Construct utilities
    vector[e - s + 1] u;
    for (idx in s:e) {
      int m = comp_driver[idx];
      int t = comp_team[idx];
      real sc_eff = sc_laps_share[r] < 0 ? 0 : sc_laps_share[r] * beta_sc;

      u[idx - s + 1] =
        theta_driver[m] +
        theta_team[t] +
        beta_grid  * grid[idx] +
        beta_stops * num_stops[idx] +
        beta_wet2  * wet2[r] +
        beta_wet3  * wet3[r] +
        beta_power * power_track[r] +
        b_team_power[t] * power_track[r] +
        b_driver_wet3[m] * wet3[r] +
        sc_eff;
    }

    // Plackett-Luce likelihood: product over ranks
    // p(y) = ∏_{i=1}^{n-1} exp(u_i) / sum_{j=i}^{n} exp(u_j)
    for (i in 1:(e - s)) {
      real num = u[i];
      real denom = log_sum_exp( segment(u, i, (e - s + 1) - i + 1) );
      target += num - denom;
    }
  }
}
generated quantities {
  // Pointwise log-likelihood per race (for LOO)
  vector[R] log_lik;
  for (r in 1:R) {
    int s = cum_N[r] + 1;
    int e = cum_N[r+1];
    real ll = 0;
    // Recompute wet indicators (not available here, so mirror from transformed data)
    // NOTE: transformed data is not accessible here, but we can infer via wet_intensity
    real wet2g = wet_intensity[r] == 2 ? 1 : 0;
    real wet3g = wet_intensity[r] == 3 ? 1 : 0;

    for (i in s:(e-1)) {
      real num = 0;
      real denom = negative_infinity();
      for (j in i:e) {
        int m = comp_driver[j];
        int t = comp_team[j];
        real sc_eff = sc_laps_share[r] < 0 ? 0 : sc_laps_share[r] * beta_sc;

        real uj =
          theta_driver[m] +
          theta_team[t] +
          beta_grid  * grid[j] +
          beta_stops * num_stops[j] +
          beta_wet2  * wet2g +
          beta_wet3  * wet3g +
          beta_power * power_track[r] +
          b_team_power[t] * power_track[r] +
          b_driver_wet3[m] * wet3g +
          sc_eff;

        if (j == i) num = uj;
        denom = log_sum_exp(denom, uj);
      }
      ll += num - denom;
    }
    log_lik[r] = ll;
  }
}
'

stan_file <- "stan/rol_extended.stan"
writeLines(stan_code, stan_file)
message("Wrote Stan model: ", stan_file)

# Compile
mod <- cmdstan_model(stan_file)

# ---------- 9) Fit model ----------
message("Sampling… (this may take a while)")
fit <- mod$sample(
  data = stan_list,
  chains = 4, parallel_chains = 4,
  iter_warmup = 1000, iter_sampling = 1000,
  adapt_delta = 0.9, max_treedepth = 12,
  refresh = 200
)

saveRDS(fit, "fits/rol_extended_fit.rds")
message("Saved fit: fits/rol_extended_fit.rds")

# ---------- 10) LOO ----------
draws <- rstan::read_stan_csv(fit$output_files()) # fallback if needed
# Prefer cmdstanr posterior:
ps <- as_draws_rvars(fit$draws())
log_lik <- fit$draws("log_lik", format = "matrix")
loo_res <- loo::loo(log_lik)
saveRDS(loo_res, "results/loo_phase1.rds")
message("Saved LOO: results/loo_phase1.rds")
print(loo_res)

# ---------- 11) Figure analogues ----------
# Driver skill = theta_driver; Constructor advantage = theta_team
summ <- fit$summary(variables = c("theta_driver","theta_team"))
drv_idx <- which(startsWith(summ$variable, "theta_driver"))
tm_idx  <- which(startsWith(summ$variable, "theta_team"))

drv_tab <- summ[drv_idx,] %>%
  mutate(driver = driver_levels[as.integer(str_extract(variable, "\\d+"))]) %>%
  transmute(driver, mean, q5 = q5, q95 = q95)

tm_tab <- summ[tm_idx,] %>%
  mutate(constructor = team_levels[as.integer(str_extract(variable, "\\d+"))]) %>%
  transmute(constructor, mean, q5 = q5, q95 = q95)

# Figure 4 analogue: Driver skill (overall, hybrid+ground era mix)
ggplot(drv_tab, aes(x = reorder(driver, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0) +
  coord_flip() +
  labs(x = NULL, y = "Driver skill (log-odds scale)",
       title = "Driver skill (Phase 1, 2018–2023)") +
  theme_minimal()
ggsave("img/phase1_fig_driver_skill.png", width = 7, height = 9, dpi = 150)

# Figure 6 analogue: Constructor advantage
ggplot(tm_tab, aes(x = reorder(constructor, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0) +
  coord_flip() +
  labs(x = NULL, y = "Constructor advantage (log-odds)",
       title = "Constructor advantage (Phase 1, 2018–2023)") +
  theme_minimal()
ggsave("img/phase1_fig_team_advantage.png", width = 7, height = 7, dpi = 150)

message("Saved figure analogues in img/")
# ---------- 12) Train/Val/Test splits for ML prep ----------
# Split by date proxy: season/round ordering
all_races <- dat %>% distinct(season, round, raceId) %>% arrange(season, round)
n <- nrow(all_races)
train_idx <- 1:floor(0.7 * n)
val_idx   <- (max(train_idx)+1):(max(train_idx)+floor(0.2*n))
test_idx  <- setdiff(1:n, c(train_idx, val_idx))

splits <- list(
  train = all_races$raceId[train_idx],
  val   = all_races$raceId[val_idx],
  test  = all_races$raceId[test_idx]
)

write_rds(splits, "data/derived/splits_70_20_10_by_time.rds")
message("Saved splits: data/derived/splits_70_20_10_by_time.rds")

invisible(TRUE)
