# ---- setup ----
library(f1dataR)
library(dplyr)
library(janitor)
library(readr)   # only if you want to write CSVs

YEARS <- 2014:2021
KEEP_ONLY_FINISHERS <- TRUE  # set FALSE if you want everyone kept

# ---- build schedule table (normalized types) ----
message("Loading schedule…")
sched_all <- YEARS |>
  lapply(function(y) {
    f1dataR::load_schedule(y) |>
      clean_names() |>
      mutate(
        season = as.integer(season),
        round  = as.integer(round),
        date   = as.Date(date)   # your 'date' is a yyyy-mm-dd string
      ) |>
      select(season, round, race_name, circuit_id, date, country, locality)
  }) |>
  bind_rows() |>
  arrange(season, round)

# ---- build results, joined to schedule ----
message("Loading race results per season & round…")
res_list <- list()

for (y in YEARS) {
  # rounds present in schedule for that year
  rounds_y <- sched_all |> filter(season == y) |> pull(round)
  for (rd in rounds_y) {
    res <- f1dataR::load_results(y, rd) |> clean_names()
    if (!nrow(res)) next

    res_norm <- res |>
      mutate(
        # attach keys
        season = as.integer(y),
        round  = as.integer(rd),
        # coerce types that are coming as character on your system
        points        = suppressWarnings(as.numeric(points)),
        position      = suppressWarnings(as.integer(position)),
        grid          = suppressWarnings(as.integer(grid)),
        laps          = suppressWarnings(as.integer(laps)),
        fastest_rank  = suppressWarnings(as.integer(fastest_rank)),
        top_speed_kph = suppressWarnings(as.numeric(top_speed_kph)),
        time_sec      = suppressWarnings(as.numeric(time_sec))
      ) |>
      # keep consistent column set
      select(
        season, round, driver_id, constructor_id, position, points, grid, laps,
        status, gap, fastest_rank, fastest, top_speed_kph, time_sec
      ) |>
      # add schedule context (race_name/date/circuit/location)
      left_join(
        sched_all,
        by = c("season","round")
      ) |>
      relocate(season, round, race_name, date, country, locality, circuit_id)

    res_list[[length(res_list) + 1]] <- res_norm
  }
}

results_all <- bind_rows(res_list) |>
  arrange(season, round, position)

# ---- optional: mirror parent paper choice (keep only finishers) ----
if (KEEP_ONLY_FINISHERS) {
  results_all <- results_all |> filter(status == "Finished")
}

# ---- sanity checks ----
message(sprintf("Built %d rows across %d races.",
                nrow(results_all),
                nrow(distinct(results_all, season, round))))

# ---- 70/20/10 split by race (season+round) ----
set.seed(340)  # reproducible
race_keys <- results_all |> distinct(season, round) |> arrange(season, round)
n_races <- nrow(race_keys)
idx <- sample.int(n_races)

n_train <- floor(0.70 * n_races)
n_test  <- floor(0.20 * n_races)
n_val   <- n_races - n_train - n_test

train_idx <- idx[1:n_train]
test_idx  <- idx[(n_train + 1):(n_train + n_test)]
val_idx   <- idx[(n_train + n_test + 1):n_races]

split_map <- bind_rows(
  mutate(race_keys[train_idx, ], split = "train"),
  mutate(race_keys[test_idx,  ], split = "test"),
  mutate(race_keys[val_idx,   ], split = "val")
)

results_split <- results_all |>
  inner_join(split_map, by = c("season","round")) |>
  relocate(split, .before = driver_id)

# convenient objects
train <- filter(results_split, split == "train")
test  <- filter(results_split, split == "test")
val   <- filter(results_split, split == "val")

message(sprintf("Split rows — train: %d, test: %d, val: %d",
                nrow(train), nrow(test), nrow(val)))

# ---- (optional) save to disk ----
# write_csv(results_split, "f1_results_2014_2021_split.csv")
# saveRDS(results_split,   "f1_results_2014_2021_split.rds")

# Done. You now have `results_split`, plus `train`, `test`, and `val`.
