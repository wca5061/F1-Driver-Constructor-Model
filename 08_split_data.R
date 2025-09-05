# 08_split_data.R
# Split F1 dataset into train / test / validation

library(tidyverse)

# Load the prepped data
f1_dat <- read_rds("dat/f1_dat.rds") %>% filter(finished)

# Set a seed for reproducibility
set.seed(42)

# Shuffle rows
f1_dat <- f1_dat %>% slice_sample(prop = 1)

# Compute split sizes (70/20/10)
n <- nrow(f1_dat)
n_train <- floor(0.7 * n)
n_test  <- floor(0.2 * n)
n_val   <- n - n_train - n_test  # remainder

# Create splits
train_set <- f1_dat[1:n_train, ]
test_set  <- f1_dat[(n_train + 1):(n_train + n_test), ]
val_set   <- f1_dat[(n_train + n_test + 1):n, ]

# Save splits into new folder
dir.create("split", showWarnings = FALSE)

# Optionally also save as CSVs
write_csv(train_set, "split/train.csv")
write_csv(test_set,  "split/test.csv")
write_csv(val_set,   "split/val.csv")

# Print confirmation
cat("Rows:", n, "\n",
    "Train:", nrow(train_set), "\n",
    "Test:", nrow(test_set), "\n",
    "Validation:", nrow(val_set), "\n")
