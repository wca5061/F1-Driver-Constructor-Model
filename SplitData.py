import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import os

# Set seed for reproducibility
np.random.seed(42)

# Load the dataset
dataset_path = os.path.join(os.path.expanduser('~'), 'Desktop', 'f1_dataset.csv')
print(f"Loading dataset from: {dataset_path}")
df = pd.read_csv(dataset_path)

print(f"Total dataset size: {len(df)} rows")
print(f"Dataset shape: {df.shape}")

# First split: 70% train, 30% temp (which will be split into 20% val, 10% test)
train_df, temp_df = train_test_split(df, test_size=0.3, random_state=42, shuffle=True)

# Second split: Split the 30% into 20% validation and 10% test
val_df, test_df = train_test_split(temp_df, test_size=0.3333, random_state=42, shuffle=True)

print(f"\nSplit sizes:")
print(f"Train set: {len(train_df)} rows ({len(train_df)/len(df)*100:.1f}%)")
print(f"Validation set: {len(val_df)} rows ({len(val_df)/len(df)*100:.1f}%)")
print(f"Test set: {len(test_df)} rows ({len(test_df)/len(df)*100:.1f}%)")

# Verify split adds up to total
print(f"\nTotal: {len(train_df) + len(val_df) + len(test_df)} rows")
print(f"Verification: {len(train_df) + len(val_df) + len(test_df) == len(df)}")

# Save the splits
desktop_path = os.path.join(os.path.expanduser('~'), 'Desktop')
train_path = os.path.join(desktop_path, 'f1_train.csv')
val_path = os.path.join(desktop_path, 'f1_validation.csv')
test_path = os.path.join(desktop_path, 'f1_test.csv')

train_df.to_csv(train_path, index=False)
val_df.to_csv(val_path, index=False)
test_df.to_csv(test_path, index=False)

print(f"\nFiles saved:")
print(f"Train set: {train_path}")
print(f"Validation set: {val_path}")
print(f"Test set: {test_path}")

# Show distribution statistics across splits
print("\n" + "="*50)
print("Distribution Analysis")
print("="*50)

print("\nSeasons distribution:")
print(f"Train: {train_df['season'].min()} - {train_df['season'].max()}")
print(f"Validation: {val_df['season'].min()} - {val_df['season'].max()}")
print(f"Test: {test_df['season'].min()} - {test_df['season'].max()}")

print("\nTop 5 drivers by count in each split:")
print("\nTrain set:")
print(train_df['driver_name'].value_counts().head())
print("\nValidation set:")
print(val_df['driver_name'].value_counts().head())
print("\nTest set:")
print(test_df['driver_name'].value_counts().head())

print("\nAverage points per split:")
print(f"Train: {train_df['points'].mean():.2f}")
print(f"Validation: {val_df['points'].mean():.2f}")
print(f"Test: {test_df['points'].mean():.2f}")

print("\nDNF rate per split:")
print(f"Train: {train_df['dnf'].mean()*100:.2f}%")
print(f"Validation: {val_df['dnf'].mean()*100:.2f}%")
print(f"Test: {test_df['dnf'].mean()*100:.2f}%")

print("\n✓ Data split complete!")