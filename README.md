# Home Credit Default Risk - Data Preparation

Comprehensive data preparation pipeline for predicting credit default risk.

## 📋 What It Does

The `data_preparation.R` script prepares loan application data through **5 automated steps**:

1. **Cleans data** - Fixes anomalies, removes constant features, caps outliers
2. **Engineers features** - Creates 65+ new features (demographics, ratios, interactions, bins)
3. **Aggregates supplementary data** - Combines bureau, previous apps, payment history
4. **Merges datasets** - Joins all sources at applicant level (SK_ID_CURR)
5. **Ensures consistency** - Prevents data leakage via training-only parameters

**Output**: Production-ready datasets with 265+ features for modeling.

## 🚀 How to Run

**Quick Start** (fully automated):
```r
install.packages("tidyverse")  # First time only
source("data_preparation.R")   # Processes everything
```

**Manual Control** (step-by-step):
```r
source("data_preparation.R")

# Clean → Engineer → Aggregate → Merge
train_clean <- clean_application_data(app_train, is_train = TRUE)$data
train_eng <- engineer_application_features(train_clean, is_train = TRUE)

bureau_agg <- aggregate_bureau_data()
prev_agg <- aggregate_previous_application()
install_agg <- aggregate_installments_payments()

train_final <- merge_supplementary_data(train_eng, bureau_agg, prev_agg, install_agg)
# Repeat for test with is_train = FALSE
```

## 📥 Inputs

Place these files in the project directory:
- `application_train.csv` (307,511 rows) - Training data
- `application_test.csv` (48,744 rows) - Test data
- `bureau.csv` - Credit bureau history
- `previous_application.csv` - Previous applications
- `installments_payments.csv` - Payment history

## 📤 Outputs

**Final datasets**:
- `application_train_final.csv` (307,511 × 265+ features) - Ready for modeling
- `application_test_final.csv` (48,744 × 265+ features) - Ready for predictions

**Also creates**: Cleaned, engineered, and aggregated intermediate files (9 total).

## ✅ Key Features

- ✓ Train/test consistency (no data leakage)
- ✓ Modular functions (use full pipeline or individual steps)
- ✓ Comprehensive features (demographics, financials, behavior, history)
- ✓ Progress logging
- ✓ Quality assured (see `QUALITY_REPORT.md`)

## 📚 Documentation

- `eda_report.html` - Exploratory analysis
- `train_test_consistency.R` - Verification tools