# ==============================================================================
# TRAIN/TEST CONSISTENCY - COMPREHENSIVE GUIDE
# ==============================================================================
# 
# This document explains how train/test consistency is ensured in data_preparation.R
# 
# CURRENT IMPLEMENTATION:
# -----------------------
# The script uses a global TRAINING_STATS list and is_train flags in functions.
# 
# When is_train = TRUE (training data):
#   - Compute medians, percentiles, thresholds from data
#   - Store in TRAINING_STATS for reuse
# 
# When is_train = FALSE (test data):
#   - Retrieve stored values from TRAINING_STATS
#   - Apply same transformations as training
# 
# CONSISTENCY GUARANTEES:
# -----------------------
# 
# 1. IMPUTATION (impute_ext_source_missing):
#    - Train: Compute median for each EXT_SOURCE variable
#    - Test: Use stored training medians
#    - Ensures: Same imputation values across train/test
# 
# 2. NEAR-ZERO VARIANCE (remove_near_zero_variance):
#    - Train: Identify features with >95% same value
#    - Test: Remove the SAME features identified in training
#    - Ensures: Identical column sets
# 
# 3. OUTLIER CAPPING (cap_financial_outliers):
#    - Train: Compute 99th percentile caps
#    - Test: Use stored training caps
#    - Ensures: Same capping thresholds
# 
# 4. BINNING (create_binned_features):
#    - Train: Compute quartile thresholds
#    - Test: Use stored training thresholds
#    - Ensures: Consistent bin boundaries
# 
# 5. DAYS_EMPLOYED IMPUTATION (impute_days_employed):
#    - Train: Compute median from non-anomalous values
#    - Test: Use stored training median
#    - Ensures: Same imputation value
# 
# USAGE EXAMPLE:
# -------------
# 
# # Load and prepare training data
# app_train <- read_csv("application_train.csv")
# train_result <- clean_application_data(app_train, is_train = TRUE)
# app_train_clean <- train_result$data
# 
# # Load and prepare test data (uses stored TRAINING_STATS)
# app_test <- read_csv("application_test.csv")
# test_result <- clean_application_data(app_test, is_train = FALSE)
# app_test_clean <- test_result$data
# 
# # Verify consistency
# train_cols <- setdiff(names(app_train_clean), "TARGET")
# test_cols <- names(app_test_clean)
# identical(train_cols, test_cols)  # Should return TRUE
# 
# COLUMN VERIFICATION:
# -------------------
# 
# The final datasets should have:
# - Training: All features + TARGET
# - Test: All features (no TARGET)
# - Same feature order (except TARGET)
# - Same data types
# - Same factor levels
# 
# WHAT'S STORED IN TRAINING_STATS:
# ---------------------------------
# 
# - EXT_SOURCE_1_MEDIAN, EXT_SOURCE_2_MEDIAN, EXT_SOURCE_3_MEDIAN
# - DAYS_EMPLOYED_MEDIAN
# - AMT_INCOME_CAP, AMT_CREDIT_CAP
# - INCOME_BINS (quartiles)
# - CREDIT_BINS (quartiles)
# - DTI_BINS, PAYMENT_BURDEN_BINS thresholds
# - REMOVED_FEATURES (list of near-zero variance features)
# 
# ==============================================================================

cat("
===============================================================================
TRAIN/TEST CONSISTENCY VERIFICATION FUNCTION
===============================================================================
")

verify_train_test_consistency <- function(train_data, test_data) {
  cat("\nVerifying train/test consistency...\n\n")
  
  # Get column names (excluding TARGET)
  train_cols <- setdiff(names(train_data), "TARGET")
  test_cols <- names(test_data)
  
  # Check if columns match
  cols_match <- identical(sort(train_cols), sort(test_cols))
  
  cat(sprintf("Number of features in train (excl. TARGET): %d\n", length(train_cols)))
  cat(sprintf("Number of features in test: %d\n", length(test_cols)))
  cat(sprintf("Columns match: %s\n\n", ifelse(cols_match, "✓ YES", "✗ NO")))
  
  if (!cols_match) {
    cat("COLUMN MISMATCHES:\n")
    
    # Features in train but not test
    train_only <- setdiff(train_cols, test_cols)
    if (length(train_only) > 0) {
      cat(sprintf("\nIn train but not test (%d):\n", length(train_only)))
      cat(paste("  -", train_only, collapse = "\n"), "\n")
    }
    
    # Features in test but not train
    test_only <- setdiff(test_cols, train_cols)
    if (length(test_only) > 0) {
      cat(sprintf("\nIn test but not train (%d):\n", length(test_only)))
      cat(paste("  -", test_only, collapse = "\n"), "\n")
    }
  }
  
  # Check data types consistency for common columns
  common_cols <- intersect(train_cols, test_cols)
  type_mismatches <- c()
  
  for (col in common_cols) {
    train_type <- class(train_data[[col]])[1]
    test_type <- class(test_data[[col]])[1]
    if (train_type != test_type) {
      type_mismatches <- c(type_mismatches, 
                           sprintf("%s: train=%s, test=%s", col, train_type, test_type))
    }
  }
  
  if (length(type_mismatches) > 0) {
    cat("\nDATA TYPE MISMATCHES:\n")
    cat(paste("  -", type_mismatches, collapse = "\n"), "\n\n")
  } else {
    cat("Data types: ✓ All match\n\n")
  }
  
  # Check for NAs introduced differently
  cat("Missing value comparison (sample of features):\n")
  sample_features <- head(common_cols, 10)
  for (col in sample_features) {
    train_na_pct <- mean(is.na(train_data[[col]])) * 100
    test_na_pct <- mean(is.na(test_data[[col]])) * 100
    cat(sprintf("  %s: train=%.1f%%, test=%.1f%%\n", col, train_na_pct, test_na_pct))
  }
  
  cat("\n")
  return(list(
    columns_match = cols_match,
    train_only = setdiff(train_cols, test_cols),
    test_only = setdiff(test_cols, train_cols),
    type_mismatches = type_mismatches
  ))
}

cat("
Consistency verification function loaded.

Usage:
  result <- verify_train_test_consistency(app_train_final, app_test_final)

This will check:
  ✓ Column name matching
  ✓ Data type consistency  
  ✓ Missing value patterns
  ✓ Feature alignment

===============================================================================
")
