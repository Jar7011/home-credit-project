# ==============================================================================
# HOME CREDIT DEFAULT RISK - DATA PREPARATION
# ==============================================================================
# 
# Purpose: Prepare and clean the application_train and application_test datasets
#          for modeling, addressing data quality issues identified in EDA
#
# Author: Data Analysis Team
# Date: February 15, 2026
# ==============================================================================

# Load required libraries
library(tidyverse)

# ==============================================================================
# TRAIN/TEST CONSISTENCY - GLOBAL STORAGE
# ==============================================================================

# Initialize global list to store training statistics
# This ensures test data uses the same transformation parameters as training
TRAINING_STATS <- list()

# ==============================================================================
# TRAIN/TEST CONSISTENCY FRAMEWORK
# ==============================================================================

#' Compute Transformation Parameters from Training Data
#' 
#' Calculates all parameters needed for data transformation from training data only
#' These parameters are stored and reused when processing test data
#' 
#' @param train_data Training dataset (uncleaned)
#' @return List of transformation parameters
compute_transformation_parameters <- function(train_data) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("COMPUTING TRANSFORMATION PARAMETERS FROM TRAINING DATA\n")
  cat(strrep("=", 80), "\n\n")
  
  params <- list()
  
  # === IMPUTATION PARAMETERS ===
  cat("Computing imputation parameters...\n")
  
  # EXT_SOURCE medians
  params$ext_source_1_median <- median(train_data$EXT_SOURCE_1, na.rm = TRUE)
  params$ext_source_2_median <- median(train_data$EXT_SOURCE_2, na.rm = TRUE)
  params$ext_source_3_median <- median(train_data$EXT_SOURCE_3, na.rm = TRUE)
  
  # DAYS_EMPLOYED median (excluding anomaly)
  params$days_employed_median <- median(
    train_data$DAYS_EMPLOYED[train_data$DAYS_EMPLOYED != 365243], 
    na.rm = TRUE
  )
  
  cat(sprintf("  - EXT_SOURCE_1 median: %.4f\n", params$ext_source_1_median))
  cat(sprintf("  - EXT_SOURCE_2 median: %.4f\n", params$ext_source_2_median))
  cat(sprintf("  - EXT_SOURCE_3 median: %.4f\n", params$ext_source_3_median))
  cat(sprintf("  - DAYS_EMPLOYED median: %.0f\n\n", params$days_employed_median))
  
  # === OUTLIER CAPPING THRESHOLDS ===
  cat("Computing outlier capping thresholds (99th percentile)...\n")
  
  params$amt_income_cap <- quantile(train_data$AMT_INCOME_TOTAL, 0.99, na.rm = TRUE)
  params$amt_credit_cap <- quantile(train_data$AMT_CREDIT, 0.99, na.rm = TRUE)
  params$cnt_children_max <- 10  # Hardcoded reasonable maximum
  
  cat(sprintf("  - AMT_INCOME_TOTAL cap: %.0f\n", params$amt_income_cap))
  cat(sprintf("  - AMT_CREDIT cap: %.0f\n", params$amt_credit_cap))
  cat(sprintf("  - CNT_CHILDREN max: %d\n\n", params$cnt_children_max))
  
  # === BINNING THRESHOLDS ===
  cat("Computing binning thresholds...\n")
  
  # Income bins
  params$income_q25 <- quantile(train_data$AMT_INCOME_TOTAL, 0.25, na.rm = TRUE)
  params$income_median <- quantile(train_data$AMT_INCOME_TOTAL, 0.50, na.rm = TRUE)
  params$income_q75 <- quantile(train_data$AMT_INCOME_TOTAL, 0.75, na.rm = TRUE)
  params$income_q90 <- quantile(train_data$AMT_INCOME_TOTAL, 0.90, na.rm = TRUE)
  
  # Credit bins
  params$credit_q25 <- quantile(train_data$AMT_CREDIT, 0.25, na.rm = TRUE)
  params$credit_median <- quantile(train_data$AMT_CREDIT, 0.50, na.rm = TRUE)
  params$credit_q75 <- quantile(train_data$AMT_CREDIT, 0.75, na.rm = TRUE)
  
  cat(sprintf("  - Income bins: %.0f, %.0f, %.0f, %.0f\n", 
              params$income_q25, params$income_median, params$income_q75, params$income_q90))
  cat(sprintf("  - Credit bins: %.0f, %.0f, %.0f\n\n", 
              params$credit_q25, params$credit_median, params$credit_q75))
  
  # === NEAR-ZERO VARIANCE FEATURES ===
  cat("Identifying near-zero variance features...\n")
  
  flag_cols <- names(train_data)[str_starts(names(train_data), "FLAG_")]
  nzv_features <- c()
  
  for (col in flag_cols) {
    if (is.numeric(train_data[[col]])) {
      prop_most_common <- max(table(train_data[[col]], useNA = "no")) / 
                         sum(!is.na(train_data[[col]]))
      if (prop_most_common > 0.95) {
        nzv_features <- c(nzv_features, col)
      }
    }
  }
  
  params$nzv_features <- nzv_features
  cat(sprintf("  - Identified %d near-zero variance features to remove\n\n", 
              length(nzv_features)))
  
  cat(strrep("=", 80), "\n")
  cat("TRANSFORMATION PARAMETERS STORED\n")
  cat(sprintf("Total parameters: %d\n", length(params)))
  cat(strrep("=", 80), "\n\n")
  
  return(params)
}


# ==============================================================================
# REUSABLE CLEANING FUNCTIONS (UPDATED FOR CONSISTENCY)
# ==============================================================================

#' Fix DAYS_EMPLOYED Anomaly
#' 
#' Handles the placeholder value 365243 in DAYS_EMPLOYED by creating a flag
#' variable and replacing the anomalous value with NA
#' 
#' @param data A dataframe containing DAYS_EMPLOYED column
#' @return Dataframe with DAYS_EMPLOYED_ANOMALY flag and cleaned DAYS_EMPLOYED
fix_days_employed_anomaly <- function(data) {
  cat("Fixing DAYS_EMPLOYED anomaly...\n")
  
  # Count anomalous values before
  n_anomalous <- sum(data$DAYS_EMPLOYED == 365243, na.rm = TRUE)
  pct_anomalous <- mean(data$DAYS_EMPLOYED == 365243, na.rm = TRUE) * 100
  
  cat(sprintf("  - Found %d anomalous values (%.2f%%)\n", 
              n_anomalous, pct_anomalous))
  
  # Create flag for anomalous employment status
  data <- data |>
    mutate(
      DAYS_EMPLOYED_ANOMALY = if_else(DAYS_EMPLOYED == 365243, 1, 0),
      DAYS_EMPLOYED = if_else(DAYS_EMPLOYED == 365243, NA_real_, DAYS_EMPLOYED)
    )
  
  cat("  - Created DAYS_EMPLOYED_ANOMALY flag\n")
  cat("  - Replaced anomalous values with NA\n\n")
  
  return(data)
}


#' Impute Missing Values in EXT_SOURCE Variables
#' 
#' Imputes missing values in EXT_SOURCE_1, EXT_SOURCE_2, and EXT_SOURCE_3
#' using median imputation and creates missing indicators
#' 
#' @param data A dataframe containing EXT_SOURCE columns
#' @param is_train Logical indicating if this is training data
#' @return Dataframe with imputed EXT_SOURCE variables and missing flags
impute_ext_source_missing <- function(data, is_train = TRUE) {
  cat("Imputing missing values in EXT_SOURCE variables...\n")
  
  ext_sources <- c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")
  
  for (var in ext_sources) {
    # Calculate missing percentage
    pct_missing <- mean(is.na(data[[var]])) * 100
    
    if (pct_missing > 0) {
      cat(sprintf("  - %s: %.2f%% missing\n", var, pct_missing))
      
      # Create missing indicator
      missing_flag <- paste0(var, "_MISSING")
      data[[missing_flag]] <- if_else(is.na(data[[var]]), 1, 0)
      
      # Compute or retrieve median
      median_key <- paste0(var, "_MEDIAN")
      if (is_train) {
        median_val <- median(data[[var]], na.rm = TRUE)
        TRAINING_STATS[[median_key]] <<- median_val
        cat(sprintf("    * Computed median: %.4f (saved for test)\n", median_val))
      } else {
        median_val <- TRAINING_STATS[[median_key]]
        cat(sprintf("    * Using training median: %.4f\n", median_val))
      }
      
      # Impute with median
      data[[var]] <- if_else(is.na(data[[var]]), median_val, data[[var]])
      cat(sprintf("    * Created %s flag\n", missing_flag))
    }
  }
  
  cat("\n")
  return(data)
}


#' Remove Near-Zero Variance Features
#' 
#' Removes features that have near-zero variance (>95% same value for binary,
#' or variance close to 0 for numeric)
#' 
#' @param data A dataframe
#' @param threshold Threshold for binary features (default 0.95)
#' @param is_train Logical indicating if this is training data
#' @return List with cleaned data and vector of removed features
remove_near_zero_variance <- function(data, threshold = 0.95, is_train = TRUE) {
  cat("Removing near-zero variance features...\n")
  
  if (is_train) {
    removed_features <- c()
    
    # Check binary FLAG_ variables
    flag_cols <- names(data)[str_starts(names(data), "FLAG_")]
    
    for (col in flag_cols) {
      if (is.numeric(data[[col]])) {
        # Calculate proportion of most common value
        prop_most_common <- max(table(data[[col]], useNA = "no")) / sum(!is.na(data[[col]]))
        
        if (prop_most_common > threshold) {
          removed_features <- c(removed_features, col)
        }
      }
    }
    
    # Store for test data
    TRAINING_STATS[["REMOVED_FEATURES"]] <<- removed_features
    
  } else {
    # Use training data's list
    removed_features <- TRAINING_STATS[["REMOVED_FEATURES"]]
    cat("  - Using training data's removal list\n")
  }
  
  # Remove identified features
  if (length(removed_features) > 0) {
    # Only remove features that exist in current dataset
    features_to_remove <- intersect(removed_features, names(data))
    if (length(features_to_remove) > 0) {
      data <- data |> select(-all_of(features_to_remove))
      cat(sprintf("  - Removed %d near-zero variance features\n", 
                  length(features_to_remove)))
      if (is_train) {
        cat(paste("    *", head(features_to_remove, 10), collapse = "\n"), "\n")
        if (length(features_to_remove) > 10) {
          cat(sprintf("    * ... and %d more\n", length(features_to_remove) - 10))
        }
      }
    }
  } else {
    cat("  - No near-zero variance features found\n")
  }
  cat("\n")
  
  return(list(
    data = data,
    removed_features = removed_features
  ))
}


#' Cap Outliers in Financial Variables
#' 
#' Caps extreme outliers at the 99th percentile for financial variables
#' 
#' @param data A dataframe
#' @param variables Vector of variable names to cap
#' @param percentile Upper percentile for capping (default 0.99)
#' @param is_train Logical indicating if this is training data
#' @return Dataframe with capped variables
cap_financial_outliers <- function(data, 
                                   variables = c("AMT_INCOME_TOTAL", "AMT_CREDIT"),
                                   percentile = 0.99,
                                   is_train = TRUE) {
  cat("Capping outliers in financial variables...\n")
  
  for (var in variables) {
    if (var %in% names(data)) {
      # Compute or retrieve cap value
      cap_key <- paste0(var, "_CAP")
      
      if (is_train) {
        cap_value <- quantile(data[[var]], percentile, na.rm = TRUE)
        TRAINING_STATS[[cap_key]] <<- cap_value
        
        # Count values above cap
        n_capped <- sum(data[[var]] > cap_value, na.rm = TRUE)
        
        if (n_capped > 0) {
          cat(sprintf("  - %s: capping %d values (%.2f%%) at %.0f (saved for test)\n", 
                      var, n_capped, 
                      n_capped / nrow(data) * 100,
                      cap_value))
          
          # Cap the values
          data[[var]] <- if_else(data[[var]] > cap_value, cap_value, data[[var]])
        }
      } else {
        cap_value <- TRAINING_STATS[[cap_key]]
        n_capped <- sum(data[[var]] > cap_value, na.rm = TRUE)
        
        if (n_capped > 0) {
          cat(sprintf("  - %s: capping %d values at training cap %.0f\n", 
                      var, n_capped, cap_value))
          data[[var]] <- if_else(data[[var]] > cap_value, cap_value, data[[var]])
        }
      }
    }
  }
  
  cat("\n")
  return(data)
}


#' Cap Children Count Extreme Values
#' 
#' Caps extreme children counts at a reasonable threshold
#' 
#' @param data A dataframe containing CNT_CHILDREN
#' @param max_children Maximum reasonable children count (default 10)
#' @return Dataframe with capped CNT_CHILDREN
cap_children_count <- function(data, max_children = 10) {
  cat("Capping extreme children counts...\n")
  
  n_capped <- sum(data$CNT_CHILDREN > max_children, na.rm = TRUE)
  
  if (n_capped > 0) {
    cat(sprintf("  - Capping %d observations with >%d children\n", 
                n_capped, max_children))
    
    data <- data |>
      mutate(CNT_CHILDREN = if_else(CNT_CHILDREN > max_children, 
                                     as.double(max_children), 
                                     as.double(CNT_CHILDREN)))
  } else {
    cat("  - No extreme children counts found\n")
  }
  
  cat("\n")
  return(data)
}


#' Impute Missing DAYS_EMPLOYED
#' 
#' Imputes missing DAYS_EMPLOYED (after anomaly removal) with median
#' 
#' @param data A dataframe containing DAYS_EMPLOYED
#' @param is_train Logical indicating if this is training data
#' @return Dataframe with imputed DAYS_EMPLOYED
impute_days_employed <- function(data, is_train = TRUE) {
  cat("Imputing missing DAYS_EMPLOYED values...\n")
  
  n_missing <- sum(is.na(data$DAYS_EMPLOYED))
  
  if (n_missing > 0) {
    if (is_train) {
      median_val <- median(data$DAYS_EMPLOYED, na.rm = TRUE)
      TRAINING_STATS[["DAYS_EMPLOYED_MEDIAN"]] <<- median_val
      cat(sprintf("  - Imputing %d missing values with median: %.0f (saved for test)\n", 
                  n_missing, median_val))
    } else {
      median_val <- TRAINING_STATS[["DAYS_EMPLOYED_MEDIAN"]]
      cat(sprintf("  - Imputing %d missing values with training median: %.0f\n", 
                  n_missing, median_val))
    }
    
    data <- data |>
      mutate(DAYS_EMPLOYED = if_else(is.na(DAYS_EMPLOYED), 
                                      median_val, 
                                      DAYS_EMPLOYED))
  } else {
    cat("  - No missing values to impute\n")
  }
  
  cat("\n")
  return(data)
}


#' Clean Application Data
#' 
#' Main function to apply all cleaning steps to application data
#' 
#' @param data Application train or test dataframe
#' @param is_train Logical indicating if this is training data
#' @return List with cleaned data and metadata about transformations
clean_application_data <- function(data, is_train = TRUE) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  if (is_train) {
    cat("CLEANING APPLICATION DATA (TRAINING)\n")
  } else {
    cat("CLEANING APPLICATION DATA (TEST - using training stats)\n")
  }
  cat(strrep("=", 80), "\n\n")
  
  original_cols <- ncol(data)
  
  # 1. Fix DAYS_EMPLOYED anomaly
  data <- fix_days_employed_anomaly(data)
  
  # 2. Impute EXT_SOURCE missing values
  data <- impute_ext_source_missing(data, is_train = is_train)
  
  # 3. Remove near-zero variance features
  nzv_result <- remove_near_zero_variance(data, is_train = is_train)
  data <- nzv_result$data
  removed_features <- nzv_result$removed_features
  
  # 4. Cap financial outliers
  data <- cap_financial_outliers(data, is_train = is_train)
  
  # 5. Cap children count
  data <- cap_children_count(data)
  
  # 6. Impute DAYS_EMPLOYED after anomaly removal
  data <- impute_days_employed(data, is_train = is_train)
  
  # Summary
  final_cols <- ncol(data)
  cat(strrep("=", 80), "\n")
  cat("CLEANING SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Original columns: %d\n", original_cols))
  cat(sprintf("Final columns: %d\n", final_cols))
  cat(sprintf("Features removed: %d\n", length(removed_features)))
  cat(sprintf("Features added: %d (flags and imputation indicators)\n", 
              final_cols - original_cols + length(removed_features)))
  cat("\n")
  
  return(list(
    data = data,
    removed_features = removed_features,
    n_original_cols = original_cols,
    n_final_cols = final_cols
  ))
}


# ==============================================================================
# FEATURE ENGINEERING FUNCTIONS
# ==============================================================================

#' Create Demographic Features
#' 
#' Converts DAYS_BIRTH and DAYS_EMPLOYED to more interpretable age and 
#' employment duration features
#' 
#' @param data A dataframe with DAYS_BIRTH and DAYS_EMPLOYED
#' @return Dataframe with new demographic features
create_demographic_features <- function(data) {
  cat("Creating demographic features...\n")
  
  data <- data |>
    mutate(
      # Age in years (DAYS_BIRTH is negative)
      AGE_YEARS = -DAYS_BIRTH / 365,
      
      # Employment duration in years (DAYS_EMPLOYED is negative)
      EMPLOYED_YEARS = -DAYS_EMPLOYED / 365,
      
      # Age categories
      AGE_GROUP = case_when(
        AGE_YEARS < 25 ~ "Under 25",
        AGE_YEARS < 35 ~ "25-34",
        AGE_YEARS < 45 ~ "35-44",
        AGE_YEARS < 55 ~ "45-54",
        AGE_YEARS < 65 ~ "55-64",
        TRUE ~ "65+"
      ),
      
      # Employment duration categories
      EMPLOYMENT_LENGTH_GROUP = case_when(
        EMPLOYED_YEARS < 1 ~ "Less than 1 year",
        EMPLOYED_YEARS < 3 ~ "1-3 years",
        EMPLOYED_YEARS < 5 ~ "3-5 years",
        EMPLOYED_YEARS < 10 ~ "5-10 years",
        TRUE ~ "10+ years"
      ),
      
      # Registration age (how long ago they registered)
      REGISTRATION_YEARS = -DAYS_REGISTRATION / 365,
      
      # ID publish age (how long ago ID was published)
      ID_PUBLISH_YEARS = -DAYS_ID_PUBLISH / 365,
      
      # Phone change recency (how long since last phone change)
      PHONE_CHANGE_YEARS = -DAYS_LAST_PHONE_CHANGE / 365
    )
  
  cat("  - Created AGE_YEARS and AGE_GROUP\n")
  cat("  - Created EMPLOYED_YEARS and EMPLOYMENT_LENGTH_GROUP\n")
  cat("  - Created REGISTRATION_YEARS, ID_PUBLISH_YEARS, PHONE_CHANGE_YEARS\n\n")
  
  return(data)
}


#' Create Financial Ratio Features
#' 
#' Creates various financial ratios commonly used in credit risk modeling:
#' - Debt-to-Income (DTI)
#' - Loan-to-Value (LTV)
#' - Payment-to-Income
#' - Credit utilization metrics
#' 
#' @param data A dataframe with financial columns
#' @return Dataframe with financial ratio features
create_financial_ratios <- function(data) {
  cat("Creating financial ratio features...\n")
  
  data <- data |>
    mutate(
      # === Core Credit Ratios ===
      
      # Credit-to-Income ratio (debt burden)
      CREDIT_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
      
      # Annuity-to-Income ratio (monthly payment burden)
      ANNUITY_INCOME_RATIO = AMT_ANNUITY / AMT_INCOME_TOTAL,
      
      # Loan-to-Value ratio (credit vs goods price)
      CREDIT_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
      
      # === Downpayment Analysis ===
      
      # Downpayment amount (goods price - credit)
      DOWNPAYMENT_AMOUNT = AMT_GOODS_PRICE - AMT_CREDIT,
      
      # Downpayment percentage
      DOWNPAYMENT_PCT = (AMT_GOODS_PRICE - AMT_CREDIT) / AMT_GOODS_PRICE,
      
      # === Income Adequacy Ratios ===
      
      # Income per family member
      INCOME_PER_PERSON = AMT_INCOME_TOTAL / CNT_FAM_MEMBERS,
      
      # Income per child (for families with children)
      INCOME_PER_CHILD = if_else(CNT_CHILDREN > 0, 
                                  AMT_INCOME_TOTAL / CNT_CHILDREN, 
                                  AMT_INCOME_TOTAL),
      
      # === Credit Burden Metrics ===
      
      # Credit term (how many periods to pay off)
      CREDIT_TERM_PERIODS = AMT_CREDIT / AMT_ANNUITY,
      
      # Years to pay off (approximate)
      CREDIT_TERM_YEARS = (AMT_CREDIT / AMT_ANNUITY) / 12,
      
      # Annuity per $1000 of credit
      ANNUITY_PER_1K_CREDIT = (AMT_ANNUITY / AMT_CREDIT) * 1000,
      
      # === External Source Combinations ===
      
      # Average external score (higher is better for creditworthiness)
      EXT_SOURCE_MEAN = (EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3) / 3,
      
      # Weighted average (EXT_SOURCE_2 and _3 are stronger predictors)
      EXT_SOURCE_WEIGHTED = (EXT_SOURCE_1 * 0.2 + 
                             EXT_SOURCE_2 * 0.4 + 
                             EXT_SOURCE_3 * 0.4),
      
      # Product of external sources (captures interaction)
      EXT_SOURCE_PRODUCT = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
      
      # === Age-Related Financial Ratios ===
      
      # Credit amount per year of age
      CREDIT_PER_AGE = AMT_CREDIT / AGE_YEARS,
      
      # Income per year of age
      INCOME_PER_AGE = AMT_INCOME_TOTAL / AGE_YEARS
    )
  
  cat("  - Created core credit ratios (DTI, LTV, payment ratios)\n")
  cat("  - Created downpayment metrics\n")
  cat("  - Created income adequacy ratios\n")
  cat("  - Created credit burden metrics\n")
  cat("  - Created external source combinations\n")
  cat("  - Created age-related financial ratios\n\n")
  
  return(data)
}


#' Create Missing Data Indicators
#' 
#' Creates binary indicators for missing values in key variables
#' Missing patterns can be predictive
#' 
#' @param data A dataframe
#' @return Dataframe with missing data indicators
create_missing_indicators <- function(data) {
  cat("Creating missing data indicators...\n")
  
  # Key variables to create missing indicators for
  vars_to_flag <- c(
    "OWN_CAR_AGE",
    "OCCUPATION_TYPE",
    "AMT_ANNUITY",
    "AMT_GOODS_PRICE",
    "CNT_FAM_MEMBERS",
    "DAYS_LAST_PHONE_CHANGE",
    "AMT_REQ_CREDIT_BUREAU_HOUR",
    "AMT_REQ_CREDIT_BUREAU_DAY",
    "AMT_REQ_CREDIT_BUREAU_WEEK",
    "AMT_REQ_CREDIT_BUREAU_MON",
    "AMT_REQ_CREDIT_BUREAU_QRT",
    "AMT_REQ_CREDIT_BUREAU_YEAR"
  )
  
  n_indicators <- 0
  
  for (var in vars_to_flag) {
    if (var %in% names(data)) {
      n_missing <- sum(is.na(data[[var]]))
      if (n_missing > 0) {
        flag_name <- paste0(var, "_MISSING")
        data[[flag_name]] <- if_else(is.na(data[[var]]), 1, 0)
        n_indicators <- n_indicators + 1
      }
    }
  }
  
  # Count total missing values per row (missingness pattern)
  data <- data |>
    mutate(
      TOTAL_MISSING_COUNT = rowSums(is.na(across(everything()))),
      MISSING_RATIO = TOTAL_MISSING_COUNT / ncol(data)
    )
  
  cat(sprintf("  - Created %d missing data indicators\n", n_indicators))
  cat("  - Created TOTAL_MISSING_COUNT and MISSING_RATIO\n\n")
  
  return(data)
}


#' Create Interaction Features
#' 
#' Creates interaction terms between important predictors
#' Captures non-linear relationships
#' 
#' @param data A dataframe
#' @return Dataframe with interaction features
create_interaction_features <- function(data) {
  cat("Creating interaction features...\n")
  
  data <- data |>
    mutate(
      # Age × Income interactions
      AGE_INCOME_INTERACTION = AGE_YEARS * log1p(AMT_INCOME_TOTAL),
      
      # Age × Credit interactions
      AGE_CREDIT_INTERACTION = AGE_YEARS * log1p(AMT_CREDIT),
      
      # Employment × Income
      EMPLOYED_INCOME_INTERACTION = EMPLOYED_YEARS * log1p(AMT_INCOME_TOTAL),
      
      # External source × Credit ratio
      EXT_SOURCE_CREDIT_RATIO = EXT_SOURCE_MEAN * CREDIT_INCOME_RATIO,
      
      # Family size × Income
      FAMILY_INCOME_INTERACTION = CNT_FAM_MEMBERS * log1p(AMT_INCOME_TOTAL),
      
      # Children × Income per person
      CHILDREN_INCOME_BURDEN = CNT_CHILDREN * INCOME_PER_PERSON,
      
      # Region rating × External source
      REGION_EXT_SOURCE = REGION_RATING_CLIENT * EXT_SOURCE_MEAN,
      
      # Age group × Income type (binary flags for combinations)
      YOUNG_WORKING = if_else(AGE_YEARS < 30 & NAME_INCOME_TYPE == "Working", 1, 0),
      OLD_PENSIONER = if_else(AGE_YEARS >= 55 & NAME_INCOME_TYPE == "Pensioner", 1, 0)
    )
  
  cat("  - Created age-based interactions\n")
  cat("  - Created employment-income interactions\n")
  cat("  - Created external source interactions\n")
  cat("  - Created family-income interactions\n")
  cat("  - Created categorical combinations\n\n")
  
  return(data)
}


#' Create Binned Variables
#' 
#' Creates binned/bucketed versions of continuous variables
#' Captures non-linear effects and reduces overfitting
#' Thresholds computed from training data only
#' 
#' @param data A dataframe
#' @param is_train Logical indicating if this is training data
#' @return Dataframe with binned features
create_binned_features <- function(data, is_train = TRUE) {
  cat("Creating binned features...\n")
  
  # Compute or retrieve binning thresholds
  if (is_train) {
    # Compute from training data
    TRAINING_STATS[["INCOME_BINS"]] <<- quantile(data$AMT_INCOME_TOTAL, 
                                                   c(0.25, 0.5, 0.75, 0.9), 
                                                   na.rm = TRUE)
    TRAINING_STATS[["CREDIT_BINS"]] <<- quantile(data$AMT_CREDIT, 
                                                   c(0.33, 0.67, 0.85), 
                                                   na.rm = TRUE)
    cat("  - Computed binning thresholds from training data\n")
  } else {
    cat("  - Using training data binning thresholds\n")
  }
  
  income_bins <- TRAINING_STATS[["INCOME_BINS"]]
  credit_bins <- TRAINING_STATS[["CREDIT_BINS"]]
  
  data <- data |>
    mutate(
      # Income bins (based on training percentiles)
      INCOME_BIN = case_when(
        AMT_INCOME_TOTAL <= income_bins[1] ~ "Very Low",
        AMT_INCOME_TOTAL <= income_bins[2] ~ "Low",
        AMT_INCOME_TOTAL <= income_bins[3] ~ "Medium",
        AMT_INCOME_TOTAL <= income_bins[4] ~ "High",
        TRUE ~ "Very High"
      ),
      
      # Credit amount bins
      CREDIT_BIN = case_when(
        AMT_CREDIT <= credit_bins[1] ~ "Small",
        AMT_CREDIT <= credit_bins[2] ~ "Medium",
        AMT_CREDIT <= credit_bins[3] ~ "Large",
        TRUE ~ "Very Large"
      ),
      
      # Credit-to-income ratio bins (fixed thresholds - domain knowledge)
      DTI_BIN = case_when(
        CREDIT_INCOME_RATIO <= 1 ~ "Low DTI",
        CREDIT_INCOME_RATIO <= 2 ~ "Moderate DTI",
        CREDIT_INCOME_RATIO <= 4 ~ "High DTI",
        TRUE ~ "Very High DTI"
      ),
      
      # Annuity-to-income ratio bins (payment burden - fixed thresholds)
      PAYMENT_BURDEN_BIN = case_when(
        ANNUITY_INCOME_RATIO <= 0.05 ~ "Light Burden",
        ANNUITY_INCOME_RATIO <= 0.10 ~ "Moderate Burden",
        ANNUITY_INCOME_RATIO <= 0.20 ~ "Heavy Burden",
        TRUE ~ "Very Heavy Burden"
      ),
      
      # Employment length bins (fixed thresholds)
      EMPLOYMENT_BIN = case_when(
        EMPLOYED_YEARS < 1 ~ "New Employee",
        EMPLOYED_YEARS < 5 ~ "Established",
        EMPLOYED_YEARS < 10 ~ "Experienced",
        TRUE ~ "Veteran"
      ),
      
      # External source score bins (fixed thresholds)
      EXT_SOURCE_BIN = case_when(
        EXT_SOURCE_MEAN <= 0.3 ~ "High Risk",
        EXT_SOURCE_MEAN <= 0.5 ~ "Medium Risk",
        EXT_SOURCE_MEAN <= 0.7 ~ "Low Risk",
        TRUE ~ "Very Low Risk"
      ),
      
      # Family size bins (fixed thresholds)
      FAMILY_SIZE_BIN = case_when(
        CNT_FAM_MEMBERS == 1 ~ "Single",
        CNT_FAM_MEMBERS == 2 ~ "Couple",
        CNT_FAM_MEMBERS <= 4 ~ "Small Family",
        TRUE ~ "Large Family"
      )
    )
  
  cat("  - Created income bins (from training percentiles)\n")
  cat("  - Created credit and DTI bins\n")
  cat("  - Created payment burden bins\n")
  cat("  - Created employment and external source bins\n")
  cat("  - Created family size bins\n\n")
  
  return(data)
}


#' Engineer Application Features
#' 
#' Main function to create all engineered features
#' 
#' @param data Application dataframe (must be cleaned first)
#' @param is_train Logical indicating if this is training data
#' @return Dataframe with all engineered features
engineer_application_features <- function(data, is_train = TRUE) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  if (is_train) {
    cat("ENGINEERING APPLICATION FEATURES (TRAINING)\n")
  } else {
    cat("ENGINEERING APPLICATION FEATURES (TEST - using training stats)\n")
  }
  cat(strrep("=", 80), "\n\n")
  
  original_cols <- ncol(data)
  
  # 1. Create demographic features
  data <- create_demographic_features(data)
  
  # 2. Create financial ratios
  data <- create_financial_ratios(data)
  
  # 3. Create missing indicators
  data <- create_missing_indicators(data)
  
  # 4. Create interaction features
  data <- create_interaction_features(data)
  
  # 5. Create binned features
  data <- create_binned_features(data, is_train = is_train)
  
  # Summary
  final_cols <- ncol(data)
  new_features <- final_cols - original_cols
  
  cat(strrep("=", 80), "\n")
  cat("FEATURE ENGINEERING SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Original columns: %d\n", original_cols))
  cat(sprintf("Final columns: %d\n", final_cols))
  cat(sprintf("New features created: %d\n", new_features))
  cat("\n")
  
  return(data)
}

# ==============================================================================
# SUPPLEMENTARY DATA AGGREGATION FUNCTIONS
# ==============================================================================

#' Aggregate Bureau Data
#' 
#' Aggregates bureau.csv to applicant level (SK_ID_CURR)
#' Creates features about credit history from other financial institutions
#' 
#' @param bureau_path Path to bureau.csv file
#' @return Dataframe aggregated to SK_ID_CURR level
aggregate_bureau_data <- function(bureau_path = "bureau.csv") {
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("AGGREGATING BUREAU DATA\n")
  cat("="*80, "\n\n", sep = "")
  
  cat("Loading bureau data...\n")
  bureau <- read_csv(bureau_path, show_col_types = FALSE)
  cat(sprintf("  - Loaded %s rows\n", format(nrow(bureau), big.mark = ",")))
  cat(sprintf("  - %s unique applicants\n\n", 
              format(n_distinct(bureau$SK_ID_CURR), big.mark = ",")))
  
  cat("Creating bureau aggregations...\n")
  
  bureau_agg <- bureau |>
    group_by(SK_ID_CURR) |>
    summarise(
      # === Count Features ===
      BUREAU_CREDIT_COUNT = n(),                    # Total number of previous credits
      BUREAU_ACTIVE_COUNT = sum(CREDIT_ACTIVE == "Active", na.rm = TRUE),
      BUREAU_CLOSED_COUNT = sum(CREDIT_ACTIVE == "Closed", na.rm = TRUE),
      
      # === Credit Types ===
      BUREAU_CREDIT_TYPE_COUNT = n_distinct(CREDIT_TYPE, na.rm = TRUE),
      BUREAU_CONSUMER_CREDIT_COUNT = sum(CREDIT_TYPE == "Consumer credit", na.rm = TRUE),
      BUREAU_CREDIT_CARD_COUNT = sum(CREDIT_TYPE == "Credit card", na.rm = TRUE),
      BUREAU_MORTGAGE_COUNT = sum(CREDIT_TYPE == "Mortgage", na.rm = TRUE),
      BUREAU_CAR_LOAN_COUNT = sum(CREDIT_TYPE == "Car loan", na.rm = TRUE),
      
      # === Currency ===
      BUREAU_CURRENCY_COUNT = n_distinct(CREDIT_CURRENCY, na.rm = TRUE),
      BUREAU_NON_LOCAL_CURRENCY = sum(CREDIT_CURRENCY != "currency 1", na.rm = TRUE),
      
      # === Overdue Amounts ===
      BUREAU_TOTAL_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_MAX_OVERDUE = max(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_AVG_OVERDUE = mean(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_HAS_OVERDUE = as.integer(any(AMT_CREDIT_SUM_OVERDUE > 0, na.rm = TRUE)),
      
      # === Credit Amounts ===
      BUREAU_TOTAL_DEBT = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      BUREAU_AVG_DEBT = mean(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      BUREAU_MAX_DEBT = max(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      
      BUREAU_TOTAL_CREDIT = sum(AMT_CREDIT_SUM, na.rm = TRUE),
      BUREAU_AVG_CREDIT = mean(AMT_CREDIT_SUM, na.rm = TRUE),
      BUREAU_MAX_CREDIT = max(AMT_CREDIT_SUM, na.rm = TRUE),
      
      # === Credit Limit ===
      BUREAU_TOTAL_CREDIT_LIMIT = sum(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE),
      BUREAU_AVG_CREDIT_LIMIT = mean(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE),
      
      # === Debt Ratios ===
      BUREAU_DEBT_CREDIT_RATIO = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE) / 
                                 sum(AMT_CREDIT_SUM, na.rm = TRUE),
      BUREAU_OVERDUE_DEBT_RATIO = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE) / 
                                  sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      
      # === Days Features ===
      BUREAU_AVG_DAYS_CREDIT = mean(DAYS_CREDIT, na.rm = TRUE),
      BUREAU_MIN_DAYS_CREDIT = min(DAYS_CREDIT, na.rm = TRUE),
      BUREAU_MAX_DAYS_CREDIT = max(DAYS_CREDIT, na.rm = TRUE),
      
      BUREAU_AVG_CREDIT_UPDATE = mean(DAYS_CREDIT_UPDATE, na.rm = TRUE),
      BUREAU_AVG_ENDDATE_FACT = mean(DAYS_ENDDATE_FACT, na.rm = TRUE),
      
      # === Prolongation ===
      BUREAU_PROLONGATION_COUNT = sum(CNT_CREDIT_PROLONG, na.rm = TRUE),
      BUREAU_AVG_PROLONGATION = mean(CNT_CREDIT_PROLONG, na.rm = TRUE),
      
      # === Active Credit Features ===
      BUREAU_ACTIVE_TOTAL_DEBT = sum(AMT_CREDIT_SUM_DEBT[CREDIT_ACTIVE == "Active"], 
                                     na.rm = TRUE),
      BUREAU_ACTIVE_AVG_DEBT = mean(AMT_CREDIT_SUM_DEBT[CREDIT_ACTIVE == "Active"], 
                                    na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    # Handle infinite values from division by zero
    mutate(across(where(is.numeric), ~if_else(is.infinite(.), NA_real_, .)))
  
  cat(sprintf("  - Created %d bureau features\n", ncol(bureau_agg) - 1))
  cat(sprintf("  - Aggregated to %s applicants\n\n", 
              format(nrow(bureau_agg), big.mark = ",")))
  
  return(bureau_agg)
}


#' Aggregate Previous Application Data
#' 
#' Aggregates previous_application.csv to applicant level (SK_ID_CURR)
#' Creates features about previous loan applications at Home Credit
#' 
#' @param prev_app_path Path to previous_application.csv file
#' @return Dataframe aggregated to SK_ID_CURR level
aggregate_previous_application <- function(prev_app_path = "previous_application.csv") {
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("AGGREGATING PREVIOUS APPLICATION DATA\n")
  cat("="*80, "\n\n", sep = "")
  
  cat("Loading previous application data...\n")
  prev_app <- read_csv(prev_app_path, show_col_types = FALSE)
  cat(sprintf("  - Loaded %s rows\n", format(nrow(prev_app), big.mark = ",")))
  cat(sprintf("  - %s unique applicants\n\n", 
              format(n_distinct(prev_app$SK_ID_CURR), big.mark = ",")))
  
  cat("Creating previous application aggregations...\n")
  
  prev_app_agg <- prev_app |>
    group_by(SK_ID_CURR) |>
    summarise(
      # === Application Counts ===
      PREV_APP_COUNT = n(),
      
      # === Application Status ===
      PREV_APP_APPROVED = sum(NAME_CONTRACT_STATUS == "Approved", na.rm = TRUE),
      PREV_APP_REFUSED = sum(NAME_CONTRACT_STATUS == "Refused", na.rm = TRUE),
      PREV_APP_CANCELED = sum(NAME_CONTRACT_STATUS == "Canceled", na.rm = TRUE),
      PREV_APP_UNUSED = sum(NAME_CONTRACT_STATUS == "Unused offer", na.rm = TRUE),
      
      # === Approval Rate ===
      PREV_APP_APPROVAL_RATE = mean(NAME_CONTRACT_STATUS == "Approved", na.rm = TRUE),
      PREV_APP_REFUSAL_RATE = mean(NAME_CONTRACT_STATUS == "Refused", na.rm = TRUE),
      
      # === Contract Type ===
      PREV_APP_CASH_LOAN_COUNT = sum(NAME_CONTRACT_TYPE == "Cash loans", na.rm = TRUE),
      PREV_APP_CONSUMER_LOAN_COUNT = sum(NAME_CONTRACT_TYPE == "Consumer loans", na.rm = TRUE),
      PREV_APP_REVOLVING_COUNT = sum(NAME_CONTRACT_TYPE == "Revolving loans", na.rm = TRUE),
      
      # === Credit Amounts (Approved Only) ===
      PREV_APP_AVG_CREDIT = mean(AMT_CREDIT[NAME_CONTRACT_STATUS == "Approved"], 
                                  na.rm = TRUE),
      PREV_APP_MAX_CREDIT = max(AMT_CREDIT[NAME_CONTRACT_STATUS == "Approved"], 
                                na.rm = TRUE),
      PREV_APP_TOTAL_CREDIT = sum(AMT_CREDIT[NAME_CONTRACT_STATUS == "Approved"], 
                                  na.rm = TRUE),
      
      # === Application vs Received ===
      PREV_APP_AVG_APPLICATION = mean(AMT_APPLICATION, na.rm = TRUE),
      PREV_APP_AVG_CREDIT_RATIO = mean(AMT_CREDIT / AMT_APPLICATION, na.rm = TRUE),
      
      # === Downpayment ===
      PREV_APP_AVG_DOWNPAYMENT = mean(AMT_DOWN_PAYMENT, na.rm = TRUE),
      PREV_APP_DOWNPAYMENT_RATIO = mean(AMT_DOWN_PAYMENT / AMT_CREDIT, na.rm = TRUE),
      
      # === Goods Price ===
      PREV_APP_AVG_GOODS_PRICE = mean(AMT_GOODS_PRICE, na.rm = TRUE),
      
      # === Annuity ===
      PREV_APP_AVG_ANNUITY = mean(AMT_ANNUITY, na.rm = TRUE),
      
      # === Time Features ===
      PREV_APP_AVG_DAYS_DECISION = mean(DAYS_DECISION, na.rm = TRUE),
      PREV_APP_MOST_RECENT_DECISION = max(DAYS_DECISION, na.rm = TRUE),
      
      # === Product Type ===
      PREV_APP_PRODUCT_TYPE_COUNT = n_distinct(NAME_PRODUCT_TYPE, na.rm = TRUE),
      PREV_APP_XNA_PRODUCT = sum(NAME_PRODUCT_TYPE == "XNA", na.rm = TRUE),
      
      # === Client Type ===
      PREV_APP_REPEATER = sum(NAME_CLIENT_TYPE == "Repeater", na.rm = TRUE),
      PREV_APP_REFRESHED = sum(NAME_CLIENT_TYPE == "Refreshed", na.rm = TRUE),
      
      # === Payment Type ===
      PREV_APP_CASH_PAYMENT = sum(NAME_PAYMENT_TYPE == "Cash through the bank", 
                                   na.rm = TRUE),
      
      # === Yield Group ===
      PREV_APP_HIGH_YIELD = sum(NAME_YIELD_GROUP %in% c("high", "middle"), na.rm = TRUE),
      
      # === Portfolio ===
      PREV_APP_POS_COUNT = sum(NAME_PORTFOLIO == "POS", na.rm = TRUE),
      PREV_APP_CASH_COUNT = sum(NAME_PORTFOLIO == "Cash", na.rm = TRUE),
      
      # === Refusal Reasons ===
      PREV_APP_HC_REFUSAL = sum(CODE_REJECT_REASON == "HC", na.rm = TRUE),
      PREV_APP_LIMIT_REFUSAL = sum(CODE_REJECT_REASON == "LIMIT", na.rm = TRUE),
      PREV_APP_SCO_REFUSAL = sum(CODE_REJECT_REASON == "SCO", na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~if_else(is.infinite(.), NA_real_, .)))
  
  cat(sprintf("  - Created %d previous application features\n", ncol(prev_app_agg) - 1))
  cat(sprintf("  - Aggregated to %s applicants\n\n", 
              format(nrow(prev_app_agg), big.mark = ",")))
  
  return(prev_app_agg)
}


#' Aggregate Installments Payments Data
#' 
#' Aggregates installments_payments.csv to applicant level (SK_ID_CURR)
#' Creates features about payment behavior and timeliness
#' 
#' @param install_path Path to installments_payments.csv file
#' @param prev_app_path Path to previous_application.csv (for SK_ID_CURR mapping)
#' @return Dataframe aggregated to SK_ID_CURR level
aggregate_installments_payments <- function(install_path = "installments_payments.csv",
                                            prev_app_path = "previous_application.csv") {
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("AGGREGATING INSTALLMENTS PAYMENTS DATA\n")
  cat("="*80, "\n\n", sep = "")
  
  cat("Loading installments payments data...\n")
  install <- read_csv(install_path, show_col_types = FALSE)
  cat(sprintf("  - Loaded %s rows\n", format(nrow(install), big.mark = ",")))
  
  # Need to map SK_ID_PREV to SK_ID_CURR
  cat("Loading previous application for mapping...\n")
  prev_app <- read_csv(prev_app_path, show_col_types = FALSE) |>
    select(SK_ID_PREV, SK_ID_CURR)
  
  # Join to get SK_ID_CURR
  install <- install |>
    left_join(prev_app, by = "SK_ID_PREV")
  
  cat(sprintf("  - %s unique applicants\n\n", 
              format(n_distinct(install$SK_ID_CURR), big.mark = ",")))
  
  cat("Creating installments payments aggregations...\n")
  
  install_agg <- install |>
    # Create derived features first
    mutate(
      PAYMENT_DIFF = AMT_PAYMENT - AMT_INSTALMENT,
      PAYMENT_RATIO = AMT_PAYMENT / AMT_INSTALMENT,
      DAYS_LATE = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
      IS_LATE = as.integer(DAYS_LATE > 0),
      IS_UNDERPAID = as.integer(AMT_PAYMENT < AMT_INSTALMENT)
    ) |>
    group_by(SK_ID_CURR) |>
    summarise(
      # === Payment Counts ===
      INSTALL_PAYMENT_COUNT = n(),
      INSTALL_CREDIT_COUNT = n_distinct(SK_ID_PREV),
      
      # === Late Payment Features ===
      INSTALL_LATE_PAYMENT_COUNT = sum(IS_LATE, na.rm = TRUE),
      INSTALL_LATE_PAYMENT_RATE = mean(IS_LATE, na.rm = TRUE),
      INSTALL_AVG_DAYS_LATE = mean(DAYS_LATE[DAYS_LATE > 0], na.rm = TRUE),
      INSTALL_MAX_DAYS_LATE = max(DAYS_LATE, na.rm = TRUE),
      INSTALL_TOTAL_DAYS_LATE = sum(pmax(DAYS_LATE, 0), na.rm = TRUE),
      
      # === Underpayment Features ===
      INSTALL_UNDERPAID_COUNT = sum(IS_UNDERPAID, na.rm = TRUE),
      INSTALL_UNDERPAID_RATE = mean(IS_UNDERPAID, na.rm = TRUE),
      INSTALL_AVG_UNDERPAYMENT = mean(PAYMENT_DIFF[PAYMENT_DIFF < 0], na.rm = TRUE),
      INSTALL_TOTAL_UNDERPAYMENT = sum(pmin(PAYMENT_DIFF, 0), na.rm = TRUE),
      
      # === Overpayment Features ===
      INSTALL_OVERPAID_COUNT = sum(PAYMENT_DIFF > 0, na.rm = TRUE),
      INSTALL_AVG_OVERPAYMENT = mean(PAYMENT_DIFF[PAYMENT_DIFF > 0], na.rm = TRUE),
      
      # === Payment Ratios ===
      INSTALL_AVG_PAYMENT_RATIO = mean(PAYMENT_RATIO, na.rm = TRUE),
      INSTALL_MIN_PAYMENT_RATIO = min(PAYMENT_RATIO, na.rm = TRUE),
      
      # === Payment Amounts ===
      INSTALL_AVG_PAYMENT = mean(AMT_PAYMENT, na.rm = TRUE),
      INSTALL_TOTAL_PAYMENT = sum(AMT_PAYMENT, na.rm = TRUE),
      INSTALL_MAX_PAYMENT = max(AMT_PAYMENT, na.rm = TRUE),
      
      # === Installment Amounts ===
      INSTALL_AVG_INSTALMENT = mean(AMT_INSTALMENT, na.rm = TRUE),
      INSTALL_TOTAL_INSTALMENT = sum(AMT_INSTALMENT, na.rm = TRUE),
      
      # === Payment Trends (Recent vs Overall) ===
      # Last 12 payments vs all
      INSTALL_RECENT_LATE_RATE = mean(IS_LATE[DAYS_INSTALMENT >= -365], na.rm = TRUE),
      INSTALL_RECENT_PAYMENT_RATIO = mean(PAYMENT_RATIO[DAYS_INSTALMENT >= -365], 
                                           na.rm = TRUE),
      
      # === Payment Consistency ===
      INSTALL_PAYMENT_STD = sd(AMT_PAYMENT, na.rm = TRUE),
      INSTALL_PAYMENT_CV = sd(AMT_PAYMENT, na.rm = TRUE) / mean(AMT_PAYMENT, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~if_else(is.infinite(.), NA_real_, .)))
  
  cat(sprintf("  - Created %d installments payment features\n", ncol(install_agg) - 1))
  cat(sprintf("  - Aggregated to %s applicants\n\n", 
              format(nrow(install_agg), big.mark = ",")))
  
  return(install_agg)
}


#' Merge All Supplementary Data
#' 
#' Merges all aggregated supplementary data sources to application data
#' 
#' @param app_data Application dataframe (train or test)
#' @param bureau_agg Aggregated bureau data
#' @param prev_app_agg Aggregated previous application data
#' @param install_agg Aggregated installments payments data
#' @return Application data with all supplementary features merged
merge_supplementary_data <- function(app_data, 
                                     bureau_agg = NULL, 
                                     prev_app_agg = NULL, 
                                     install_agg = NULL) {
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("MERGING SUPPLEMENTARY DATA\n")
  cat("="*80, "\n\n", sep = "")
  
  original_cols <- ncol(app_data)
  original_rows <- nrow(app_data)
  
  # Merge bureau
  if (!is.null(bureau_agg)) {
    cat("Merging bureau data...\n")
    app_data <- app_data |>
      left_join(bureau_agg, by = "SK_ID_CURR")
    cat(sprintf("  - Added %d bureau features\n", ncol(bureau_agg) - 1))
  }
  
  # Merge previous application
  if (!is.null(prev_app_agg)) {
    cat("Merging previous application data...\n")
    app_data <- app_data |>
      left_join(prev_app_agg, by = "SK_ID_CURR")
    cat(sprintf("  - Added %d previous application features\n", ncol(prev_app_agg) - 1))
  }
  
  # Merge installments
  if (!is.null(install_agg)) {
    cat("Merging installments payment data...\n")
    app_data <- app_data |>
      left_join(install_agg, by = "SK_ID_CURR")
    cat(sprintf("  - Added %d installments payment features\n", ncol(install_agg) - 1))
  }
  
  final_cols <- ncol(app_data)
  
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("MERGE SUMMARY\n")
  cat("="*80, "\n", sep = "")
  cat(sprintf("Original columns: %d\n", original_cols))
  cat(sprintf("Final columns: %d\n", final_cols))
  cat(sprintf("Features added: %d\n", final_cols - original_cols))
  cat(sprintf("Rows: %s (unchanged)\n", format(original_rows, big.mark = ",")))
  cat("\n")
  
  return(app_data)
}


# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

if (!interactive()) {
  # Load data
  cat("Loading data...\n")
  app_train <- read_csv("application_train.csv", show_col_types = FALSE)
  app_test <- read_csv("application_test.csv", show_col_types = FALSE)
  
  cat("Train set:", nrow(app_train), "rows x", ncol(app_train), "columns\n")
  cat("Test set:", nrow(app_test), "rows x", ncol(app_test), "columns\n\n")
  
  # === STEP 1: CLEAN DATA ===
  train_result <- clean_application_data(app_train)
  app_train_clean <- train_result$data
  
  test_result <- clean_application_data(app_test)
  app_test_clean <- test_result$data
  
  # === STEP 2: ENGINEER FEATURES ===
  app_train_engineered <- engineer_application_features(app_train_clean)
  app_test_engineered <- engineer_application_features(app_test_clean)
  
  # === STEP 3: AGGREGATE SUPPLEMENTARY DATA ===
  # These are large files - may take several minutes
  bureau_agg <- aggregate_bureau_data()
  prev_app_agg <- aggregate_previous_application()
  install_agg <- aggregate_installments_payments()
  
  # === STEP 4: MERGE SUPPLEMENTARY DATA ===
  app_train_final <- merge_supplementary_data(
    app_train_engineered,
    bureau_agg = bureau_agg,
    prev_app_agg = prev_app_agg,
    install_agg = install_agg
  )
  
  app_test_final <- merge_supplementary_data(
    app_test_engineered,
    bureau_agg = bureau_agg,
    prev_app_agg = prev_app_agg,
    install_agg = install_agg
  )
  
  # === STEP 5: SAVE RESULTS ===
  cat("Saving processed data...\n")
  
  # Save intermediate files
  write_csv(app_train_clean, "application_train_clean.csv")
  write_csv(app_test_clean, "application_test_clean.csv")
  
  # Save engineered files
  write_csv(app_train_engineered, "application_train_engineered.csv")
  write_csv(app_test_engineered, "application_test_engineered.csv")
  
  # Save aggregated supplementary data
  write_csv(bureau_agg, "bureau_aggregated.csv")
  write_csv(prev_app_agg, "previous_application_aggregated.csv")
  write_csv(install_agg, "installments_payments_aggregated.csv")
  
  # Save final files (with all features)
  write_csv(app_train_final, "application_train_final.csv")
  write_csv(app_test_final, "application_test_final.csv")
  
  cat("\n")
  cat("="*80, "\n", sep = "")
  cat("PROCESSING COMPLETE!\n")
  cat("="*80, "\n", sep = "")
  cat("\nFiles created:\n")
  cat("\nCleaned data:\n")
  cat("  - application_train_clean.csv\n")
  cat("  - application_test_clean.csv\n")
  cat("\nEngineered features:\n")
  cat("  - application_train_engineered.csv\n")
  cat("  - application_test_engineered.csv\n")
  cat("\nAggregated supplementary data:\n")
  cat("  - bureau_aggregated.csv\n")
  cat("  - previous_application_aggregated.csv\n")
  cat("  - installments_payments_aggregated.csv\n")
  cat("\nFinal datasets (all features):\n")
  cat("  - application_train_final.csv\n")
  cat("  - application_test_final.csv\n")
  cat("\nFinal dimensions:\n")
  cat(sprintf("  Train: %s rows x %d columns\n", 
              format(nrow(app_train_final), big.mark = ","),
              ncol(app_train_final)))
  cat(sprintf("  Test: %s rows x %d columns\n", 
              format(nrow(app_test_final), big.mark = ","),
              ncol(app_test_final)))
  cat("\n")
}
