#' Apply quality control filtering to astro samples
#'
#' @param data A data frame containing the raw astro data
#' @param snr_threshold Minimum signal-to-noise ratio
#' @param teff_thresholds A vector of two values indicating the minimum and
#'   maximum effective temperature
#' @param logg_threshold Maximum surface gravity
#' @param starflag Star flag for quality control
#' @param vb_threshold Minimum membership probability
#'
#' @returns A list containing two data frames: `abundance` and `meta`.
apply_qc_filtering <- function(data,
                               snr_threshold = 70,
                               teff_thresholds = c(3500, 5500),
                               logg_threshold = 3.6,
                               starflag = 0,
                               vb_threshold = 0.9) {
  data_cleaned <- data |>
    # apply QC filters
    dplyr::filter(
      SNREV > !!snr_threshold, # Signal-to-noise ratio
      TEFF_SPEC > !!teff_thresholds[1], # Min temperature of star
      TEFF_SPEC < !!teff_thresholds[2], # Max temperature of star
      LOGG_SPEC < !!logg_threshold, # Gravity
      STARFLAG == !!starflag, # QC on measurement
      VB_PROB > !!vb_threshold # Position/PM-based membership probability
    ) |>
    # remove duplicates
    dplyr::distinct(APOGEE_ID, .keep_all = TRUE)

  # GLON and GLAT are space coordinates (latitude and longutitude)
  # for other measurements, see
  # https://data.sdss.org/datamodel/files/APOGEE_GC/GC_members_VAC.html
  meta_cols <- c(
    "X_H", "GLON", "GLAT", "GC_NAME", "APSTAR_ID", "TELESCOPE", "LOCATION_ID",
    "TARGFLAGS", "STARFLAG", "VHELIO_AVG", "TEFF_SPEC", "LOGG_SPEC",
    "VB_PROB", "SNREV", "APOGEE_ID"
  )
  # get metadata
  metadata <- data_cleaned |>
    dplyr::select(tidyselect::all_of(meta_cols))

  # get abundance data
  abundance_cols <- c(
    "C_FE", "CI_FE", "N_FE", "O_FE", "NA_FE", "MG_FE", "AL_FE", "SI_FE",
    "S_FE", "K_FE", "CA_FE", "TI_FE", "TIII_FE", "V_FE", "CR_FE", "MN_FE",
    "FE_H", "CO_FE", "NI_FE"
  )
  abundance_data <- data_cleaned |>
    dplyr::select(tidyselect::all_of(abundance_cols))

  return(list(abundance = abundance_data, meta = metadata))
}


#' Clean astro data (i.e., impute and standardize)
#'
#' @param train_data A data frame containing the training abundance data.
#' @param test_data A data frame containing the test abundance data.
#'
#' @returns A list containing the cleaned `train` and `test` data frames.
clean_astro_data <- function(train_data, test_data,
                             impute_mode = c("mean", "rf")) {

  # impute data
  impute_mode <- match.arg(impute_mode)
  if (impute_mode == "mean") {
    # impute missing values using mean
    col_means <- apply(train_data, 2, function(x) mean(x, na.rm = TRUE))
    for (col in 1:ncol(train_data)) {
      train_data[is.na(train_data[, col]), col] <- col_means[col]
      test_data[is.na(test_data[, col]), col] <- col_means[col]
    }
  } else if (impute_mode == "rf") {
    # impute missing values using RF imputation
    imputed_data <- missForest::missForest(
      # should ideally do this only on train data,
      # but currently no implementation to do so in missForest R package
      xmis = as.matrix(rbind(train_data, test_data))
    )
    train_data <- tibble::as_tibble(
      imputed_data$ximp[1:nrow(train_data), ]
    )
    test_data <- tibble::as_tibble(
      imputed_data$ximp[(nrow(train_data) + 1):nrow(imputed_data$ximp), ]
    )
  }

  # standardize data
  imputer <- caret::preProcess(
    train_data,
    method = c("center", "scale")
  )
  train_data <- predict(imputer, train_data)
  test_data <- predict(imputer, test_data)

  return(list(train = train_data, test = test_data))
}


#' Get abundance data with 7, 11, or all 19 features
#'
#' @param data A data frame containing the abundance data.
#' @param feature_mode A numeric value indicating the number of features to
#'   return. Options are 7, 11, or 19.
#'
#' @returns A data frame containing the selected abundance feature set.
get_abundance_data <- function(data, feature_mode = c(7, 11, 19)) {
  if (feature_mode == 7) {
    keep_features <- c(
      "FE_H", "MG_FE", "O_FE", "SI_FE", "CA_FE", "NI_FE", "AL_FE"
    )
  } else if (feature_mode == 11) {
    keep_features <- c(
      "FE_H", "MG_FE", "O_FE", "SI_FE", "CA_FE", "NI_FE", "AL_FE",
      "C_FE", "K_FE", "MN_FE", "N_FE"
    )
  } else if (feature_mode == 19) {
    keep_features <- colnames(data)
  } else {
    stop("Invalid feature mode. Choose 7, 11, or 19.")
  }
  data <- data |>
    dplyr::select(tidyselect::all_of(keep_features))
  return(data)
}
