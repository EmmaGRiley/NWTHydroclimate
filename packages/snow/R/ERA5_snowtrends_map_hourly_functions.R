# =============================================================================
# ERA5 Snow Trends Mapping - Functions
# =============================================================================
# This file contains all reusable functions for ERA5 snow trend analysis
# Required by: ERA5_snowtrends_map_hourly.R
# 
# Functions included:
# - interpret_acf: Interprets autocorrelation results for manual survey data
# - interpret_acf_canswe: Interprets autocorrelation results for CanSWE data  
# - serial_autocorr_test: Tests for serial autocorrelation in manual surveys
# - serial_autocorr_test_canswe: Tests for serial autocorrelation in CanSWE data
# - p.value.function: Determines plotting style based on p-values
# - sen: Sen slope calculation wrapper
# - appendvar: Utility function for variable matching
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Append variable from one data frame to another
#' 
#' Utility function for matching and appending variables between data frames.
#' 
#' @param var1 Vector of values to match
#' @param var2 Vector to match against
#' @param var3 Vector of values to return
#' @return Vector of matched values
#' @export
appendvar <- function(var1, var2, var3) {
  var4 <- var3[match(var1, var2)]
  return(var4)
}

# =============================================================================
# AUTOCORRELATION INTERPRETATION FUNCTIONS
# =============================================================================

#' Interpret autocorrelation results for manual survey data
#' 
#' Interprets autocorrelation function (ACF) results from manual snow survey data,
#' identifying significant lags and characterizing autocorrelation strength.
#' 
#' @param acf_results Data frame with autocorrelation test results. Must contain
#'   columns: site, n_years, lag, acf
#' @param only_autocorrelated Logical, if TRUE only return autocorrelated sites
#' @return Data frame with interpreted autocorrelation results including:
#'   \itemize{
#'     \item site: Site name
#'     \item sample_size: Number of years
#'     \item significance_threshold: Statistical significance threshold
#'     \item lag1_acf: Lag-1 autocorrelation value
#'     \item lag1_significant: Whether lag-1 is significant
#'     \item is_autocorrelated: Whether site shows autocorrelation
#'     \item significant_lags: Number of significant lags
#'     \item first_sig_lag: First significant lag
#'     \item max_sig_lag: Lag with maximum autocorrelation
#'     \item max_sig_acf: Maximum autocorrelation value
#'     \item lag1_strength: Strength classification (None/Weak/Moderate/Strong)
#'   }
#' @export
interpret_acf <- function(acf_results, only_autocorrelated = FALSE) {
  # Initialize empty list to store results
  all_summaries <- list()
  
  # Process each site
  for (site in unique(acf_results$site)) {
    # Add error handling for each site
    tryCatch({
      site_data <- acf_results[acf_results$site == site, ]
      
      # Check if site_data is empty or contains NA values
      if (nrow(site_data) == 0 || all(is.na(site_data$acf))) {
        warning(sprintf("Skipping site %s: insufficient data", site))
        next
      }
      
      n <- site_data$n_years
      if (is.na(n) || n <= 0) {
        warning(sprintf("Skipping site %s: invalid n_years", site))
        next
      }
      
      sig_threshold <- qnorm(0.975)/sqrt(n)
      
      # Safely extract and check lags
      lags <- unlist(site_data$lag)
      acf_values <- unlist(site_data$acf)
      
      if (length(lags) == 0 || length(acf_values) == 0) {
        warning(sprintf("Skipping site %s: missing lag or ACF values", site))
        next
      }
      
      # More robust significant lags calculation
      significant_lags <- logical(0)  # Initialize
      lag_indices <- which(lags > 0)
      
      if (length(lag_indices) > 0) {
        # Remove NA values before comparison
        valid_acf <- acf_values[lag_indices]
        valid_indices <- !is.na(valid_acf)
        
        if (any(valid_indices)) {
          significant_lags <- valid_acf[valid_indices] > sig_threshold |
            valid_acf[valid_indices] < -sig_threshold
        } else {
          significant_lags <- logical(0)
        }
      }
      
      # Identify details about which lags are significant
      lag1_idx <- which(lags == 1)
      lag1_acf_val <- if (length(lag1_idx)) acf_values[lag1_idx] else NA_real_
      lag1_sig <- if (length(lag1_idx) && !is.na(lag1_acf_val)) abs(lag1_acf_val) > sig_threshold else FALSE
      
      sig_lags <- if (length(significant_lags) && any(significant_lags, na.rm = TRUE)) {
        valid_lag_indices <- lag_indices[valid_indices]
        valid_lag_indices[significant_lags]
      } else integer(0)
      first_sig_lag <- if (length(sig_lags)) min(sig_lags) else NA_integer_
      
      max_sig_lag <- if (length(sig_lags)) {
        sig_acf <- acf_values[match(sig_lags, lags)]
        sig_lags[which.max(abs(sig_acf))]
      } else NA_integer_
      
      max_sig_acf <- if (!is.na(max_sig_lag)) acf_values[match(max_sig_lag, lags)] else NA_real_
      
      # Check if significant_lags is empty
      if (length(significant_lags) == 0) {
        warning(sprintf("Skipping site %s: no valid significant lags found", site))
        next
      }
      
      # Only proceed if we should include this site
      if (!only_autocorrelated || any(significant_lags, na.rm = TRUE)) {
        # Create summary
        summary_row <- data.frame(
          site = site,
          sample_size = n,
          significance_threshold = sig_threshold,
          lag1_acf = lag1_acf_val,
          lag1_significant = lag1_sig,
          is_autocorrelated = any(significant_lags, na.rm = TRUE),
          significant_lags = sum(significant_lags, na.rm = TRUE),
          first_sig_lag = first_sig_lag,
          max_sig_lag = max_sig_lag,
          max_sig_acf = max_sig_acf,
          lag1_strength = dplyr::case_when(
            is.na(lag1_acf_val) ~ "Unknown",
            abs(lag1_acf_val) > 0.4 ~ "Strong",
            abs(lag1_acf_val) > 0.2 ~ "Moderate",
            abs(lag1_acf_val) > 0.1 ~ "Weak",
            TRUE ~ "None"
          )
        )
        all_summaries[[length(all_summaries) + 1]] <- summary_row
      }
    }, error = function(e) {
      warning(sprintf("Error processing site %s: %s", site, e$message))
    })
  }
  
  # Combine all summaries and return
  if (length(all_summaries) > 0) {
    return(do.call(rbind, all_summaries))
  } else {
    # Return empty data frame with correct column structure if no sites meet criteria
    return(data.frame(
      site = character(),
      sample_size = numeric(),
      significance_threshold = numeric(),
      lag1_acf = numeric(),
      lag1_significant = logical(),
      is_autocorrelated = logical(),
      significant_lags = numeric(),
      first_sig_lag = integer(),
      max_sig_lag = integer(),
      max_sig_acf = numeric(),
      lag1_strength = character(),
      stringsAsFactors = FALSE
    ))
  }
}

#' Interpret autocorrelation results for CanSWE data
#' 
#' Interprets autocorrelation function (ACF) results from CanSWE data,
#' identifying significant lags and characterizing autocorrelation strength.
#' 
#' @param acf_results_canswe Data frame with CanSWE autocorrelation test results.
#'   Must contain columns: site, n_years, lag, acf
#' @param only_autocorrelated Logical, if TRUE only return autocorrelated sites
#' @return Data frame with interpreted autocorrelation results (same structure
#'   as interpret_acf)
#' @export
interpret_acf_canswe <- function(acf_results_canswe, only_autocorrelated = FALSE) {
  # Initialize empty list to store results
  all_summaries <- list()
  
  # Process each site
  for (site in unique(acf_results_canswe$site)) {
    # Add error handling for each site
    tryCatch({
      site_data <- acf_results_canswe[acf_results_canswe$site == site, ]
      
      # Check if site_data is empty or contains NA values
      if (nrow(site_data) == 0 || all(is.na(site_data$acf))) {
        warning(sprintf("Skipping site %s: insufficient data", site))
        next
      }
      
      n <- site_data$n_years
      if (is.na(n) || n <= 0) {
        warning(sprintf("Skipping site %s: invalid n_years", site))
        next
      }
      
      sig_threshold <- qnorm(0.975)/sqrt(n)
      
      # Safely extract and check lags
      lags <- unlist(site_data$lag)
      acf_values <- unlist(site_data$acf)
      
      if (length(lags) == 0 || length(acf_values) == 0) {
        warning(sprintf("Skipping site %s: missing lag or ACF values", site))
        next
      }
      
      # More robust significant lags calculation
      significant_lags <- logical(0)  # Initialize
      lag_indices <- which(lags > 0)
      
      if (length(lag_indices) > 0) {
        # Remove NA values before comparison
        valid_acf <- acf_values[lag_indices]
        valid_indices <- !is.na(valid_acf)
        
        if (any(valid_indices)) {
          significant_lags <- valid_acf[valid_indices] > sig_threshold |
            valid_acf[valid_indices] < -sig_threshold
        } else {
          significant_lags <- logical(0)
        }
      }
      
      # Identify details about which lags are significant
      lag1_idx <- which(lags == 1)
      lag1_acf_val <- if (length(lag1_idx)) acf_values[lag1_idx] else NA_real_
      lag1_sig <- if (length(lag1_idx)) abs(lag1_acf_val) > sig_threshold else FALSE
      
      sig_lags <- if (length(significant_lags) && any(significant_lags, na.rm = TRUE)) {
        valid_lag_indices <- lag_indices[valid_indices]
        valid_lag_indices[significant_lags]
      } else integer(0)
      first_sig_lag <- if (length(sig_lags)) min(sig_lags) else NA_integer_
      
      max_sig_lag <- if (length(sig_lags)) {
        sig_acf <- acf_values[match(sig_lags, lags)]
        sig_lags[which.max(abs(sig_acf))]
      } else NA_integer_
      
      max_sig_acf <- if (!is.na(max_sig_lag)) acf_values[match(max_sig_lag, lags)] else NA_real_
      
      # Check if significant_lags is empty
      if (length(significant_lags) == 0) {
        warning(sprintf("Skipping site %s: no valid significant lags found", site))
        next
      }
      
      # Only proceed if we should include this site
      if (!only_autocorrelated || any(significant_lags, na.rm = TRUE)) {
        # Create summary
        summary_row <- data.frame(
          site = site,
          sample_size = n,
          significance_threshold = sig_threshold,
          lag1_acf = lag1_acf_val,
          lag1_significant = lag1_sig,
          is_autocorrelated = any(significant_lags, na.rm = TRUE),
          significant_lags = sum(significant_lags, na.rm = TRUE),
          first_sig_lag = first_sig_lag,
          max_sig_lag = max_sig_lag,
          max_sig_acf = max_sig_acf,
          lag1_strength = dplyr::case_when(
            is.na(lag1_acf_val) ~ "Unknown",
            abs(lag1_acf_val) > 0.4 ~ "Strong",
            abs(lag1_acf_val) > 0.2 ~ "Moderate",
            abs(lag1_acf_val) > 0.1 ~ "Weak",
            TRUE ~ "None"
          )
        )
        all_summaries[[length(all_summaries) + 1]] <- summary_row
      }
    }, error = function(e) {
      warning(sprintf("Error processing site %s: %s", site, e$message))
    })
  }
  
  # Combine all summaries and return
  if (length(all_summaries) > 0) {
    return(do.call(rbind, all_summaries))
  } else {
    # Return empty data frame with correct column structure if no sites meet criteria
    return(data.frame(
      site = character(),
      sample_size = numeric(),
      significance_threshold = numeric(),
      lag1_acf = numeric(),
      is_autocorrelated = logical(),
      significant_lags = numeric(),
      lag1_strength = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# =============================================================================
# AUTOCORRELATION TESTING FUNCTIONS
# =============================================================================

#' Test for serial autocorrelation in manual survey data
#' 
#' Tests for serial autocorrelation in manual snow survey data using
#' autocorrelation function (ACF) and Ljung-Box test.
#' 
#' @param data Data frame with manual survey data. Must contain columns:
#'   site, year, swe_cm, data_flag_1, data_flag_2, density, surface_type, activity
#' @param start_year Starting year for analysis
#' @param end_year Ending year for analysis
#' @param flags Data quality flags to exclude
#' @param hdensity High density threshold
#' @param ldensity Low density threshold
#' @param surface Surface types to include (e.g., "upland", "lake")
#' @param act Activity types to include (e.g., "A", "IA")
#' @param exclude_sites Sites to exclude from analysis
#' @param only_autocorrelated Logical, if TRUE only return autocorrelated sites
#' @return Data frame with autocorrelation test results including:
#'   \itemize{
#'     \item site: Site name
#'     \item n_years: Number of years of data
#'     \item lag: Vector of lag values
#'     \item acf: Vector of ACF values
#'     \item lb_lag: Lag used for Ljung-Box test
#'     \item p_value: P-value from Ljung-Box test
#'   }
#' @export
serial_autocorr_test <- function(data, start_year, end_year, flags, hdensity, ldensity, 
                                 surface, act, exclude_sites, only_autocorrelated = FALSE) {
  # First filter the data
  filtered_data <- data %>%
    dplyr::filter(
      year >= start_year,
      year <= end_year,
      data_flag_1 %!in% flags,
      data_flag_2 %!in% flags,
      is.na(density) | density < hdensity,
      is.na(density) | density > ldensity,
      surface_type %in% surface,
      activity %in% act
    ) %>%
    dplyr::group_by(site, year) %>%
    dplyr::summarize(meanswe = mean(swe_cm, na.rm = TRUE), .groups = 'drop')
  
  # Process each site
  results <- filtered_data %>%
    dplyr::group_by(site) %>%
    dplyr::summarize(
      n_years = dplyr::n_distinct(year),
      acf_result = list(acf(meanswe, plot = FALSE)),
      # Calculate Ljung-Box test with appropriate lag
      lb_lag = max(2, min(10, floor(n_years/4))),  # choose a reasonable joint lag
      p_value = tryCatch({
        Box.test(meanswe, type = "Ljung-Box", lag = lb_lag)$p.value
      }, error = function(e) {
        warning(sprintf("Ljung-Box test failed for site %s: %s", site, e$message))
        NA_real_
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      lag = purrr::map(acf_result, ~.x$lag) %>% purrr::map(unlist),
      acf = purrr::map(acf_result, ~.x$acf) %>% purrr::map(unlist)
    ) %>%
    dplyr::select(-acf_result)  # Remove the temporary acf_result column
  
  if (only_autocorrelated) {
    results <- results %>%
      dplyr::filter(p_value < 0.05)  # No need to unlist anymore
  }
  
  return(results)
}

#' Test for serial autocorrelation in CanSWE data
#' 
#' Tests for serial autocorrelation in CanSWE data using autocorrelation
#' function (ACF) and Ljung-Box test.
#' 
#' @param data Data frame with CanSWE data. Must contain columns: site, year, swe_cm
#' @param start_year Starting year for analysis
#' @param end_year Ending year for analysis
#' @param only_autocorrelated Logical, if TRUE only return autocorrelated sites
#' @return Data frame with autocorrelation test results (same structure as
#'   serial_autocorr_test)
#' @export
serial_autocorr_test_canswe <- function(data, start_year, end_year, min_year, only_autocorrelated = FALSE) {
  
  #filter data
  data <- data %>%
    dplyr::group_by(site) 
  
  filtered_data <- data %>%
    dplyr::filter(year >= start_year,
                  year <= end_year) %>%    
    dplyr::group_by(site) %>%
    dplyr::mutate(no_years = dplyr::n_distinct(year)) %>%
    dplyr::filter(no_years > min_year)
  
  # Process each site
  results <- filtered_data %>%
    dplyr::group_by(site) %>%
    dplyr::summarize(
      n_years = dplyr::n_distinct(year),
      acf_result = list(acf(swe_cm, plot = FALSE)),
      # Calculate Ljung-Box test with appropriate lag
      lb_lag = max(2, min(10, floor(n_years/4))),  # choose a reasonable joint lag
      p_value = tryCatch({
        Box.test(swe_cm, type = "Ljung-Box", lag = lb_lag)$p.value
      }, error = function(e) {
        warning(sprintf("Ljung-Box test failed for site %s: %s", site, e$message))
        NA_real_
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      lag = purrr::map(acf_result, ~.x$lag) %>% purrr::map(unlist),
      acf = purrr::map(acf_result, ~.x$acf) %>% purrr::map(unlist)
    ) %>%
    dplyr::select(-acf_result)  # Remove the temporary acf_result column
  
  if (only_autocorrelated) {
    results <- results %>%
      dplyr::filter(p_value < 0.05)  # No need to unlist anymore
  }
  
  return(results)
}

# =============================================================================
# PLOTTING AND ANALYSIS FUNCTIONS
# =============================================================================

#' Determine plotting style based on p-values
#' 
#' Returns a ggplot2 geom_smooth object with appropriate styling based on
#' significance levels from Mann-Kendall test results.
#' 
#' @param significance Significance level threshold
#' @param mk.value Mann-Kendall test results (vector with p-value at index 2)
#' @return ggplot2::geom_smooth object with appropriate color and size based on
#'   significance level
#' @export
p.value.function <- function(significance, mk.value){
  if (significance < 0.25 && significance > 0.1) { 
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    } else if (mk.value[2] < 0.1) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray60", size = 1)
    } else if (mk.value[2] < 0.25) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray80", size = 1)
    }
    
  } else if (significance >= 0.1) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    } else if (mk.value[2] < 0.1) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray60", size = 1)
    }
    
  } else if (significance >= 0.05) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    } else if (mk.value[2] < 0.05) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray40", size = 1.5)
    }
    
  } else if (significance <= 0.01) {
    
    if (mk.value[2] < 0.01) {
      ggplot2::geom_smooth(method = sen, se=TRUE, colour = "gray20", size = 2)  
    }
  }
}

#' Sen slope calculation wrapper
#' 
#' Wrapper function for median-based linear model (Sen slope) calculation.
#' 
#' @param ... Arguments passed to mblm::mblm
#' @param weights Optional weights parameter (not currently used)
#' @return Sen slope estimation result from mblm::mblm
#' @export
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

# =============================================================================
# Reusable functions for plotting ERA5 vs manual snow survey relationships 
# splitting up relationships based on basins 
# prep -> spatial join -> strict nearest fill -> metrics
# -> per-group stats -> standard plots and boxplots
# Always uses explicit namespaces (dplyr::, sf::, etc.)
# =============================================================================

# -----------------------------
# 0) Small helpers
# -----------------------------
fmt_p <- function(p) {
  if (is.na(p)) {
    "NA"
  } else if (p < 0.01) {
    "< 0.01"
  } else {
    paste0("= ", signif(p, 2))
  }
}

# pick x/y columns based on normalized vs raw
get_xy_cols <- function(normalized = TRUE) {
  if (isTRUE(normalized)) {
    list(x = "NormSen_slope", y = "NormMagnitude")
  } else {
    list(x = "Sen_slope", y = "Magnitude")
  }
}

# -----------------------------
# 1) Read + prep comparison table
# -----------------------------
prep_comparison_table <- function(
    csv_path,
    sen_scale = 10000,
    era5_mm_mult = 1000,
    manual_mm_mult = 10,
    drop_col = "X"
) {
  dt <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  
  dt <- dplyr::distinct(dt) %>%
    dplyr::mutate(
      Sen_slope      = .data$Sen_slope * sen_scale,
      ERA5_meanmax_mm   = .data$meanmaxswe * era5_mm_mult,
      Manual_meanmax_mm = .data$sitemeanswe * manual_mm_mult,
      NormSen_slope   = (.data$Sen_slope / .data$ERA5_meanmax_mm) * 100,
      NormMagnitude   = (.data$Magnitude / .data$Manual_meanmax_mm) * 100
    )
  
  if (!is.null(drop_col) && drop_col %in% names(dt)) {
    dt <- dplyr::select(dt, -dplyr::all_of(drop_col))
  }
  
  dt
}

# -----------------------------
# 2) Make sf points from a table
# -----------------------------
as_points_sf <- function(dt, lon_col = "Longitude", lat_col = "Latitude", crs) {
  sf::st_as_sf(
    dt,
    coords = c(lon_col, lat_col),
    crs = crs,
    remove = FALSE
  )
}

# -----------------------------
# 3) Attach a region label (ecozone/ecoregion/ecoprovince/basin) via polygon join
#    with strict nearest fill only within max_km for missing points
# -----------------------------
attach_region <- function(
    pts_sf,
    regions_sf,
    region_name_col,   # e.g. "ECOZONE_NA", "ECOREGION1", "ECOPROVI_1", "basin"
    max_km = 50,
    region_out_col = "region"
) {
  # ensure CRS match
  if (sf::st_crs(pts_sf) != sf::st_crs(regions_sf)) {
    pts_sf <- sf::st_transform(pts_sf, sf::st_crs(regions_sf))
  }
  
  # keep only name + geometry from regions
  regions_sf <- regions_sf %>%
    dplyr::select(dplyr::all_of(region_name_col)) %>%
    dplyr::rename(!!region_out_col := dplyr::all_of(region_name_col)) %>%
    sf::st_make_valid() %>%
    sf::st_zm(drop = TRUE, what = "ZM")
  
  # within join
  joined <- sf::st_join(pts_sf, regions_sf, join = sf::st_within, left = TRUE)
  
  # strict nearest for missing, only if within max_km
  missing <- is.na(joined[[region_out_col]])
  
  joined$dist_to_region_m <- NA_real_
  
  if (any(missing)) {
    pts_miss <- joined[missing, , drop = FALSE]
    
    nn <- sf::st_nearest_feature(pts_miss, regions_sf)
    d  <- sf::st_distance(pts_miss, regions_sf[nn, ], by_element = TRUE)
    
    ok <- as.numeric(d) <= (max_km * 1000)
    
    joined[[region_out_col]][missing] <- ifelse(
      ok,
      regions_sf[[region_out_col]][nn],
      NA_character_
    )
    
    joined$dist_to_region_m[missing] <- as.numeric(d)
  }
  
  joined
}

# -----------------------------
# 4) Add error metrics (raw + normalized)
# -----------------------------
add_error_metrics <- function(dt_or_sf) {
  dt_or_sf %>%
    dplyr::mutate(
      abs_err      = abs(.data$Sen_slope - .data$Magnitude),
      abs_err_norm = abs(.data$NormSen_slope - .data$NormMagnitude)
    )
}

# -----------------------------
# 5) Filter to groups with >= n_min observations (or distinct sites if provided)
# -----------------------------
filter_min_group_n <- function(
    dt,
    group_col,
    n_min = 3,
    distinct_id_col = NULL
) {
  g <- rlang::sym(group_col)
  
  if (is.null(distinct_id_col)) {
    dt %>%
      dplyr::group_by(!!g) %>%
      dplyr::filter(dplyr::n() >= n_min) %>%
      dplyr::ungroup()
  } else {
    id <- rlang::sym(distinct_id_col)
    dt %>%
      dplyr::group_by(!!g) %>%
      dplyr::filter(dplyr::n_distinct(!!id) >= n_min) %>%
      dplyr::ungroup()
  }
}

# -----------------------------
# 6) Per-group regression + correlation stats
#    Returns: n, slope, slope_p, r2, spearman_rho, spearman_p
# -----------------------------
group_stats <- function(
    dt,
    group_col,
    x_col,
    y_col,
    n_min = 3
) {
  g <- rlang::sym(group_col)
  x <- rlang::sym(x_col)
  y <- rlang::sym(y_col)
  
  dt %>%
    dplyr::filter(!is.na(!!g), !is.na(!!x), !is.na(!!y)) %>%
    dplyr::group_by(!!g) %>%
    dplyr::filter(dplyr::n() >= n_min) %>%
    dplyr::group_modify(function(.x, .y) {
      fit <- stats::lm(stats::as.formula(paste0(y_col, " ~ ", x_col)), data = .x)
      
      slope_row <- broom::tidy(fit) %>%
        dplyr::filter(.data$term == x_col)
      
      gl <- broom::glance(fit)
      
      ct <- stats::cor.test(.x[[x_col]], .x[[y_col]], method = "spearman")
      
      dplyr::tibble(
        n             = nrow(.x),
        slope         = slope_row$estimate,
        slope_p_value = slope_row$p.value,
        r2            = gl$r.squared,
        rho_spearman  = unname(ct$estimate),
        rho_p_value   = ct$p.value
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$n))
}

# -----------------------------
# 7) Scatter with separate regression per group (and 1:1 line)
# -----------------------------
plot_scatter_by_group <- function(
    dt,
    group_col,
    normalized = TRUE,
    shared_limits = NULL,
    xlab = NULL,
    ylab = NULL,
    se = FALSE
) {
  xy <- get_xy_cols(normalized = normalized)
  
  if (is.null(shared_limits)) {
    shared_limits <- range(c(dt[[xy$x]], dt[[xy$y]]), na.rm = TRUE)
  }
  
  if (is.null(xlab)) {
    xlab <- if (normalized) "% change in ERA5-Land SWE/decade" else "ERA5 trend (Sen's slope)"
  }
  if (is.null(ylab)) {
    ylab <- if (normalized) "% change in manual SWE/decade" else "Manual trend (Magnitude)"
  }
  
  ggplot2::ggplot(
    dt,
    ggplot2::aes(
      x = .data[[xy$x]],
      y = .data[[xy$y]]
    )
  ) +
    ggplot2::geom_smooth(
      ggplot2::aes(color = .data[[group_col]]),
      method = "lm",
      se = se,
      linewidth = 1,
      alpha = 0.25
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data[[group_col]]),
      size = 6,
      shape = 21,
      color = "black",
      stroke = 1,
      alpha = 0.9
    ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dotted",
      color = "black",
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(limits = shared_limits) +
    ggplot2::scale_y_continuous(limits = shared_limits) +
    ggplot2::labs(x = xlab, y = ylab, fill = NULL, color = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, linewidth = 1, color = "black"),
      axis.text  = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 18)
    )
}

# -----------------------------
# 8) Boxplots for mean SWE (mm), trends, and error metrics
# -----------------------------
make_long_for_boxplots <- function(
    dt,
    group_col,
    normalized = TRUE
) {
  xy <- get_xy_cols(normalized = normalized)
  
  # error variable depends on normalized
  err_col <- if (normalized) "abs_err_norm" else "abs_err"
  
  # SWE and trend in a long format for boxplots
  swe_long <- dt %>%
    dplyr::select(
      dplyr::all_of(group_col),
      .data$ERA5_meanmax_mm,
      .data$Manual_meanmax_mm
    ) %>%
    tidyr::pivot_longer(
      cols = c("ERA5_meanmax_mm", "Manual_meanmax_mm"),
      names_to = "source",
      values_to = "meanmax_swe_mm"
    ) %>%
    dplyr::mutate(
      source = dplyr::recode(
        .data$source,
        ERA5_meanmax_mm   = "ERA5-Land",
        Manual_meanmax_mm = "Manual surveys"
      )
    )
  
  trend_long <- dt %>%
    dplyr::select(
      dplyr::all_of(group_col),
      dplyr::all_of(xy$x),
      dplyr::all_of(xy$y)
    ) %>%
    tidyr::pivot_longer(
      cols = c(dplyr::all_of(xy$x), dplyr::all_of(xy$y)),
      names_to = "series",
      values_to = "trend"
    ) %>%
    dplyr::mutate(
      source = dplyr::if_else(.data$series == xy$x, "ERA5-Land", "Manual surveys")
    ) %>%
    dplyr::select(dplyr::all_of(group_col), .data$source, .data$trend)
  
  err_long <- dt %>%
    dplyr::select(dplyr::all_of(group_col), dplyr::all_of(err_col)) %>%
    dplyr::rename(abs_error = dplyr::all_of(err_col))
  
  list(swe_long = swe_long, trend_long = trend_long, err_long = err_long)
}

plot_box_swe <- function(swe_long, group_col) {
  ggplot2::ggplot(
    swe_long,
    ggplot2::aes(x = .data[[group_col]], y = .data$meanmax_swe_mm, fill = .data$source)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = "Mean end-of-season SWE (mm)", fill = NULL)
}

plot_box_trend <- function(trend_long, group_col, normalized = TRUE) {
  ylab <- if (normalized) "Trend (%/decade)" else "Trend (original units)"
  ggplot2::ggplot(
    trend_long,
    ggplot2::aes(x = .data[[group_col]], y = .data$trend, fill = .data$source)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = ylab, fill = NULL)
}

plot_box_error <- function(err_long, group_col, normalized = TRUE) {
  ylab <- if (normalized) "|% ERA5-L slope − % Manual SS slope|" else "|ERA5-L slope − Manual SS slope|"
  ggplot2::ggplot(
    err_long,
    ggplot2::aes(x = .data[[group_col]], y = .data$abs_error)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0) +
    ggplot2::geom_jitter() +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = ylab)

}

# -----------------------------
# 9) Means table for SWE / trends / error (by group and source)
# -----------------------------
summarize_means <- function(
    swe_long,
    trend_long,
    err_long,
    group_col
) {
  means_swe <- swe_long %>%
    dplyr::group_by(.data[[group_col]], .data$source) %>%
    dplyr::summarise(
      n = sum(!is.na(.data$meanmax_swe_mm)),
      mean_meanmax_swe_mm = mean(.data$meanmax_swe_mm, na.rm = TRUE),
      .groups = "drop"
    )
  
  means_trend <- trend_long %>%
    dplyr::group_by(.data[[group_col]], .data$source) %>%
    dplyr::summarise(
      n = sum(!is.na(.data$trend)),
      mean_trend = mean(.data$trend, na.rm = TRUE),
      .groups = "drop"
    )
  
  means_error <- err_long %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      n = sum(!is.na(.data$abs_error)),
      mean_abs_error = mean(.data$abs_error, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(means_swe = means_swe, means_trend = means_trend, means_error = means_error)
}

# -----------------------------
# 10) Define a function to calculate the Haversine distance (for closest lat/long match)
# -----------------------------

haversine_dist <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Earth radius in kilometers
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c # Distance in km
}


# -----------------------------
# 11) Create a multi basin polygon for basin-averaging
# -----------------------------

as_basin_sf <- function(x, name) {
  # If x is geometry-only (sfc), wrap it
  if (inherits(x, "sfc")) {
    x <- sf::st_sf(basin = name, geometry = x)
  } else {
    x <- x %>% dplyr::mutate(basin = name)
  }
  
  x %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::select(basin, geometry)
}
