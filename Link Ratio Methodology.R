#############################################################
# Link Ratio Methods for IBNR Reserve Calculation
# Implementation of techniques from:
# The Faculty and Institute of Actuaries Claims Reserving Manual
# Volume 1, Chapter E (Pages 161-182), 1997 Edition
#
# R implementation by: Jericott Lujan
# Dependencies: ChainLadder package
#############################################################

library(ChainLadder)

# ---------------------------
# DATA PREPARATION
# ---------------------------

#' Load example triangle from Claims Reserving Manual (E5.1)
#' 
#' @return A cumulative claims triangle matrix
#' @references Claims Reserving Manual V1, p.161

incremental_data <- matrix(
  c(1001, 854, 568, 565, 347, 148,
    1113, 990, 671, 648, 422, NA,
    1265, 1168, 800, 744, NA, NA,
    1490, 1383, 1007, NA, NA, NA,
    1725, 1536, NA, NA, NA, NA,
    1889, NA, NA, NA, NA, NA),
  nrow = 6, byrow = TRUE
)

# Set row and column names
rownames(incremental_data) <- 0:5
colnames(incremental_data) <- 0:5

# Convert to triangle object and accumulate
incremental_triangle <- as.triangle(incremental_data, 
                                    origin = "origin", 
                                    dev = "dev")
cumulative_triangle <- incr2cum(incremental_triangle)

# Known ultimate for the oldest accident year
known_ultimate <- 3705

# ---------------------------------------
## LINK RATIO METHODS - INTRODUCTION
# ---------------------------------------

#' Calculate development factors from a claims triangle
#' Based on Claims Reserving Manual (1997), Volume 1, Chapter E5.

calculate_dev_factors <- function(tri) {
  lapply(1:nrow(tri), function(i) {
    row <- unname(tri[i, ])
    ratios <- row[-1] / row[-length(row)]
  })
}

# -------------------------------
## LINK RATIO - WORST CASE METHOD
# -------------------------------

#' Calculates IBNR using maximum observed link ratios (most conservative approach)
#' 
#' @param tri A cumulative claims triangle
#' @param ultimate Known ultimate for oldest accident year
#' @return Data frame with IBNR results
#' @references Claims Reserving Manual V1, p.161

calculate_worst_case_ibnr <- function(tri, ultimate) {
  
  dev_ratios <- calculate_dev_factors(tri)
  
  # Calculate maximum ratios for each development period
  max_ratios <- sapply(1:max(lengths(dev_ratios)), function(j) {
    max(sapply(dev_ratios, function(x) x[j]), na.rm = TRUE)
  })
  
  # Calculate ultimate ratio
  last_known <- tail(na.omit(unname(tri[1, ])), 1)
  ultimate_ratio <- ultimate / last_known
  
  # Combine and calculate cumulative ratios
  all_ratios <- c(max_ratios, ultimate_ratio)
  cum_ratios <- cumprod(rev(all_ratios))
  
  # Get latest diagonal values
  diagonales <- list()
  for (i in 1:ncol(tri)) {
    fila <- unname(tri[i,])
    fila <- fila[!is.na(fila)]
    diagonales[[i]] <- fila[length(fila)]
  }
  diagonal <- sapply(diagonales, function(x) x[1])
  
  # Prepare results
  results <- data.frame(index=as.numeric(rownames(tri)))
  results$Diagonal <- diagonal
  results$FDA <- cum_ratios
  results$Ultimate <- results$Diagonal * results$FDA
  results$IBNR <- results$Ultimate - results$Diagonal
  
  return(results)
}

worst_case_results <- calculate_worst_case_ibnr(cumulative_triangle, known_ultimate)
print("Worst Case Link Ratio Results:")
print(worst_case_results)

# -------------------------------
## LINK RATIO - AVERAGE METHOD
# -------------------------------

#' Calculates IBNR using average link ratios
#' #' Based on Claims Reserving Manual (1997), Volume 1, Chapter E6.
#' 
#' @param tri A cumulative claims triangle
#' @param ultimate Known ultimate for oldest accident year
#' @return Data frame with IBNR results
#' @references Claims Reserving Manual V1, p.167

calculate_average_ibnr <- function(tri, ultimate) {
  
  dev_ratios <- calculate_dev_factors(tri)
  
  # Calculate average ratios for each development period
  avg_ratios <- sapply(1:max(lengths(dev_ratios)), function(j) {
    mean(sapply(dev_ratios, function(x) x[j]), na.rm = TRUE)
  })
  
  # Calculate ultimate ratio
  last_known <- tail(na.omit(unname(tri[1, ])), 1)
  ultimate_ratio <- ultimate / last_known
  
  # Combine and calculate cumulative ratios
  all_ratios <- c(avg_ratios, ultimate_ratio)
  cum_ratios <- cumprod(rev(all_ratios))
  
  # Get latest diagonal values
  diagonales <- list()
  for (i in 1:ncol(tri)) {
    fila <- unname(tri[i,])
    fila <- fila[!is.na(fila)]
    diagonales[[i]] <- fila[length(fila)]
  }
  diagonal <- sapply(diagonales, function(x) x[1])
  
  # Prepare results
  results <- data.frame(index=as.numeric(rownames(tri)))
  results$Diagonal <- diagonal
  results$FDA <- cum_ratios
  results$Ultimate <- results$Diagonal * results$FDA
  results$IBNR <- results$Ultimate - results$Diagonal
  
  return(results)
}

average_results <- calculate_average_ibnr(cumulative_triangle, known_ultimate)
print("Average Link Ratio Results:")
print(average_results)

# -------------------------------
## CHAIN LADDER - ORIGINAL WEIGHTS
# -------------------------------

#' Calculates IBNR using Chain Ladder with original weights (volume-weighted)
#' #' Based on Claims Reserving Manual (1997), Volume 1, Chapter E8.
#' 
#' @param tri A cumulative claims triangle
#' @param ultimate Known ultimate for oldest accident year
#' @return Data frame with IBNR results
#' @references Claims Reserving Manual V1, p.173


calculate_chainladder_ibnr <- function(tri, ultimate) {
  dev_ratios <- calculate_dev_factors(tri)
  
  # Calculate weighted ratios for each development period
  weighted_ratios <- sapply(1:max(lengths(dev_ratios)), function(j) {
    col_ratios <- sapply(dev_ratios, function(x) x[j])
    col_values <- tri[, j][!is.na(col_ratios)]
    col_ratios <- col_ratios[!is.na(col_ratios)]
    sum(col_ratios * col_values) / sum(col_values)
  })
  
  # Calculate ultimate ratio
  last_known <- tail(na.omit(unname(tri[1, ])), 1)
  ultimate_ratio <- ultimate / last_known
  weighted_ratios <- weighted_ratios
  
  # Combine and calculate cumulative ratios
  all_ratios <- c(weighted_ratios, ultimate_ratio)
  cum_ratios <- cumprod(rev(all_ratios))
  
  # Get latest diagonal values
  diagonales <- list()
  for (i in 1:ncol(tri)) {
    fila <- unname(tri[i,])
    fila <- fila[!is.na(fila)]
    diagonales[[i]] <- fila[length(fila)]
  }
  diagonal <- sapply(diagonales, function(x) x[1])
  
  # Prepare results
  results <- data.frame(index=as.numeric(rownames(tri)))
  results$Diagonal <- diagonal
  results$FDA <- cum_ratios
  results$Ultimate <- results$Diagonal * results$FDA
  results$IBNR <- results$Ultimate - results$Diagonal
  
  return(results)
  
}

chainladder_results <- calculate_chainladder_ibnr(cumulative_triangle, known_ultimate)
print("Chain Ladder (Weighted) Results:")
print(chainladder_results)

# -------------------------------
## LINK RATIO - TRENDING METHOD  
# -------------------------------

#' Calculates IBNR using trending link ratios with least squares projection
#' #' Based on Claims Reserving Manual (1997), Volume 1, Chapter E9.
#' 
#' @param tri A cumulative claims triangle
#' @param ultimate Known ultimate for oldest accident year
#' @return Data frame with IBNR results
#' @references Claims Reserving Manual V1, p.177-182

calculate_trending_ibnr <- function(tri, ultimate) {
  
  dev_ratios <- calculate_dev_factors(tri)
  ratio_factor <- list()
  
  # Project ratios with trend
  for (i in 1:length(dev_ratios)) {
    col <- sapply(dev_ratios, function(x) x[i])
    yi <- col[!is.na(col)]
    n <- length(yi)
    
    if (n > 2) {
      xi <- seq(0, n - 1) - (n - 1) / 2
      c <- mean(yi)
      b <- sum(xi * yi)/sum(xi^2)
      x_vals <- xi[length(xi)] + seq(1, i) 
      ratios <- b * x_vals + c
      ratio_factor[[i]] <- rev(ratios)
    } else if (n > 0) {
      ratio_factor[[i]] <- rep(max(yi), i)
    } else {
      last_known <- tail(na.omit(unname(tri[1, ])), 1)
      ultimate_ratio <- ultimate / last_known
      ratio_factor[[i]] <- rep(ultimate_ratio, i)
    }
  }
  
  # Calculate cumulative factors
  factor_acum <- list()
  for (i in 1:length(ratio_factor)) {
    col <- sapply(ratio_factor, function(x) x[i])
    col <- col[!is.na(col)]
    factor_acum[[i]] <- prod(col)
  }
  fa_ac <- sapply(factor_acum, function(x) x[1])
  
  # Get latest diagonal values
  diagonales <- list()
  for (i in 1:ncol(tri)) {
    fila <- unname(tri[i,])
    fila <- fila[!is.na(fila)]
    diagonales[[i]] <- fila[length(fila)]
  }
  diagonal <- rev(sapply(diagonales, function(x) x[1]))
  
  # Prepare results
  results <- data.frame(index=as.numeric(rownames(tri)))
  results$Diagonal <- diagonal
  results$FDA <- fa_ac
  results$Ultimate <- results$Diagonal * results$FDA
  results$IBNR <- results$Ultimate - results$Diagonal
  
  return(results)
}

trending_results <- calculate_trending_ibnr(cumulative_triangle, known_ultimate)
print("Link Ratio - Trending Method Results:")
print(trending_results)

# ---------------------
## RESULTS COMPARISION
# ---------------------

results_comparison <- data.frame(
  Method = c("Worst Case", "Average", "Chain Ladder", "Trending"),
  Total_Ultimate = c(
    sum(worst_case_results$Ultimate),
    sum(average_results$Ultimate),
    sum(chainladder_results$Ultimate),
    sum(trending_results$Ultimate)
  ),
  Total_IBNR = c(
    sum(worst_case_results$IBNR),
    sum(average_results$IBNR),
    sum(chainladder_results$IBNR),
    sum(trending_results$IBNR)
  )
)

print("Comparison of Link Ratio Methods:")
print(results_comparison)



