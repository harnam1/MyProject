#' Perform Uniformity Test and Copula Fitting
#'
#' This function takes uniform data transformed using Pearson distributions and
#' fits various copulas (t, Gumbel, Frank, Clayton, Gaussian). It accepts a
#' \code{flow_cols} argument for multiple columns and maps numeric Pearson types
#' to Roman numeral strings. Data validation is performed to ensure inputs are valid.
#'
#' @param data A data frame of flows.
#' @param flow_cols A character vector of column names (e.g., \code{c("parkerdam","greenriver",...)}).
#' @param params_list A list of parameter sets for each flow column. Each element
#'   should have \code{type} (numeric or character), plus the necessary parameters
#'   for that type.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{AD_Results}: A named list of AD test results (one per column).
#'   \item \code{Copula_Results}: A list of fitted copulas and statistics.
#'   \item \code{Summary}: A data frame of copula fits (AIC, BIC, etc.).
#' }
#'
#' @importFrom copula fitCopula tCopula gumbelCopula frankCopula claytonCopula normalCopula
#' @importFrom ADGofTest ad.test
#' @importFrom stats AIC BIC logLik punif
#' @importFrom utils combn
#' @importFrom PearsonDS ppearson0 ppearsonI ppearsonII ppearsonIII ppearsonIV ppearsonV ppearsonVI ppearsonVII
#' @export
analyze_copulas <- function(data, flow_cols, params_list) {

  # Validate that data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }

  # Validate that all flow_cols exist in data
  missing_cols <- setdiff(flow_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Validate that params_list has entries for all flow_cols
  missing_params <- setdiff(flow_cols, names(params_list))
  if (length(missing_params) > 0) {
    stop("Missing fitted parameters for: ", paste(missing_params, collapse = ", "))
  }

  # Helper: Map numeric Pearson type to Roman numeral string
  map_numeric_to_roman <- function(num_type) {
    if (is.character(num_type)) return(num_type)
    switch(as.character(num_type),
           "0" = "0",
           "1" = "I",
           "2" = "II",
           "3" = "III",
           "4" = "IV",
           "5" = "V",
           "6" = "VI",
           "7" = "VII",
           stop("Unknown numeric Pearson type: ", num_type))
  }

  # Helper: Choose the appropriate Pearson CDF based on type
  pick_pearson_cdf <- function(x, pearson_info) {
    real_type <- map_numeric_to_roman(pearson_info$type)
    switch(
      real_type,
      "0" = ppearson0(x, mean = pearson_info$mean, sd = pearson_info$sd, lower.tail = TRUE, log.p = FALSE),
      "I" = ppearsonI(x, a = pearson_info$a, b = pearson_info$b, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "II" = ppearsonII(x, a = pearson_info$a, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "III" = ppearsonIII(x, shape = pearson_info$shape, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "IV" = ppearsonIV(x, m = pearson_info$m, nu = pearson_info$nu, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "V" = ppearsonV(x, shape = pearson_info$shape, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "VI" = ppearsonVI(x, a = pearson_info$a, b = pearson_info$b, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      "VII" = ppearsonVII(x, df = pearson_info$df, location = pearson_info$location, scale = pearson_info$scale, lower.tail = TRUE, log.p = FALSE),
      stop("Unsupported Pearson type: ", real_type)
    )
  }

  # Create uniform variables for each column in flow_cols
  uniform_list <- lapply(flow_cols, function(col) {
    transformed <- pick_pearson_cdf(data[[col]], params_list[[col]])

    # Check for NA or Inf immediately after transformation for each column
    if (any(is.na(transformed)) || any(is.infinite(transformed))) {
      stop("Transformation resulted in NA or infinite values")
    }

    # Check for out of range values immediately
    if (any(transformed < 0, na.rm = TRUE) || any(transformed > 1, na.rm = TRUE)) {
      stop("Uniform data out of [0, 1] range.")
    }

    return(transformed)
  })

  names(uniform_list) <- flow_cols
  data_uniform <- do.call(cbind, uniform_list)

  # Perform AD tests for each column
  ad_results <- lapply(flow_cols, function(col) {
    ADGofTest::ad.test(uniform_list[[col]], punif)
  })
  names(ad_results) <- flow_cols

  # Helper: Fit and evaluate a given copula model
  fit_and_evaluate2 <- function(copula_model, data_mat, copula_name) {
    fit <- fitCopula(copula_model, data_mat, method = "ml")
    loglik_total <- as.numeric(logLik(fit))
    aic <- AIC(fit)
    bic <- BIC(fit)
    list(Copula = copula_name, LogLikelihood = loglik_total, AIC = aic, BIC = bic, Parameters = fit@estimate, Fit = fit)
  }

  # Set copula dimension and define copulas
  d <- length(flow_cols)

  # Validate that the dimension > 2
  if (d < 2) {
    # Return just the AD results with empty copula info when dimension is insufficient
    warning("Copula fitting requires at least 2 dimensions, skipping copula analysis.")
    return(list(
      AD_Results = ad_results,
      Copula_Results = list(),
      Summary = data.frame(
        Copula = character(),
        LogLikelihood = numeric(),
        AIC = numeric(),
        BIC = numeric()
      )
    ))
  }

  # Continue Fitting Copulas
  copula_list <- list(
    t = tCopula(dim = d),
    gumbel = gumbelCopula(dim = d),
    frank = frankCopula(dim = d),
    clayton = claytonCopula(dim = d),
    gaussian = normalCopula(dim = d)
  )

  # Fit all copulas
  all_results <- lapply(names(copula_list), function(name) {
    fit_and_evaluate2(copula_list[[name]], data_uniform, paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), " Copula"))
  })

  # Create summary table
  summary_table <- data.frame(
    Copula = sapply(all_results, function(x) x$Copula),
    LogLikelihood = sapply(all_results, function(x) x$LogLikelihood),
    AIC = sapply(all_results, function(x) x$AIC),
    BIC = sapply(all_results, function(x) x$BIC)
  )

  # Return final results
  list(AD_Results = ad_results, Copula_Results = all_results, Summary = summary_table)
}




#' Compute Conditional Survival Probability from Fitted Copulas
#'
#' Computes \eqn{P(X_{i_0} > x_{i_0} \mid X_{-i_0} > x_{-i_0}) =
#' \frac{P(X_1 > x_1, \dots, X_d > x_d)}{P(X_{-i_0} > x_{-i_0})}},
#' where \eqn{X_{-i_0}} represents the variables in the conditioning set.
#' Internally, this function uses a nested helper \code{pSurvivalCopula()}
#' to compute multivariate survival probabilities via inclusion–exclusion on
#' the fitted copula. Additional data validation is performed to ensure
#' the correct structure of inputs, including named \code{thresholds}
#' and the presence of the specified \code{copula_name} in
#' \code{analysis_results}.
#'
#' @param analysis_results The output list from \code{analyze_copulas()},
#'   which must contain \code{$Copula_Results} with one or more fitted copulas.
#' @param copula_name A string specifying which copula to use from
#'   \code{analysis_results$Copula_Results} (e.g., \emph{"Gumbel Copula"}).
#' @param thresholds A named numeric vector of thresholds for each variable
#'   (e.g., \code{c(parkerdam = 10000, greenriver = 9000)}). The names must
#'   match those in \code{params_list} and correspond to columns in the data.
#' @param params_list A list of Pearson parameter sets corresponding to each
#'   variable, as produced by \code{fit_pearson_params()}.
#' @param cond_indices An integer vector specifying the indices of variables
#'   in the conditioning set (e.g., \code{2:4}).
#'
#' @return A numeric value: the conditional survival probability.
#'
#' @importFrom copula pCopula gumbelCopula claytonCopula frankCopula normalCopula tCopula
#' @export
compute_conditional_survival <- function(
    analysis_results,
    copula_name,
    thresholds,
    params_list,
    cond_indices
) {
  # Validate analysis_results
  if (!is.list(analysis_results) || !"Copula_Results" %in% names(analysis_results)) {
    stop("analysis_results must be a list containing $Copula_Results.")
  }

  # Validate copula_name presence
  cr_list <- analysis_results$Copula_Results
  match_idx <- which(sapply(cr_list, function(x) x$Copula) == copula_name)
  if (length(match_idx) == 0) {
    stop("Could not find copula named '", copula_name, "' in analysis_results.")
  }

  # Validate thresholds
  if (!is.numeric(thresholds) || is.null(names(thresholds))) {
    stop("thresholds must be a named numeric vector.")
  }

  # Validate params_list and index existence
  for (nm in names(thresholds)) {
    if (!nm %in% names(params_list)) {
      stop("No Pearson parameters found for '", nm, "' in params_list.")
    }
  }
  if (!is.numeric(cond_indices) || any(cond_indices <= 0)) {
    stop("cond_indices must be a vector of positive integer indices.")
  }

  # Extract the fitted copula
  this_fit <- cr_list[[match_idx]]$Fit
  full_copula <- this_fit@copula

  # Helper: pSurvivalCopula
  pSurvivalCopula <- function(u, cop) {
    # Check dimension consistency
    d_cop <- dim(cop)
    d_u   <- length(u)
    if (d_u != d_cop) {
      stop(sprintf("Dimension mismatch: length(u)=%d but copula dimension=%d.", d_u, d_cop))
    }
    total <- 0
    # Inclusion–exclusion loop
    for (k in 0:d_cop) {
      sign <- (-1)^k
      if (k == 0) {
        total <- total + sign * copula::pCopula(rep(1, d_cop), cop)
      } else {
        combos <- combn(d_cop, k)
        sum_k  <- 0
        for (col in 1:ncol(combos)) {
          idx_subset <- combos[, col]
          vs <- rep(1, d_cop)
          vs[idx_subset] <- u[idx_subset]
          sum_k <- sum_k + copula::pCopula(vs, cop)
        }
        total <- total + sign * sum_k
      }
    }
    total
  }

  # Helpers to map Pearson type and pick CDF
  map_numeric_to_roman <- function(num_type) {
    if (is.character(num_type)) return(num_type)
    switch(
      as.character(num_type),
      "0" = "0",
      "1" = "I",
      "2" = "II",
      "3" = "III",
      "4" = "IV",
      "5" = "V",
      "6" = "VI",
      "7" = "VII",
      stop("Unknown numeric Pearson type:", num_type)
    )
  }

  pick_pearson_cdf <- function(x, pearson_info) {
    real_type <- map_numeric_to_roman(pearson_info$type)
    switch(
      real_type,
      "0"   = ppearson0(x, mean = pearson_info$mean, sd = pearson_info$sd,
                        lower.tail = TRUE, log.p = FALSE),
      "I"   = ppearsonI(x, a = pearson_info$a, b = pearson_info$b,
                        location = pearson_info$location, scale = pearson_info$scale,
                        lower.tail = TRUE, log.p = FALSE),
      "II"  = ppearsonII(x, a = pearson_info$a,
                         location = pearson_info$location, scale = pearson_info$scale,
                         lower.tail = TRUE, log.p = FALSE),
      "III" = ppearsonIII(x, shape = pearson_info$shape,
                          location = pearson_info$location, scale = pearson_info$scale,
                          lower.tail = TRUE, log.p = FALSE),
      "IV"  = ppearsonIV(x, m = pearson_info$m, nu = pearson_info$nu,
                         location = pearson_info$location, scale = pearson_info$scale,
                         lower.tail = TRUE, log.p = FALSE),
      "V"   = ppearsonV(x, shape = pearson_info$shape,
                        location = pearson_info$location, scale = pearson_info$scale,
                        lower.tail = TRUE, log.p = FALSE),
      "VI"  = ppearsonVI(x, a = pearson_info$a, b = pearson_info$b,
                         location = pearson_info$location, scale = pearson_info$scale,
                         lower.tail = TRUE, log.p = FALSE),
      "VII" = ppearsonVII(x, df = pearson_info$df,
                          location = pearson_info$location, scale = pearson_info$scale,
                          lower.tail = TRUE, log.p = FALSE),
      stop("Unsupported Pearson type in compute_conditional_survival.")
    )
  }

  # Convert thresholds to u-scale
  u_names <- names(thresholds)
  u_vals <- numeric(length(u_names))
  for (j in seq_along(u_names)) {
    colname <- u_names[j]
    xj <- thresholds[colname]
    pearson_info <- params_list[[colname]]
    u_vals[j] <- pick_pearson_cdf(xj, pearson_info)
    if (u_vals[j] < 0 || u_vals[j] > 1) {
      stop("Value out of [0, 1] range after Pearson transformation for ", colname)
    }
  }

  # Numerator: joint survival for all variables
  num <- pSurvivalCopula(u_vals, full_copula)

  # Build sub-copula for conditioning set
  sub_dim <- length(cond_indices)
  fitted_params <- cr_list[[match_idx]]$Parameters
  cop_class <- class(full_copula)

  if ("gumbelCopula" %in% cop_class) {
    sub_copula <- gumbelCopula(param = fitted_params, dim = sub_dim)
  } else if ("claytonCopula" %in% cop_class) {
    sub_copula <- claytonCopula(param = fitted_params, dim = sub_dim)
  } else if ("frankCopula" %in% cop_class) {
    sub_copula <- frankCopula(param = fitted_params, dim = sub_dim)
  } else if ("normalCopula" %in% cop_class) {
    sub_copula <- normalCopula(param = fitted_params, dim = sub_dim)
  } else {
    stop("Unsupported copula family in compute_conditional_survival.")
  }

  # Denominator: survival probability of conditioning set
  u_cond <- u_vals[cond_indices]
  denom <- pSurvivalCopula(u_cond, sub_copula)

  # Return conditional survival probability
  cond_prob <- num / denom
  cond_prob
}











