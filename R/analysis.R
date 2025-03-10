#' Perform Uniformity Test and Copula Fitting
#'
#' This function takes uniform data transformed using Pearson distributions and
#' fits various copulas (t, Gumbel, Frank, Clayton, Gaussian). It adds minimal
#' modifications to the original code:
#' \enumerate{
#'   \item Accepts a \code{flow_cols} argument for multiple columns
#'   \item Maps numeric Pearson types to Roman numeral strings (e.g. 4 -> "IV")
#' }
#'
#' @param data A data frame of flows.
#' @param flow_cols A character vector of column names (e.g., \code{c("parkerdam","greenriver",...)})
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
#' @importFrom PearsonDS ppearson0 ppearsonI ppearsonII ppearsonIII ppearsonIV ppearsonV ppearsonVI ppearsonVII
#' @export
analyze_copulas <- function(data, flow_cols, params_list) {

  #############################################################################
  # 1. Helper: Convert numeric Pearson type -> Roman numeral string
  #############################################################################
  map_numeric_to_roman <- function(num_type) {
    # Extend/modify this map if needed:
    # 0 -> "0", 1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII"
    # If 'num_type' is already a string, just return it as is:
    if (is.character(num_type)) return(num_type)
    # If numeric:
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

  #############################################################################
  # 2. Helper: Pick the correct Pearson CDF
  #############################################################################
  pick_pearson_cdf <- function(x, pearson_info) {

    # Possibly convert numeric type to string
    real_type <- map_numeric_to_roman(pearson_info$type)

    # Switch on the final string-based type
    switch(
      real_type,
      "0" = ppearson0(
        x,
        mean       = pearson_info$mean,
        sd         = pearson_info$sd,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "I" = ppearsonI(
        x,
        a          = pearson_info$a,
        b          = pearson_info$b,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "II" = ppearsonII(
        x,
        a          = pearson_info$a,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "III" = ppearsonIII(
        x,
        shape      = pearson_info$shape,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "IV" = ppearsonIV(
        x,
        m          = pearson_info$m,
        nu         = pearson_info$nu,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "V" = ppearsonV(
        x,
        shape      = pearson_info$shape,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "VI" = ppearsonVI(
        x,
        a          = pearson_info$a,
        b          = pearson_info$b,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      "VII" = ppearsonVII(
        x,
        df         = pearson_info$df,
        location   = pearson_info$location,
        scale      = pearson_info$scale,
        lower.tail = TRUE,
        log.p      = FALSE
      ),
      stop("Unsupported Pearson type: ", real_type)
    )
  }

  #############################################################################
  # 3. Create uniform variables for each column in flow_cols (original style)
  #############################################################################
  uniform_list <- lapply(flow_cols, function(col) {
    pick_pearson_cdf(data[[col]], params_list[[col]])
  })
  names(uniform_list) <- flow_cols

  data_uniform <- do.call(cbind, uniform_list)

  #############################################################################
  # 4. Perform AD tests for each column
  #    same structure as the original, just a small loop
  #############################################################################
  ad_results <- list()
  for (col in flow_cols) {
    ad_results[[col]] <- ADGofTest::ad.test(uniform_list[[col]], punif)
  }

  #############################################################################
  # 5. Internal function to fit and evaluate copulas
  #############################################################################
  fit_and_evaluate2 <- function(copula_model, data_mat, copula_name) {
    fit <- fitCopula(copula_model, data_mat, method = "ml")
    loglik_total <- as.numeric(logLik(fit))
    aic <- AIC(fit)
    bic <- BIC(fit)
    k <- attr(logLik(fit), "df")

    list(
      Copula        = copula_name,
      LogLikelihood = loglik_total,
      AIC           = aic,
      BIC           = bic,
      Parameters    = fit@estimate,
      Fit           = fit
    )
  }

  #############################################################################
  # 6. Define copulas (default: dimension = length(flow_cols))
  #    If you want to fix dim=4, you can revert to 4.
  #############################################################################
  d <- length(flow_cols)
  copula_list <- list(
    t        = tCopula(dim = d),
    gumbel   = gumbelCopula(dim = d),
    frank    = frankCopula(dim = d),
    clayton  = claytonCopula(dim = d),
    gaussian = normalCopula(dim = d)
  )

  #############################################################################
  # 7. Fit all copulas
  #############################################################################
  all_results <- lapply(names(copula_list), function(name) {
    fit_and_evaluate2(
      copula_list[[name]],
      data_uniform,
      paste0(
        toupper(substr(name, 1, 1)),
        substr(name, 2, nchar(name)),
        " Copula"
      )
    )
  })

  #############################################################################
  # 8. Create summary
  #############################################################################
  summary_table <- data.frame(
    Copula = sapply(all_results, function(x) x$Copula),
    LogLikelihood = sapply(all_results, function(x) x$LogLikelihood),
    AIC = sapply(all_results, function(x) x$AIC),
    BIC = sapply(all_results, function(x) x$BIC)
  )

  #############################################################################
  # 9. Return final results
  #############################################################################
  list(
    AD_Results    = ad_results,
    Copula_Results= all_results,
    Summary       = summary_table
  )
}





pSurvivalCopula <- function(u, cop) {
  # 1) Sanity check: dimension consistency
  d_cop <- dim(cop)    # The dimension of the copula (e.g., 4 for a 4D copula)
  d_u   <- length(u)   # The length of the input vector u
  if (d_u != d_cop) {
    stop(sprintf("Dimension mismatch: length(u)=%d but copula dimension=%d.",
                 d_u, d_cop))
  }

  # 2) Inclusion-exclusion calculation:
  #    P(X1 > x1, ..., Xd > xd)
  #    = sum_{k=0 to d} [ (-1)^k * sum_of_C(u_vectors_for_subsets_of_size_k) ]
  #    where each subset-of-size-k vector has F_j(x_j) in positions j of the subset,
  #    and 1 in positions not in the subset.
  #    This formula generalizes the well-known 2D case:
  #        P(X1 > x1, X2 > x2) = 1 - u1 - u2 + C(u1, u2).

  total <- 0
  for (k in 0:d_cop) {
    sign <- (-1)^k
    if (k == 0) {
      # The empty subset => vector of all 1s => pCopula(1,1,1,...) = 1 by definition of a copula
      total <- total + sign * copula::pCopula(rep(1, d_cop), cop)
    } else {
      # combos = all subsets of size k from {1, 2, ..., d_cop}
      combos <- combn(d_cop, k)
      sum_k  <- 0
      for (col in 1:ncol(combos)) {
        # For each subset S of size k, create a vector vs with vs[S] = u[S] and vs[-S]=1
        idx_subset <- combos[, col]
        vs <- rep(1, d_cop)
        vs[idx_subset] <- u[idx_subset]
        # Evaluate the copula at vs => C(vs) = P(X1 <= vs[1], ..., Xd <= vs[d])
        sum_k <- sum_k + copula::pCopula(vs, cop)
      }
      # Multiply by (-1)^k and add it to total
      total <- total + sign * sum_k
    }
  }
  return(total)
}







#' Compute Conditional Survival Probability from Fitted Copulas
#'
#' This function computes the conditional survival probability
#' \deqn{P(X_{i_0} > x_{i_0} \mid X_{-i_0} > x_{-i_0}) = \frac{P(X_1 > x_1, \dots, X_d > x_d)}{P(X_{-i_0} > x_{-i_0})},}
#' where X_{-i_0} represents the variables in the conditioning set.
#'
#' @param analysis_results The output list from \code{analyze_copulas()}.
#' @param copula_name A string specifying which copula to use from \code{analysis_results$Copula_Results}
#'   (e.g., "Gumbel Copula").
#' @param thresholds A named numeric vector of thresholds for each variable (e.g.,
#'   \code{c(parkerdam=10000, greenriver=9000, cameo=8500, gunnison=8000)}).
#' @param params_list A list of Pearson parameter sets corresponding to each variable.
#' @param cond_indices An integer vector specifying the conditioning variables (e.g., \code{2:4}).
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
  # 1. Extract the fitted copula object from analysis_results by matching copula_name.
  cr_list <- analysis_results$Copula_Results
  match_idx <- which(sapply(cr_list, function(x) x$Copula) == copula_name)
  if (length(match_idx) == 0) {
    stop("Could not find copula named '", copula_name, "' in analysis_results.")
  }
  this_fit <- cr_list[[match_idx]]$Fit  # the fitCopula() result
  full_copula <- this_fit@copula        # the fitted copula object

  # 2. Convert thresholds to u-scale: u_j = F_j(x_j).
  #    We assume the names in thresholds match those in params_list.
  map_numeric_to_roman <- function(num_type) {
    # Extend/modify this map if needed:
    # 0 -> "0", 1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII"
    # If 'num_type' is already a string, just return it as is:
    if (is.character(num_type)) return(num_type)
    # If numeric:
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
    # Use a switch on real_type to select the appropriate Pearson CDF
    switch(
      real_type,
      "0" = ppearson0(x,
                      mean = pearson_info$mean,
                      sd = pearson_info$sd,
                      lower.tail = TRUE, log.p = FALSE),
      "I" = ppearsonI(x,
                      a = pearson_info$a,
                      b = pearson_info$b,
                      location = pearson_info$location,
                      scale = pearson_info$scale,
                      lower.tail = TRUE, log.p = FALSE),
      "II" = ppearsonII(x,
                        a = pearson_info$a,
                        location = pearson_info$location,
                        scale = pearson_info$scale,
                        lower.tail = TRUE, log.p = FALSE),
      "III" = ppearsonIII(x,
                          shape = pearson_info$shape,
                          location = pearson_info$location,
                          scale = pearson_info$scale,
                          lower.tail = TRUE, log.p = FALSE),
      "IV" = ppearsonIV(x,
                        m = pearson_info$m,
                        nu = pearson_info$nu,
                        location = pearson_info$location,
                        scale = pearson_info$scale,
                        lower.tail = TRUE, log.p = FALSE),
      "V" = ppearsonV(x,
                      shape = pearson_info$shape,
                      location = pearson_info$location,
                      scale = pearson_info$scale,
                      lower.tail = TRUE, log.p = FALSE),
      "VI" = ppearsonVI(x,
                        a = pearson_info$a,
                        b = pearson_info$b,
                        location = pearson_info$location,
                        scale = pearson_info$scale,
                        lower.tail = TRUE, log.p = FALSE),
      "VII" = ppearsonVII(x,
                          df = pearson_info$df,
                          location = pearson_info$location,
                          scale = pearson_info$scale,
                          lower.tail = TRUE, log.p = FALSE),
      stop("Unsupported Pearson type in compute_conditional_survival.")
    )
  }

  u_names <- names(thresholds)
  u_vals <- numeric(length(u_names))
  for (j in seq_along(u_names)) {
    colname <- u_names[j]
    xj <- thresholds[colname]
    pearson_info <- params_list[[colname]]
    u_vals[j] <- pick_pearson_cdf(xj, pearson_info)
  }

  # 3. Compute numerator: joint survival probability for all variables.
  num <- pSurvivalCopula(u_vals, full_copula)

  # 4. Build sub-copula for conditioning set.
  sub_dim <- length(cond_indices)
  fitted_params <- cr_list[[match_idx]]$Parameters  # Use the fitted parameter(s) from the analysis results
  cop_class <- class(full_copula)
  if ("gumbelCopula" %in% cop_class) {
    sub_copula <- gumbelCopula(param = fitted_params, dim = sub_dim)
  } else if ("claytonCopula" %in% cop_class) {
    sub_copula <- claytonCopula(param = fitted_params, dim = sub_dim)
  } else if ("frankCopula" %in% cop_class) {
    sub_copula <- frankCopula(param = fitted_params, dim = sub_dim)
  } else if ("normalCopula" %in% cop_class) {
    sub_copula <- normalCopula(param = fitted_params, dim = sub_dim)
  } else if ("tCopula" %in% cop_class) {
    df <- full_copula@df
    sub_copula <- tCopula(param = fitted_params, dim = sub_dim, df = df)
  } else {
    stop("Unsupported copula family in compute_conditional_survival.")
  }


  # 5. Compute denominator: survival probability for conditioning set.
  u_cond <- u_vals[cond_indices]
  denom <- pSurvivalCopula(u_cond, sub_copula)

  # 6. Return conditional survival probability.
  cond_prob <- num / denom
  return(cond_prob)
}










