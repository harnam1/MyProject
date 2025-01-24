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

