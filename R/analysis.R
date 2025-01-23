#' Perform Uniformity Test and Copula Fitting
#'
#' This function takes uniform data transformed using Pearson distributions and fits various copulas.
#'
#' @param data Data frame of flows.
#' @param params_list Parameter list from fit_pearson_params.
#'
#' @return A list containing fitted copulas and a summary table.
#' @export
#'
#' @importFrom copula fitCopula tCopula gumbelCopula frankCopula claytonCopula normalCopula
#' @importFrom ADGofTest ad.test
#' @importFrom stats AIC BIC logLik punif
#' @importFrom PearsonDS ppearsonI ppearsonIII ppearsonIV
analyze_copulas <- function(data, params_list) {
  # Note: No library calls here anymore

  # Convert to uniform marginals using PearsonDS functions
  u1 <- ppearsonIV(data$parkerdam,
                   m = params_list$parker$m, nu = params_list$parker$nu,
                   location = params_list$parker$location, scale = params_list$parker$scale,
                   lower.tail = TRUE, log.p = FALSE)

  u2 <- ppearsonI(data$greenriver,
                  a = params_list$green$a, b = params_list$green$b,
                  location = params_list$green$location, scale = params_list$green$scale,
                  lower.tail = TRUE, log.p = FALSE)

  u3 <- ppearsonI(data$cameo,
                  a = params_list$cameo$a, b = params_list$cameo$b,
                  location = params_list$cameo$location, scale = params_list$cameo$scale,
                  lower.tail = TRUE, log.p = FALSE)

  u4 <- ppearsonIII(data$gunnison,
                    shape = params_list$gunnison$shape,
                    location = params_list$gunnison$location, scale = params_list$gunnison$scale,
                    lower.tail = TRUE, log.p = FALSE)

  data_uniform <- cbind(u1, u2, u3, u4)

  # Perform AD test for uniformity using ADGofTest::ad.test
  ad_results <- list(
    parker = ADGofTest::ad.test(u1, punif),
    green = ADGofTest::ad.test(u2, punif),
    cameo = ADGofTest::ad.test(u3, punif),
    gunnison = ADGofTest::ad.test(u4, punif)
  )

  # Internal function to fit and evaluate copulas
  fit_and_evaluate2 <- function(copula_model, data, copula_name) {
    fit <- fitCopula(copula_model, data, method = "ml")
    loglik_total <- as.numeric(logLik(fit))
    aic <- AIC(fit)
    bic <- BIC(fit)
    k <- attr(logLik(fit), "df")

    list(Copula = copula_name, LogLikelihood = loglik_total,
         AIC = aic, BIC = bic, Parameters = fit@estimate, Fit = fit)
  }

  # Define various copulas from copula package
  copula_list <- list(
    t = tCopula(dim = 4),
    gumbel = gumbelCopula(dim = 4),
    frank = frankCopula(dim = 4),
    clayton = claytonCopula(dim = 4),
    gaussian = normalCopula(dim = 4)
  )

  # Fit all copulas and collect results
  all_results <- lapply(names(copula_list), function(name) {
    fit_and_evaluate2(copula_list[[name]], data_uniform,
                      paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), " Copula"))
  })

  summary_table <- data.frame(
    Copula = sapply(all_results, function(x) x$Copula),
    LogLikelihood = sapply(all_results, function(x) x$LogLikelihood),
    AIC = sapply(all_results, function(x) x$AIC),
    BIC = sapply(all_results, function(x) x$BIC)
  )

  list(AD_Results = ad_results, Copula_Results = all_results, Summary = summary_table)
}
