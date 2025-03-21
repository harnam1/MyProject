library(ggplot2)  # Load ggplot2 explicitly if not already loaded
# Step 1: Setting the working directory and loading the functions into the environment
setwd("/Users/harnamsinghthethi/Desktop/Uni stuff/Third Year/Project/MyProject")   # Set working directory to your package
devtools::load_all()

# Step 2: Load the data using your package function
# Instead of read.csv, we now rely on the included dataset and the load function
df <- load_peakflow_data()

# Check the data
head(df)

# Step 3: Plot the time series using the package function
# df has a date column and columns c("parkerdam","greenriver","cameo","gunnison")
flow_cols <- c("parkerdam", "greenriver", "cameo", "gunnison")

# Time Series
plot_time_series(df, date, flow_cols)


# Step 4: Plot histograms for each river using your package function
# This replaces the manual histogram calls with the `plot_river_histograms()` function
plot_river_histograms(df, flow_cols)


# Step 5: Fit Pearson parameters to each river
params_list <- fit_pearson_params(df, flow_cols)

# Step 6: Plot histograms with density lines for each river using the parameters
plot_histograms_with_density(df, flow_cols, params_list)

# Step 7: Analyze copulas
# This function internally transforms data to uniform scale, performs AD tests,
results <- analyze_copulas(df,flow_cols, params_list)

# Comment this out usually!!
#write.csv(results$Summary, file = "copula_summary.csv", row.names = FALSE)
# Print out copula summary results
print(results$Summary)

# The AD results (uniformity tests) are also included:
print(results$AD_Results)

# Step 8: Compute Conditional Survival Probability using compute_conditional_survival()
# Define thresholds for each river using 99th percentiles:
thresholds <- c(
  parkerdam  = as.numeric(quantile(df$parkerdam, 0.99, na.rm = TRUE)),
  greenriver = as.numeric(quantile(df$greenriver, 0.99, na.rm = TRUE)),
  cameo      = as.numeric(quantile(df$cameo, 0.99, na.rm = TRUE)),
  gunnison   = as.numeric(quantile(df$gunnison, 0.99, na.rm = TRUE))
)

# For instance, we want to compute the conditional probability:
# In this case, Parker Dam is the first variable and we condition on the remaining ones.
cond_indices <- 2:4

#Compute conditional survival probability using "Gumbel Copula"
cond_prob_gumbel <- compute_conditional_survival(
 analysis_results = results,
 copula_name      = "Gumbel Copula",
 thresholds       = thresholds,
 params_list      = params_list,
 cond_indices     = cond_indices
)
cat("Conditional Survival Probability (Gumbel Copula):", cond_prob_gumbel, "\n")

# Compute conditional survival probability using Clayton Copula
cond_prob_clayton <- compute_conditional_survival(
  analysis_results = results,
  copula_name      = "Clayton Copula",
  thresholds       = thresholds,
  params_list      = params_list,
  cond_indices     = cond_indices
)
cat("Conditional Survival Probability (Clayton Copula):", cond_prob_clayton, "\n")

# Compute conditional survival probability using Frank Copula
cond_prob_frank <- compute_conditional_survival(
  analysis_results = results,
  copula_name      = "Frank Copula",
  thresholds       = thresholds,
  params_list      = params_list,
  cond_indices     = cond_indices
)
cat("Conditional Survival Probability (Frank Copula):", cond_prob_frank, "\n")









# Load package functions
devtools::load_all()


flow_cols = c("parkerdam", "greenriver", "cameo", "gunnison")
data = load_peakflow_data()
params_list <- fit_pearson_params(data, flow_cols)

# Fit copulas using uniform data and Pearson parameters
copula_results <- analyze_copulas(
  data = data,
  flow_cols = flow_cols,
  params_list <- params_list

)

# Define 0.99 thresholds for a 1-in-100-year event for each river
thresholds_99 <- c(
  parkerdam = 0.99,
  greenriver = 0.99,
  cameo = 0.99,
  gunnison = 0.99
)

# Specify the conditioning set indices (e.g., last three rivers)
cond_indices <- 2:4

# Compute conditional survival probability for each copula individually
cond_prob_T <- compute_conditional_survival(
  analysis_results = copula_results,
  copula_name = "T Copula",
  thresholds = thresholds_99,
  params_list = params_list,
  cond_indices = cond_indices
)

cond_prob_Gumbel <- compute_conditional_survival(
  analysis_results = copula_results,
  copula_name = "Gumbel Copula",
  thresholds = thresholds_99,
  params_list = params_list,
  cond_indices = cond_indices
)

cond_prob_Frank <- compute_conditional_survival(
  analysis_results = copula_results,
  copula_name = "Frank Copula",
  thresholds = thresholds_99,
  params_list = params_list,
  cond_indices = cond_indices
)

cond_prob_Clayton <- compute_conditional_survival(
  analysis_results = copula_results,
  copula_name = "Clayton Copula",
  thresholds = thresholds_99,
  params_list = params_list,
  cond_indices = cond_indices
)

cond_prob_Gaussian <- compute_conditional_survival(
  analysis_results = copula_results,
  copula_name = "Gaussian Copula",
  thresholds = thresholds_99,
  params_list = params_list,
  cond_indices = cond_indices
)

# Print the conditional survival probabilities for each copula
print(c("T Copula" = cond_prob_T,
        "Gumbel Copula" = cond_prob_Gumbel,
        "Frank Copula" = cond_prob_Frank,
        "Clayton Copula" = cond_prob_Clayton,
        "Gaussian Copula" = cond_prob_Gaussian))
