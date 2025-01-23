# Step 1: Setting the working directory and loading the functions into the environment
setwd("/Users/harnamsinghthethi/Desktop/Uni stuff/Third Year/Project/MyProject")   # Set working directory to your package
devtools::load_all()

# Step 2: Load the data using your package function
# Instead of read.csv, we now rely on the included dataset and the load function
df <- load_peakflow_data()

# Check the data
head(df)

# Step 3: Plot the time series using the package function
# This replicates the ggplot time-series plot from the original code
plot_time_series(df)

# Step 4: Plot histograms for each river using your package function
# This replaces the manual histogram calls with the `plot_river_histograms()` function
plot_river_histograms(df)

# Step 5: Fit Pearson parameters to each river
params_list <- fit_pearson_params(df)

# Step 6: Plot histograms with density lines for each river using the parameters
# This replaces the line-by-line calls of `plot_histogram_with_density()`
plot_histograms_with_density(df, params_list)

# Step 7: Analyze copulas
# This function internally transforms data to uniform scale, performs AD tests,
# fits copulas, and returns results similar to what you did manually before.
results <- analyze_copulas(df, params_list)

# Print out copula summary results
print(results$Summary)

# The AD results (uniformity tests) are also included:
print(results$AD_Results)

# If you need conditional probabilities for gumbel, clayton, frank, etc.,
# and `analyze_copulas()` doesn't provide them directly, consider:
# - Adding a function to compute them using the uniform values and fitted copulas
# - Or extract the fitted copulas and thresholds from `results` and replicate the logic manually here.

# For now, `analyze_copulas()` should replicate the logic of:
# - Transforming data to uniform margins
# - Performing AD tests on uniform data
# - Fitting copulas and generating a summary table

# If you want the exact conditional probability calculations done previously:
# You may need to write a small function or code block that:
# - Uses s1, s2, s3, s4 (computed from the thresholds and survival functions)
# - Extracts a fitted copula from results (e.g., gumbel_copula)
# - Computes joint and marginal probabilities, then conditional probabilities
# Similar to your original code.

# Example (if needed):
# s1 <- S1(x1); s2 <- S2(x2); s3 <- S3(x3); s4 <- S4(x4)
# gumbel_copula <- results$Copula_Results[[which(...)]]$Fit@copula  # or something similar
# ... and so on.

# In summary:
# This `testing.R` script loads all functions from your package and uses them
# to replicate the original results. If something doesn't produce identical results,
# it's likely due to differences in how `analyze_copulas()` encapsulates logic.
