# Load required libraries
library(pcapred)
library(bigsnpr)
library(dplyr)
library(pcapred.ref)
# The vegan package is used for the procrustes function
if (!require(vegan)) install.packages("vegan")
library(vegan)

# Read the analysis file

read.table("analysis.fam", header=FALSE) -> analysis
read.table("westeuras.fam", header=FALSE) -> westeuras
read.table("euromerged.fam", header=FALSE) -> euromerged

# Identify common samples between analysis and reference data
common_samples <- intersect(analysis[,2], euromerged[,2])
# Match indices of common samples in analysis and reference data
matched_indices_test <- match(common_samples, analysis[,2])
matched_indices_ref <- match(common_samples, euromerged[,2])
# Identify indices of samples to be removed in analysis data
removed_indices_test <- match(westeueras[,2], analysis[,2])

# Assuming pred2 contains reference PCs
pc_ref <- pred2

# Using the pcapred package for projection/predicting
dat <- readbed("analysis")
dat <- mergeref(dat, verbose = TRUE, mergeon = "CHRPOS")
pred3 <- predictpcs(dat)
pc_test <- pred3


# Matching samples have already been defined as matched_ref and matched_test
# Define matched_indices_ref and matched_indices_test beforehand
# Centering the matched samples relative to the reference
center_ref <- colMeans(pc_ref[matched_indices_ref, ])
matched_ref_centered <- sweep(pc_ref[matched_indices_ref, ], 2, center_ref, FUN = "-")
matched_test_centered <- sweep(pc_test[matched_indices_test, ], 2, center_ref, FUN = "-")

# Procrustes transformation for the matched centered samples
procrustes_res <- procrustes(matched_ref_centered, matched_test_centered, symmetric = TRUE)

# Calculating the scale ratio between reference and test datasets
scale_ref <- max(dist(matched_ref_centered))  # Maximum distance among reference samples
scale_test <- max(dist(matched_test_centered))  # Maximum distance among matched test samples
scale_ratio <- scale_ref / scale_test

# Scaling the test dataset
pc_test_centered <- sweep(pc_test[-removed_indices_test, ], 2, center_ref, FUN = "-")  # Centering all test data
pc_test_transformed <- pc_test_centered %*% procrustes_res$rotation  # Applying rotation
pc_test_scaled <- pc_test_transformed * scale_ratio  # Scaling according to scale ratio
pc_test_scaled_translated <- sweep(pc_test_scaled, 2, center_ref, FUN = "+")  # Translating back to original position

# Visualization of reference and scaled test data
plot(pc_ref[, 1], pc_ref[, 2], col = 'black', pch = 19, xlab = "PC1", ylab = "PC2", main = "Scaled Test Data After Procrustes Transformation")
points(pc_test_scaled_translated[, 1], pc_test_scaled_translated[, 2], col = 'red', pch = 19)

# Preparing and saving the transformed test data
analysis[-removed_indices_test, ] -> tested
cbind(tested[,1:2], pc_test_scaled_translated) -> tested.pred
write.table(tested.pred, "ukr.projected.txt", quote = FALSE, row.names = FALSE)
