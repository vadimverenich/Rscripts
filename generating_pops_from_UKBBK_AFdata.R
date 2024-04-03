# Install and load necessary package
if (!require(data.table)) install.packages("data.table")
library(data.table)

# Read the data
filename <- "path_to_your_data_file.txt"  # Replace with your file path
snp_data <- fread(filename, header = T)


simulate_genotypes <- function(freq, A1, A2) {
  #adjusted_freq <- freq + runif(1, -variability_factor, variability_factor)
  #adjusted_freq <- min(max(adjusted_freq, 0), 1)  # Ensure frequency stays between 0 and 1
  
  # Calculate genotype probabilities assuming Hardy-Weinberg Equilibrium
  p_AA <- (1 - freq)^2
  p_AB <- 2 * freq * (1 - freq)
  p_BB <- freq^2
  # Simulate genotypes
  sample(c(paste0(A2, A2), paste0(A2, A1), paste0(A1, A1)), num_individuals,
         replace = TRUE, prob = c(p_AA, p_AB, p_BB))
}

num_individuals <- 50
populations <- colnames(snp_data)[6:ncol(snp_data)]  # Adjust column indices as per your data
simulated_data <- list()

for (pop in populations) {
  genotypes <- apply(snp_data, 1, function(row) {
    simulate_genotypes(as.numeric(row[pop]), row['A1'], row['A2'])
  })
  simulated_data[[pop]] <- t(genotypes)
}

write_plink_files <- function(simulated_genotypes, snp_data, population_name, num_individuals) {
  num_snps <- nrow(snp_data)

  # Check length of simulated_genotypes
  if (length(simulated_genotypes) != num_individuals * num_snps) {
    stop("Mismatch in the number of elements in simulated_genotypes for population ", population_name)
  }

  # .map file
  map_data <- snp_data[, .(chrom, rsid, '0', position)]
  fwrite(map_data, paste0(population_name, ".map"), sep = "\t", col.names = FALSE, quote = FALSE)

  # .ped file
  ped_data <- matrix(nrow = num_individuals, ncol = 6 + 2 * num_snps)
  for (i in 1:num_individuals) {
    individual_id <- paste0(population_name, "_indiv_", i)
    start_index <- (i - 1) * num_snps + 1
    end_index <- i * num_snps
    individual_genotypes <- simulated_genotypes[start_index:end_index]
    genotypes_split <- unlist(strsplit(individual_genotypes, ""))
    ped_data[i, ] <- c(individual_id, individual_id, 0, 0, 0, -9, genotypes_split)
  }

  fwrite(as.data.frame(ped_data), paste0(population_name, ".ped"), sep = " ", col.names = FALSE, quote = FALSE, row.names = FALSE)
}

# Generate files for each population
for (pop in names(simulated_data)) {
  write_plink_files(simulated_data[[pop]], snp_data, pop, num_individuals)
}

