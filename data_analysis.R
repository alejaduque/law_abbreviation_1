# Get a list of all CSV files starting with "output_"
csv_files <- list.files(pattern = "^output_.*\\.csv$")

# Initialize a vector to store all p-values
all_p_values <- c()
mwl <- c()
rb <- c()

# Iterate over each CSV file
for (file in csv_files) {
  # Read data from CSV file
  data <- read.csv(file)
  
  # Assuming your CSV file has columns named "Length" and "Frequency"
  lengths <- data$Length
  frequencies <- data$Frequency
  
  # Run Kendall's tau correlation test
  result <- cor.test(lengths, frequencies, method = "kendall")
  
  # Store the p-value for the correlation test
  all_p_values <- c(all_p_values, result$p.value)
  
  # Calculate weighted sum of lengths
  weighted_sum <- sum(data$Frequency * data$Length)
  total_frequency <- sum(data$Frequency)
  mean_length <- weighted_sum / total_frequency
  mwl <- c(mwl, mean_length)
  
  length_sum <- sum(data$Length)
  rb <- c(rb, length_sum/nrow(data))
}

# Apply Holm-Bonferroni correction to all p-values
holm_corrected_p_values <- p.adjust(all_p_values, method = "holm")

for (i in seq_along(csv_files)) {
  cat("Correlation test p-value for file:", csv_files[i], "\n")
  cat("Original p-value:", all_p_values[i], "\n")
  cat("Holm-Bonferroni corrected p-value:", holm_corrected_p_values[i], "\n")
  cat("Mean word length:", mwl[i], "\n")
  cat("Random baseline:", rb[i], "\n")
  cat("\n")
}
