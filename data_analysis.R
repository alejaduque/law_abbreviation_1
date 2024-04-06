# Get a list of all CSV files starting with "output_"
csv_files <- list.files(pattern = "^output_.*\\.csv$")

# Initialize a vector to store all p-values
all_p_values <- c()
mwl <- c()
rb <- c()
mb <- c()
do <- c()
os <- c()

# Iterate over each CSV file
for (file in csv_files) {
  # Read data from CSV file
  data <- read.csv(file)
  
  lengths <- data$Length
  frequencies <- data$Frequency
  
  # Run Kendall's tau correlation test
  result <- cor.test(lengths, frequencies, method = "kendall")
  
  # Store the p-value for the correlation test
  all_p_values <- c(all_p_values, result$p.value)
  
  # Calculate weighted sum of lengths
  total_frequency <- sum(data$Frequency)
  mean_length <- sum(data$Frequency/total_frequency * data$Length)
  mwl <- c(mwl, mean_length)
  
  # Calculate random baseline
  length_sum <- sum(data$Length)
  random_b <- length_sum/nrow(data)
  rb <- c(rb, random_b)
  
  # Minimum baseline
  sorted_freqs <- sort(as.array(data$Frequency), decreasing = TRUE)
  sorted_lengths <- sort(as.array(data$Length), decreasing = FALSE)
  min_b <- sum(sorted_freqs/total_frequency * sorted_lengths)
  mb <- c(mb, min_b)
  
  # Degree of optimality and optimality score
  do <- c(do, min_b/mean_length)
  os <- c(os, (random_b-mean_length)/(random_b-min_b))
}

# Apply Holm-Bonferroni correction to all p-values
holm_corrected_p_values <- p.adjust(all_p_values, method = "holm")

for (i in seq_along(csv_files)) {
  cat("Correlation test p-value for file:", csv_files[i], "\n")
  cat("Original p-value:", all_p_values[i], "\n")
  cat("Holm-Bonferroni corrected p-value:", holm_corrected_p_values[i], "\n")
  cat("Mean word length:", mwl[i], "\n")
  cat("Random baseline:", rb[i], "\n")
  cat("Minimum baseline:", mb[i], "\n")
  cat("Degree of optimality:", do[i], "\n")
  cat("Optimality score:", os[i], "\n")
  cat("\n")
}
