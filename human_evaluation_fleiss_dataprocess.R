# Install and load the 'irr' package
install.packages("irr")
install.packages("readr")
library(irr)

library(irr)
library(readr)
#raters_results <- read_csv("I:/DataMgmt/00_Archive_old/dataMgmt_ddrive_[will_be_deleted_2024_09_30]/dataMgmt/tmp/Vivian/raters_results.xls")

raters_results <- read.csv("~/Downloads/raters_results.csv")
#raters_results[is.na(raters_results)] <- 0

View(raters_results)



#kappa_result <- kappam.fleiss(raters_results[2:5], detail = TRUE)
#kappa_result <- kappam.fleiss(raters_results[2], detail = TRUE)
#kappam.fleiss(raters_results, detail = TRUE)




# Assuming 'df' is your data frame with raters' ratings
# The data frame should have rows representing subjects and columns representing raters

# Extract the relevant columns for each rater
rater1 <- raters_results[, "Ji"]
rater2 <- raters_results[, "Yiming"]
rater3 <- raters_results[, "Ran"]
rater4 <- raters_results[, "Wang"]
# ... Repeat for each rater

rater1 <- as.data.frame(rater1)
rater2 <- as.data.frame(rater2)
rater3 <- as.data.frame(rater3)
rater4 <- as.data.frame(rater4)
# Remove rows with missing values for each rater
rater1 <- rater1[complete.cases(rater1), ]
rater2 <- rater2[complete.cases(rater2), ]
rater3 <- rater3[complete.cases(rater3), ]
rater4 <- rater4[complete.cases(rater4), ]
# ... Repeat for each rater

# Create an array with the raters' ratings


# Calculate the mean for each row
mean_ratings <- rowMeans(ratings)

# Calculate the median for each row
median_ratings <- apply(ratings, 1, median)

# Combine mean and median ratings into a data frame
ratings_summary <- data.frame(mean_ratings = mean_ratings, median_ratings = median_ratings)

# Print the summary table
print(ratings_summary)



# Calculate mean for each rater
mean_ratings <- colMeans(ratings)

# Calculate median for each rater
median_ratings <- sapply(ratings, median)

# Combine mean and median ratings into a data frame
ratings_summary <- data.frame(
  Rater = colnames(ratings),
  Mean = mean_ratings,
  Median = median_ratings
)

# Print the summary table
print(ratings_summary)




# Calculate Fleiss' kappa
fleiss_kappa <- kappam.fleiss(ratings,detail =TRUE)

# Print the results
print(fleiss_kappa)




# Calculate observed agreement for each rater
obs_agreement_rater1 <- mean(rater1 == rowMeans(cbind(rater2, rater3, rater4)))
obs_agreement_rater2 <- mean(rater2 == rowMeans(cbind(rater1, rater3, rater4)))
obs_agreement_rater3 <- mean(rater3 == rowMeans(cbind(rater1, rater2, rater4)))
obs_agreement_rater4 <- mean(rater4 == rowMeans(cbind(rater1, rater2, rater3)))

# Print the results
print(obs_agreement_rater1)
print(obs_agreement_rater2)
print(obs_agreement_rater3)
print(obs_agreement_rater4)

##Testing some other kappa values among the sample
# The data frame should have rows representing subjects and columns representing raters

# Specify the number of rows to process at a time
chunk_size <- 10

# Create an array with the raters' ratings
#ratings <- cbind(df[, "Rater1"], df[, "Rater2"], df[, "Rater3"], df[, "Rater4"])

# Calculate Fleiss' kappa for every chunk of 10 rows
num_rows <- nrow(ratings)
num_chunks <- ceiling(num_rows / chunk_size)

for (i in 1:num_chunks) {
  start_row <- (i - 1) * chunk_size + 1
  end_row <- min(i * chunk_size, num_rows)
  
  # Extract the chunk of rows
  chunk_ratings <- ratings[start_row:end_row, ]
  
  # Calculate Fleiss' kappa for the chunk
  fleiss_kappa <- kappam.fleiss(chunk_ratings, detail = TRUE)
  
  # Print the results for the current chunk
  cat(sprintf("Fleiss' kappa for rows %d to %d:\n", start_row, end_row))
  print(fleiss_kappa)
  cat("\n")
}


##Print final result for further calculations in Python
View(raters_results)
write.csv(ratings, "raters.csv", row.names=TRUE)
