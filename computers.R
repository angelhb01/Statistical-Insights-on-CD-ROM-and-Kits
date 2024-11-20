cd <- subset(Computers, cd == "yes")  # A subset of all computers that have a CD-ROM
non_cd <- subset(Computers, cd == "no")  # A subset of all computers that do not have a CD-ROM

# Sample size for computers with CD-ROM (n1), and sample size for computers with non CD-ROM (n2)
n1 <- 2908
n2 <- 3351

success1 <- sum(cd$multi == "yes") # CD-ROM computers that have a multi media kit
fails1 <- sum(cd$multi == "no") # CD-ROM computers that do not have a multi media kit

# Counts for successes and failures for CD-ROM computers & creates a bar plot for the visualization.
counts1 <- c(success1, fails1)
barplot(counts1, beside = TRUE, col = c("green", "red"), main = "Success & Fails for CD-ROM", xlab = "Group and Outcome", ylab = "Count")

# Counts for successes and failures for non CD-ROM computers & creates a bar plot for the visualization.
counts2 <- c(success2, fails2)
barplot(counts2, beside = TRUE, col = c("green", "red"), main = "Success & Fails for Non CD-ROM", xlab = "Group and Outcome", ylab = "Count")

# Creates a small matrix for the successses and fails for both groups.
data_matrix <- matrix(c(success1, fails1, success2, fails2), nrow = 2, byrow = TRUE)
rownames(data_matrix) <- c("Success", "Failure")  # The row names for the matrix returns the count of successes and failure
colnames(data_matrix) <- c("CD-ROM", "Non-CD-ROM") # The column names for the matrix returns the count of successes and failure

# Creates a mosaic plot to show a visualization of both groups of their successes and failures
mosaicplot(data_matrix, main = "Mosaic Plot of Successes and Failures by CD-ROM Group", color = c("green", "red"), xlab = "CD-ROM Group", ylab = "Outcome", las = 1)

# The sampled proportions of both groups that have a multi media kit
pHat1 <- success1 / n1
pHat2 <- success2 / n2

ptEst <- pHat1 - pHat2  # Our estimator is the difference between the two groups

z <- 1.96  # Our citical value which indicates how many standard errors the data is away from the true population proportion

SE <- sqrt(((pHat1*(1-pHat1))/(n1))+((pHat2*(1-pHat2))/(n2))) # The formula for the standard error

# Below calculates the confidence interval which indicates that we are 95% confident that the true population proportion is within the interval.
lower <- 0.30-1.96*0.0085
upper <- 0.30+1.96*0.0085

# Confidence Interval = (0.283, 0.317)
# Since there isn't at least 10 observations that have a multi media kit in the non CD-ROM group, the interval is not accurate.
