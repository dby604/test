# Set seed to create a simulated dataset
set.seed(1)

# Number of observations for each group
nA <- 100
nB <- 100

#Here we assume it is a medical experiment, where group A is a placebo group and group B is a treatment group
# Create simulated data for group A
group_A <- data.frame(
  Group = rep("A", nA),
  male = rnorm(nA, mean = 15, sd = 2),
  female = rnorm(nA, mean = 20, sd = 3),
  naive = rlnorm(nA, mean = 2, sd = 0.6),
  experienced = rlnorm(nA, mean = 3, sd = 0.6)
)

# Generate simulated data for group B
group_B <- data.frame(
  Group = rep("B", nB),
  male = rnorm(nB, mean = 12, sd = 2),
  female = rnorm(nB, mean = 19, sd = 3),
  naive = rlnorm(nB, mean = 1.5, sd = 0.6),
  experienced = rlnorm(nB, mean = 2.9, sd = 0.6)
)

# Combine data for both groups
simulated_data <- rbind(group_A, group_B)

t_test_results <- lapply(simulated_data[, -1], function(x) {
  t_test <- t.test(x ~ Group, data = simulated_data)
  return(t_test)
})

# p-values from the t-tests
p_values <- sapply(t_test_results, function(x) x$p.value)

# Print the p-values
print(p_values)

# Create boxplots for each feature
par(mfrow = c(1, 4))
for (feature in names(simulated_data)[-1]) {
  boxplot(simulated_data[[feature]] ~ simulated_data$Group,
          xlab = "Group", ylab = feature,
          main = paste("Boxplot of", feature),
          col = c("red", "green"))
  legend("topright", legend = c("A", "B"), fill = c("red", "green"))
}

