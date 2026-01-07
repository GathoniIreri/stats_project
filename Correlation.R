# Check current structure
cat("Current personalize variable:\n")
print(table(my_data_clean$personalize))

# Convert to numeric: generic = 0, personalized = 1
my_data_clean$personalize_numeric <- ifelse(my_data_clean$personalize == "personalized", 1, 0)

cat("\nConverted to numeric:\n")
cat("0 = generic, 1 = personalized\n\n")
# Pearson correlation
cor_test <- cor.test(my_data_clean$personalize_numeric, 
                     my_data_clean$post_minus_pre, 
                     method = "pearson")
cat("RESULTS:\n")
cat("-----------------------------------\n")
cat("Correlation coefficient (r):", round(cor_test$estimate, 4), "\n")
cat("p-value:", format(cor_test$p.value, scientific = TRUE), "\n")
cat("95% Confidence Interval: [", round(cor_test$conf.int[1], 4), ",", 
    round(cor_test$conf.int[2], 4), "]\n")
cat("-----------------------------------\n\n")
# Interpretation
cat("INTERPRETATION:\n")
if(cor_test$p.value < 0.05) {
  cat("✓ Significant correlation (p < 0.05)\n")
  if(cor_test$estimate > 0) {
    cat("  → Positive correlation: Personalization associated with higher persuasion shift\n")
  } else {
    cat("  → Negative correlation: Personalization associated with lower persuasion shift\n")
  }
} else {
  cat("✗ No significant correlation (p ≥ 0.05)\n")
  cat("  → No linear relationship between personalization and persuasion shift\n")
}

cat("\nEffect size interpretation:\n")
r_value <- abs(cor_test$estimate)
if(r_value < 0.1) {
  cat("  Very weak correlation (r < 0.1)\n")
} else if(r_value < 0.3) {
  cat("  Weak correlation (0.1 ≤ r < 0.3)\n")
} else if(r_value < 0.5) {
  cat("  Moderate correlation (0.3 ≤ r < 0.5)\n")
} else {
  cat("  Strong correlation (r ≥ 0.5)\n")
}
cat("\n=== CREATING VISUALIZATION ===\n")

library(ggplot2)
# Boxplot (better for binary predictor)
ggplot(my_data_clean, aes(x = personalize, y = post_minus_pre, fill = personalize)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "red", color = "darkred") +
  labs(
    title = "Persuasion Shift by Message Type",
    subtitle = paste0("Pearson r = ", round(cor_test$estimate, 3), 
                     ", p = ", format.pval(cor_test$p.value, digits = 3)),
    x = "Message Type",
    y = "Persuasion Shift (Post - Pre)",
    caption = "Red diamond = mean"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("generic" = "#E41A1C", "personalized" = "#377EB8"))


cat("\n=== DESCRIPTIVE STATISTICS ===\n\n")

# By group - using complete cases
generic_data <- my_data_clean[!is.na(my_data_clean$personalize) & 
                                !is.na(my_data_clean$post_minus_pre) &
                                my_data_clean$personalize == "generic", ]
personalized_data <- my_data_clean[!is.na(my_data_clean$personalize) & 
                                     !is.na(my_data_clean$post_minus_pre) &
                                     my_data_clean$personalize == "personalized", ]

cat("GENERIC messages:\n")
cat("  N:", nrow(generic_data), "\n")
cat("  Mean:", round(mean(generic_data$post_minus_pre, na.rm = TRUE), 2), "\n")
cat("  SD:", round(sd(generic_data$post_minus_pre, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(generic_data$post_minus_pre, na.rm = TRUE), 2), "\n\n")

cat("PERSONALIZED messages:\n")
cat("  N:", nrow(personalized_data), "\n")
cat("  Mean:", round(mean(personalized_data$post_minus_pre, na.rm = TRUE), 2), "\n")
cat("  SD:", round(sd(personalized_data$post_minus_pre, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(personalized_data$post_minus_pre, na.rm = TRUE), 2), "\n\n")

cat("DIFFERENCE:\n")
mean_diff <- mean(personalized_data$post_minus_pre, na.rm = TRUE) - 
  mean(generic_data$post_minus_pre, na.rm = TRUE)
cat("  Mean difference:", round(mean_diff, 2), "points\n")
cat("  Personalized", ifelse(mean_diff > 0, "HIGHER", "LOWER"), "than generic\n")