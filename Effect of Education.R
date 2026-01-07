# Load required libraries
library(ggplot2)

# Create the interaction plot
ggplot(my_data_clean, aes(x = education, y = post_minus_pre, color = personalize, group = personalize)) +
  # Add points
  geom_point(alpha = 0.4, size = 2) +
  # Add smoothed lines to show trend
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  # Labels and theme
  labs(
    title = "Interaction Between Personalization and Education on Persuasion Shift",
    x = "Education",
    y = "Persuasion Shift (Post - Pre)",
    color = "Message Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("generic" = "#E41A1C", "personalized" = "#377EB8"))

# Alternative: Test the interaction statistically
model <- lm(post_minus_pre ~ personalize * education, data = my_data_clean)
summary(model)

# The interaction term (personalize:education) tells you if the slopes differ significantly