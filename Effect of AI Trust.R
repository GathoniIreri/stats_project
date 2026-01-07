# Load required libraries
library(ggplot2)

# ========================================
# INTERACTION: Personalization × AI Trust
# ========================================

# Plot for AI trust (continuous variable - use line plot)
ggplot(my_data_clean, aes(x = ai_trust_1, y = post_minus_pre, 
                          color = personalize, group = personalize)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Personalization × AI Trust Interaction",
    x = "AI Trust",
    y = "Persuasion Shift (Post - Pre)",
    color = "Message Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("generic" = "#E41A1C", "personalized" = "#377EB8"))

# Statistical test for AI trust interaction
model_ai_trust <- lm(post_minus_pre ~ personalize * ai_trust_1, data = my_data_clean)
print("=== AI TRUST INTERACTION MODEL ===")
summary(model_ai_trust)