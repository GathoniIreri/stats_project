# VISUALIZATION 2: Alternative view - Split by Age Groups
# ========================================

# Create age categories
my_data_clean$age_cat <- cut(my_data_clean$age, 
                             breaks = 3, 
                             labels = c("Younger", "Middle Age", "Older"))

# Plot: AI Trust × Personalization, faceted by Age
ggplot(my_data_clean, aes(x = ai_trust_1, y = post_minus_pre, 
                          color = personalize, group = personalize)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  facet_wrap(~ age_cat, ncol = 3) +
  labs(
    title = "Three-Way Interaction: Alternative View",
    subtitle = "Does the AI trust effect differ by personalization type AND age group?",
    x = "AI Trust",
    y = "Persuasion Shift (Post - Pre)",
    color = "Message Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  ) +
  scale_color_manual(values = c("generic" = "#E41A1C", "personalized" = "#377EB8"))