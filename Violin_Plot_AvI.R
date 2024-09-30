# Required Libraries
library(ggplot2)

# Violin plot comparing Avidity Index values between Acute and Chronic cases
ggplot(final_merged_data, aes(x=as.factor(Acute), y=AvI_Lesser046, fill=as.factor(Acute))) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white", outlier.color="red") +  # Adds a boxplot inside for clarity
  ggtitle("Violin Plot of Avidity Index Values for Acute vs Chronic Cases") +
  xlab("Clinical Condition") + ylab("Avidity Index Values") +
  scale_x_discrete(labels = c("0" = "Chronic", "1" = "Acute")) +  # Labels for clinical conditions
  theme_minimal()
