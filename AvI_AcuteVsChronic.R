
final_merged_data <- read.csv("/mnt/chromeos/GoogleDrive/SharedWithMe/68805/Data_Analytics_R/HBV_R/Final_Merged_Mutation_Data.csv")

# Required Libraries
library(ggplot2)

# Boxplot comparing IgM values between Acute and Chronic cases
ggplot(final_merged_data, aes(x=Region, y=IgM_Greater85, fill=Region)) +
  geom_boxplot() +
  facet_wrap(~Region) +  # Optional: separate plots for each region
  ggtitle("Boxplot of IgM Values for Acute vs Chronic Cases") +
  xlab("Clinical Condition") + ylab("IgM Values") +
  theme_minimal()

# Boxplot comparing Avidity Index between Acute and Chronic cases
ggplot(final_merged_data, aes(x=Region, y=AvI_Lesser046, fill=Region)) +
  geom_boxplot() +
  facet_wrap(~Region) +  # Optional: separate plots for each region
  ggtitle("Boxplot of Avidity Index for Acute vs Chronic Cases") +
  xlab("Clinical Condition") + ylab("Avidity Index Values") +
  theme_minimal()

