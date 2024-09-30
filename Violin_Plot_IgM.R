final_merged_data <- read.csv("/mnt/chromeos/GoogleDrive/SharedWithMe/68805/Data_Analytics_R/HBV_R/Final_Merged_Mutation_Data.csv")

# Required Libraries
library(ggplot2)


# Reshape the data to focus on Acute and Chronic comparisons
ggplot(final_merged_data, aes(x=as.factor(Acute), y=IgM_Greater85, fill=as.factor(Acute))) +
  geom_violin() +
  ggtitle("Violin Plot of IgM Values for Acute vs Chronic Cases") +
  xlab("Clinical Condition") + ylab("IgM Values") +
  scale_x_discrete(labels = c("0" = "Chronic", "1" = "Acute")) +  # Labeling the x-axis
  theme_minimal()
