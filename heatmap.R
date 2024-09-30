# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load your dataset
data <- read.csv("/mnt/chromeos/GoogleDrive/SharedWithMe/68805/Data_Analytics_R/HBV_R/HBV_R - Clinical.csv")


# Select relevant columns for visualization
selected_columns <- data[, c("RT_Region", "RT_Acute", "RT_Chronic", "SHB_Region", "SHB_Acute", "SHB_Chronic", 
                             "Pre_Core_Region", "Pre_Core_Region_Acute", "Pre_Core_Region_Chronic", 
                             "Core_Region", "Core_Acute", "Core_Chronic")]

# Melt the dataset to a long format
melted_data <- melt(selected_columns, id.vars = c("RT_Region", "SHB_Region", "Pre_Core_Region", "Core_Region"))

# Create a column to label mutations where the frequency is more than 75%
melted_data$label <- ifelse(melted_data$value > 75, as.character(melted_data$RT_Region), "")

# Create the heatmap with labels placed on the plot
ggplot(melted_data, aes(x = variable, y = RT_Region, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "white") +
  theme_minimal() +
  labs(title = "Mutation Frequency Heatmap", x = "Conditions", y = NULL) +  # No y-axis label
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),  # Remove y-axis ticks and labels
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Tilt x-axis labels
  geom_text(aes(label = label),  # Place labels directly on the plot
            hjust = 0.5, vjust = 0.5, color = "black", size = 3)  # Adjust label size and positioning
