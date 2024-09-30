# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the dataset
data <- read.csv("HBV_R - Mutation_Frequency_Acute_Chronic.csv", skip = 2, fill = TRUE)

# Clean up the data
colnames(data) <- c("Region", "Total", "AHB_Total", "AHB_Percent", "CHB_Total", "CHB_Percent", 
                    "P_value_Total", "AHB_Excl", "AHB_Excl_Percent", "CHB_Excl", 
                    "CHB_Excl_Percent", "P_value_Excl", "Both", "Both_Percent")

# Filter relevant columns for visualization
df <- data[, c("Region", "AHB_Total", "CHB_Total", "AHB_Excl", "CHB_Excl", "Both")]

# Convert columns to numeric
df$AHB_Total <- as.numeric(as.character(df$AHB_Total))
df$CHB_Total <- as.numeric(as.character(df$CHB_Total))
df$AHB_Excl <- as.numeric(as.character(df$AHB_Excl))
df$CHB_Excl <- as.numeric(as.character(df$CHB_Excl))
df$Both <- as.numeric(as.character(df$Both))

# Remove rows with NA values
df <- na.omit(df)

# Reshape the data to long format for heatmap plotting
df_long <- melt(df, id.vars = "Region", variable.name = "Mutation_Type", value.name = "Frequency")

# Generate the heatmap
ggplot(df_long, aes(x = Region, y = Mutation_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of Mutation Freq-Acute_Chronic cases",
       x = "Virus Region", y = "Mutation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
