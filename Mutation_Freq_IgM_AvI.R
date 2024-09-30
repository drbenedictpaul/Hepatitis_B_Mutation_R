# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the dataset
data <- read.csv("HBV_Mutation_Freq_IgM_AvI.csv")

# Clean up the data
colnames(data) <- c("Region", "Total", "IgM_Greater_8.5", "IgM_Less_8.5", "P_value_IgM", 
                    "AvI_Less_0.46", "AvI_Greater_0.46", "P_value_AvI")

# Filter relevant columns for visualization
df <- data[, c("Region", "IgM_Greater_8.5", "IgM_Less_8.5", "AvI_Less_0.46", "AvI_Greater_0.46")]

# Convert mutation frequency columns to numeric
df$IgM_Greater_8.5 <- as.numeric(as.character(df$IgM_Greater_8.5))
df$IgM_Less_8.5 <- as.numeric(as.character(df$IgM_Less_8.5))
df$AvI_Less_0.46 <- as.numeric(as.character(df$AvI_Less_0.46))
df$AvI_Greater_0.46 <- as.numeric(as.character(df$AvI_Greater_0.46))

# Reshape the data to long format for heatmap plotting
df_long <- melt(df, id.vars = "Region", variable.name = "Mutation_Type", value.name = "Frequency")

# Ensure the Frequency column is numeric
df_long$Frequency <- as.numeric(df_long$Frequency)

# Generate the heatmap
ggplot(df_long, aes(x = Region, y = Mutation_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of Mutation Frequencies (IgM and AvI)",
       x = "Virus Region", y = "Mutation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
