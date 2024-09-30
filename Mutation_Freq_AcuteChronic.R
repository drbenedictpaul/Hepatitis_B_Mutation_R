# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Load the Acute/Chronic dataset
acute_chronic_data <- read.csv("HBV_R - Mutation_Frequency_Acute_Chronic.csv", skip = 2, fill = TRUE)

# Clean up the acute/chronic dataset
colnames(acute_chronic_data) <- c("Region", "Total", "AHB_Total", "AHB_Percent", "CHB_Total", "CHB_Percent", 
                                  "P_value_Total", "AHB_Excl", "AHB_Excl_Percent", "CHB_Excl", 
                                  "CHB_Excl_Percent", "P_value_Excl", "Both", "Both_Percent")

# Filter relevant columns from acute/chronic dataset including Exclusive mutations
acute_chronic_df <- acute_chronic_data[, c("Region", "AHB_Total", "CHB_Total", "AHB_Excl", "CHB_Excl", "Both")]

# Convert mutation frequency columns to numeric
acute_chronic_df$AHB_Total <- as.numeric(as.character(acute_chronic_df$AHB_Total))
acute_chronic_df$CHB_Total <- as.numeric(as.character(acute_chronic_df$CHB_Total))
acute_chronic_df$AHB_Excl <- as.numeric(as.character(acute_chronic_df$AHB_Excl))
acute_chronic_df$CHB_Excl <- as.numeric(as.character(acute_chronic_df$CHB_Excl))
acute_chronic_df$Both <- as.numeric(as.character(acute_chronic_df$Both))

# Reshape acute/chronic dataset for plotting
acute_chronic_long <- melt(acute_chronic_df, id.vars = "Region", variable.name = "Mutation_Type", value.name = "Frequency")

# Generate the Acute/Chronic heatmap with scale set to 100
ggplot(acute_chronic_long, aes(x = Region, y = Mutation_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  
  # Set the color scale limits to 100
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 100)) +
  
  labs(title = "Mutation Frequencies of Acute_Chronic cases",
       x = "Virus Region", y = "Mutation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
