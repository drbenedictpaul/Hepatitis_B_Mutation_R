# Load necessary libraries
library(fmsb) # For radar chart
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

# Prepare the dataset for radar plot
# Radar plots need min-max scaling, so we'll add rows for max and min values
df_radar <- df[, -1] # Exclude 'Region' column
df_radar <- rbind(rep(max(df_radar, na.rm = TRUE), ncol(df_radar)), # Max value for scaling
                  rep(0, ncol(df_radar)), # Min value for scaling
                  df_radar)

# Set row names to Regions
rownames(df_radar) <- c("Max", "Min", df$Region)

# Create radar chart
radarchart(df_radar, axistype = 1, 
           # Customizing colors and transparency
           pcol = c("blue", "red", "green", "purple"), 
           pfcol = c(rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5), rgb(0.7,0.2,0.5,0.5), rgb(0.9,0.1,0.3,0.5)),
           plwd = 2, 
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(df_radar, na.rm = TRUE), 20), 
           cglwd = 0.8, 
           vlcex = 0.8)

# Add a title
title(main = "Radar Plot of Mutation Freq Acute vs Chronic Cases", cex.main = 1.2)
