# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("Acute_Chronic_Mutations.csv", skip = 2)  # Skip the first two header rows

# Rename columns for clarity
colnames(data) <- c('RT_Mutation', 'RT_Acute', 'SHB_Mutation', 'SHB_Acute', 
                    'Pre_Core_Mutation', 'Pre_Core_Acute', 'Core_Mutation', 
                    'Core_Acute', 'RT_Mutation_Chronic', 'RT_Chronic', 
                    'RT_p_value', 'SHB_Mutation_Chronic', 'SHB_Chronic', 
                    'SHB_p_value', 'Pre_Core_Mutation_Chronic', 
                    'Pre_Core_Chronic', 'Pre_Core_p_value', 'Core_Mutation_Chronic', 
                    'Core_Chronic', 'Core_p_value')

# Select relevant columns for Acute and Chronic cases
acute_data <- data %>%
  select(RT_Mutation, RT_Acute, SHB_Mutation, SHB_Acute, Pre_Core_Mutation, Pre_Core_Acute, Core_Mutation, Core_Acute) %>%
  gather(key = "Region", value = "Percentage", -RT_Mutation, -SHB_Mutation, -Pre_Core_Mutation, -Core_Mutation)

chronic_data <- data %>%
  select(RT_Mutation_Chronic, RT_Chronic, SHB_Mutation_Chronic, SHB_Chronic, Pre_Core_Mutation_Chronic, Pre_Core_Chronic, Core_Mutation_Chronic, Core_Chronic) %>%
  gather(key = "Region", value = "Percentage", -RT_Mutation_Chronic, -SHB_Mutation_Chronic, -Pre_Core_Mutation_Chronic, -Core_Mutation_Chronic)

# Add a new column for case type
acute_data$Case_Type <- 'AHB'
chronic_data$Case_Type <- 'CHB-AE'

# Combine Acute and Chronic data
combined_data <- bind_rows(
  acute_data %>% mutate(Mutation_Label = with(., ifelse(Region == "RT_Acute", RT_Mutation,
                                                        ifelse(Region == "SHB_Acute", SHB_Mutation,
                                                               ifelse(Region == "Pre_Core_Acute", Pre_Core_Mutation,
                                                                      Core_Mutation))))),
  
  chronic_data %>% mutate(Mutation_Label = with(., ifelse(Region == "RT_Chronic", RT_Mutation_Chronic,
                                                          ifelse(Region == "SHB_Chronic", SHB_Mutation_Chronic,
                                                                 ifelse(Region == "Pre_Core_Chronic", Pre_Core_Mutation_Chronic,
                                                                        Core_Mutation_Chronic)))))
)

# Assign region names (RT, SHB, Pre_Core, Core) to both Acute and Chronic cases
combined_data$Region_Type <- ifelse(grepl("RT", combined_data$Region), "RT",
                                    ifelse(grepl("SHB", combined_data$Region), "SHB",
                                           ifelse(grepl("Pre_Core", combined_data$Region), "Pre_Core", "Core")))

# Remove duplicate mutation labels, but retain all dots
combined_data <- combined_data %>%
  group_by(Mutation_Label) %>%
  mutate(Label_Display = ifelse(Percentage == max(Percentage), Mutation_Label, NA)) %>%
  ungroup()

# Plot using shapes for Acute/Chronic and colors for Regions
ggplot(combined_data, aes(x = Mutation_Label, 
                          y = Percentage, shape = Case_Type, color = Region_Type)) +
  geom_point(size = 3) +
  theme(axis.text.x = element_blank()) +  # Remove x-axis labels
  labs(title = "Mutation frequencies among clinically classified acute hepatitis B (AHB) and acute exacerbation of chronic hepatitis B (CHB-AE)", x = "Mutations", y = "Percentage Occurrence") +
  scale_shape_manual(values = c(16, 17)) +  # Two shapes for Acute and Chronic
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")) +  # New vibrant colors
  geom_text(aes(label = ifelse(Percentage > 25, Label_Display, ''),
                hjust = ifelse(Mutation_Label == "A128V", -0.3, 1.3),  # Adjust hjust for better positioning
                nudge_x = ifelse(Mutation_Label == "A128V", 0.3, -0.3)),  # Move labels closer to the points
            vjust = 0.5, size = 3)  # Label mutations with >25%, only highest percentage
