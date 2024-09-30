# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)  # For repelling labels to prevent overlap

# Load the dataset
mutations_data <- read.csv("Mutations_Unique_Shared.csv")

# Rename columns for easier manipulation
colnames(mutations_data) <- c('RT_Mutation', 'RT_Acute', 'RT_Chronic', 'RT_p_value',
                              'SHB_Mutation', 'SHB_Acute', 'SHB_Chronic', 'SHB_p_value',
                              'PreCore_Mutation', 'PreCore_Acute', 'PreCore_Chronic', 'PreCore_p_value',
                              'Core_Mutation', 'Core_Acute', 'Core_Chronic', 'Core_p_value')

# Convert relevant columns to numeric
mutations_data <- mutations_data %>%
  mutate(across(c(RT_Acute, RT_Chronic, SHB_Acute, SHB_Chronic, 
                  PreCore_Acute, PreCore_Chronic, Core_Acute, Core_Chronic), as.numeric))

# Classify mutations as "Shared", "Unique_Acute", or "Unique_Chronic"
mutations_data <- mutations_data %>%
  mutate(Mutation_Class = case_when(
    (RT_Acute > 0 & RT_Chronic > 0) |
      (SHB_Acute > 0 & SHB_Chronic > 0) |
      (PreCore_Acute > 0 & PreCore_Chronic > 0) |
      (Core_Acute > 0 & Core_Chronic > 0) ~ "Shared",
    
    (RT_Acute > 0 & (is.na(RT_Chronic) | RT_Chronic == 0)) | 
      (SHB_Acute > 0 & (is.na(SHB_Chronic) | SHB_Chronic == 0)) |
      (PreCore_Acute > 0 & (is.na(PreCore_Chronic) | PreCore_Chronic == 0)) |
      (Core_Acute > 0 & (is.na(Core_Chronic) | Core_Chronic == 0)) ~ "Unique_Acute",
    
    (RT_Chronic > 0 & (is.na(RT_Acute) | RT_Acute == 0)) | 
      (SHB_Chronic > 0 & (is.na(SHB_Acute) | SHB_Acute == 0)) |
      (PreCore_Chronic > 0 & (is.na(PreCore_Acute) | PreCore_Acute == 0)) |
      (Core_Chronic > 0 & (is.na(Core_Acute) | Core_Acute == 0)) ~ "Unique_Chronic",
    
    TRUE ~ NA_character_
  ))

# Reshape the data for plotting
mutations_long <- mutations_data %>%
  pivot_longer(cols = c(RT_Acute, RT_Chronic, SHB_Acute, SHB_Chronic, PreCore_Acute, PreCore_Chronic, Core_Acute, Core_Chronic),
               names_to = c("Region", "Cohort"),
               names_pattern = "(.*)_(.*)",
               values_to = "Percentage")

# Filter the data to include only mutations with >15% in either Acute or Chronic
mutations_long_filtered <- mutations_long %>%
  filter(Percentage > 15)

# Add a new column to set the x-axis category (Acute, Shared, Chronic)
mutations_long_filtered <- mutations_long_filtered %>%
  mutate(Category = case_when(
    Mutation_Class == "Unique_Acute" ~ "Acute",
    Mutation_Class == "Shared" ~ "Shared",
    Mutation_Class == "Unique_Chronic" ~ "Chronic"
  ))

# Create a combined column for mutation labels
mutations_long_filtered <- mutations_long_filtered %>%
  mutate(Mutation_Label = case_when(
    Region == "RT" ~ RT_Mutation,
    Region == "SHB" ~ SHB_Mutation,
    Region == "PreCore" ~ PreCore_Mutation,
    Region == "Core" ~ Core_Mutation
  ))

# Create the dot plot with labels for mutations
ggplot(mutations_long_filtered, aes(x = Category, y = Region, size = Percentage, color = Region, label = Mutation_Label)) +
  geom_point() +
  geom_text_repel(size = 3, max.overlaps = Inf, box.padding = 0.35, point.padding = 0.3, hjust = -0.3) +  # Label all mutations
  scale_size_continuous(range = c(3, 10)) +  # Make the size of points proportional to percentage
  labs(title = "Dot Plot of Shared and Unique Mutations",
       x = "Mutation Class (Unique and Shared)",
       y = NULL) +  # Remove the Y-axis label
  scale_color_manual(values = c("RT" = "red", "SHB" = "blue", "PreCore" = "orange", "Core" = "purple")) +  # Change PreCore to orange
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank())  # Remove y-axis ticks
