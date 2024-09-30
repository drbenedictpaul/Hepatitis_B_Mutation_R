# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
data <- read.csv("/mnt/chromeos/GoogleDrive/SharedWithMe/68805/Data_Analytics_R/HBV_R/HBV_R - Clinical.csv")

# Clean column names
colnames(data) <- make.names(colnames(data), unique = TRUE)

# # Check the new column names
colnames(data)


# Now, based on your cleaned column names, you can select relevant columns
hbv_data_cleaned <- data %>%
  select(RT_Region, RT_Acute, RT_Chronic, 
         SHB_Region, SHB_Acute, SHB_Chronic, 
         Pre_Core_Region, Pre_Core_Region_Acute, Pre_Core_Region_Chronic, 
         Core_Region, Core_Acute, Core_Chronic)

# Reshape the data to long format, ensuring mutations are associated with their respective regions
hbv_data_long <- hbv_data_cleaned %>%
  pivot_longer(cols = c(RT_Acute, RT_Chronic, SHB_Acute, SHB_Chronic, 
                        Pre_Core_Region_Acute, Pre_Core_Region_Chronic, Core_Acute, Core_Chronic), 
               names_to = c("Region", "Cohort_Type"), 
               names_sep = "_", 
               values_to = "Frequency") %>%
  mutate(Mutation = case_when(
    Region == "RT" ~ RT_Region,
    Region == "SHB" ~ SHB_Region,
    Region == "PreCore" ~ Pre_Core_Region,
    Region == "Core" ~ Core_Region
  ))

# Check the reshaped data
head(hbv_data_long)

# # Create a plot for one region (e.g., RT region)
# ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Mutation, y = Frequency, fill = Cohort_Type)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   labs(title = "Mutation Frequencies in RT Region", y = "Frequency (%)", x = "Mutations") +
#   theme_minimal()

# Load necessary libraries
library(ggplot2)

# Lollipop plot for mutation frequencies in one region (e.g., RT Region)
lollipop_plot <- ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Mutation, y = Frequency)) +
  geom_segment(aes(x = Mutation, xend = Mutation, y = 0, yend = Frequency), color = "skyblue") +
  geom_point(aes(color = Cohort_Type), size = 3) +
  facet_wrap(~ Cohort_Type) +
  coord_flip() +  # Flip the coordinates for horizontal lollipop
  labs(title = "Mutation Frequencies in RT Region", y = "Frequency (%)", x = "Mutations") +
  theme_minimal()

# Heatmap of mutations across different cohorts and regions
heatmap_plot <- ggplot(hbv_data_long, aes(x = Mutation, y = Cohort_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  facet_wrap(~ Region, scales = "free_x") +  # Separate heatmaps for each region
  scale_fill_gradient(low = "blue", high = "red") +  # Color gradient from low to high frequencies
  labs(title = "Mutation Distribution Across Cohorts and Regions", x = "Mutations", y = "Cohort Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load the patchwork library to combine plots
# install.packages("patchwork")
library(patchwork)

# Combine the lollipop plot and heatmap side by side
combined_plot <- lollipop_plot + heatmap_plot

# Display the combined plot
combined_plot

# Heatmap for RT region
heatmap_RT <- ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Mutation, y = Cohort_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Mutation Distribution in RT Region", x = "Mutations", y = "Cohort Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lollipop plot for RT region
lollipop_RT <- ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Mutation, y = Frequency)) +
  geom_segment(aes(x = Mutation, xend = Mutation, y = 0, yend = Frequency), color = "skyblue") +
  geom_point(aes(color = Cohort_Type), size = 3) +
  coord_flip() +
  labs(title = "Mutation Frequencies in RT Region", y = "Frequency (%)", x = "Mutations") +
  theme_minimal()

# Display both plots one after another
heatmap_RT
lollipop_RT

# Filter for mutations with frequency above 20%
prominent_mutations <- hbv_data_long %>%
  filter(Region == "RT", Frequency > 20)  # Ensure that 'Frequency' is available in the dataset

# Heatmap for RT region with swapped x and y axes (without y-axis labels)
heatmap_RT <- ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Cohort_Type, y = Mutation, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Mutation Distribution in RT Region", x = "Cohort Type", y = "Mutations") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text (Mutation labels)
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1)
  ) +
  geom_text(data = prominent_mutations, aes(x = Cohort_Type, y = Mutation, label = Mutation),
            color = "black", size = 3, vjust = 1)  # Adjust size and position as needed

# Lollipop plot for RT region with labels beside/below dots and without y-axis labels
lollipop_RT <- ggplot(hbv_data_long %>% filter(Region == "RT"), aes(x = Frequency, y = Mutation)) +
  geom_segment(aes(x = 0, xend = Frequency, y = Mutation, yend = Mutation), color = "skyblue") +
  geom_point(aes(color = Cohort_Type), size = 3) +
  labs(title = "Mutation Frequencies in RT Region", x = "Frequency (%)", y = "Mutations") +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +  # Remove y-axis text (Mutation labels)
  geom_text(data = prominent_mutations, 
            aes(x = Frequency, y = Mutation, label = Mutation), 
            color = "black", size = 3, 
            hjust = ifelse(prominent_mutations$Frequency > 70, 0.5, -0.2),  # Labels below for >70% freq
            vjust = ifelse(prominent_mutations$Frequency > 70, 1.5, 0),     # Adjust vjust for above 70%
            nudge_x = ifelse(prominent_mutations$Frequency > 70, 0, 1))     # Nudge to the right for ≤70%

# Display both plots one after another
heatmap_RT
lollipop_RT






