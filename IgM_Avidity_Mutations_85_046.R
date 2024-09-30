# Recode categories into two new categories: "Category I" and "Category II"
combined_data$New_Category <- ifelse((combined_data$Measurement == "IgM" & combined_data$Category == ">8.5%") | 
                                       (combined_data$Measurement == "Avidity" & combined_data$Category == "<0.46%"),
                                     "IgM >8.5; AI <0.46", 
                                     "IgM <8.5; AI >0.46")

# Plot using ggplot2 with the colors and shapes switched between categories and legend title removed
ggplot(combined_data, aes(x = Mutation, y = Count, color = New_Category, shape = New_Category)) +
  geom_point(size = 3) +  # Scatter plot with different shapes for categories
  facet_wrap(~ Region, scales = "free_x") +
  labs(title = "IgM and Avidity by Mutation and Region",
       x = NULL,  # Remove the x-axis label
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        legend.title = element_blank()) +  # Remove the legend title
  scale_color_manual(values = c("IgM >8.5; AI <0.46" = "#FF9999", "IgM <8.5; AI >0.46" = "#9999FF")) +  # Switch colors
  scale_shape_manual(values = c("IgM >8.5; AI <0.46" = 17, "IgM <8.5; AI >0.46" = 16))  # Switch shapes (triangle for Cat I, circle for Cat II)
