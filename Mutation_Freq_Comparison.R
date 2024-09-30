# Find the maximum value in the data
max_value <- max(combined_data$Frequency, na.rm = TRUE)

# Adjust the scale_fill_gradient limits based on the maximum value
ggplot(combined_data, aes(x = Region, y = Mutation_Type, fill = Frequency)) +
  geom_tile(color = "white") +
  
  # Set the color scale limits to cover the full range of values
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, max_value)) +
  
  labs(title = "Comparison of Mutation Frequencies",
       x = "Virus Region", y = "Mutation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Dataset)
