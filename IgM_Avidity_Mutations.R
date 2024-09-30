# Load required libraries
library(ggplot2)

# Load the IgM and Avidity data, skipping the first two rows (headers)
igm_data <- read.csv("IgM_Mutations.csv", skip = 2, stringsAsFactors = FALSE)
avidity_data <- read.csv("Avidity_Mutations.csv", skip = 2, stringsAsFactors = FALSE)

# Clean and rename the column names for IgM data
colnames(igm_data) <- c("RT_Region", "<8.5%_RT", ">8.5%_RT", 
                        "SHB_Region", "<8.5%_SHB", ">8.5%_SHB", 
                        "Pre_Core_Region", "<8.5%_Pre_Core", ">8.5%_Pre_Core", 
                        "Core_Region", "<8.5%_Core", ">8.5%_Core")

# Reshape IgM data into long format
rt_igm_long <- data.frame(Mutation = igm_data$RT_Region, 
                          Category = c(rep("<8.5%", nrow(igm_data)), rep(">8.5%", nrow(igm_data))),
                          Count = c(igm_data$`<8.5%_RT`, igm_data$`>8.5%_RT`),
                          Region = "RT",
                          Measurement = "IgM")

shb_igm_long <- data.frame(Mutation = igm_data$SHB_Region, 
                           Category = c(rep("<8.5%", nrow(igm_data)), rep(">8.5%", nrow(igm_data))),
                           Count = c(igm_data$`<8.5%_SHB`, igm_data$`>8.5%_SHB`),
                           Region = "SHB",
                           Measurement = "IgM")

pre_core_igm_long <- data.frame(Mutation = igm_data$Pre_Core_Region, 
                                Category = c(rep("<8.5%", nrow(igm_data)), rep(">8.5%", nrow(igm_data))),
                                Count = c(igm_data$`<8.5%_Pre_Core`, igm_data$`>8.5%_Pre_Core`),
                                Region = "Pre_Core",
                                Measurement = "IgM")

core_igm_long <- data.frame(Mutation = igm_data$Core_Region, 
                            Category = c(rep("<8.5%", nrow(igm_data)), rep(">8.5%", nrow(igm_data))),
                            Count = c(igm_data$`<8.5%_Core`, igm_data$`>8.5%_Core`),
                            Region = "Core",
                            Measurement = "IgM")

# Combine IgM data into one dataset
igm_combined <- rbind(rt_igm_long, shb_igm_long, pre_core_igm_long, core_igm_long)

# Clean and rename the column names for Avidity data
colnames(avidity_data) <- c("RT_Region", "<0.46%_RT", ">0.46%_RT", 
                            "SHB_Region", "<0.46%_SHB", ">0.46%_SHB", 
                            "Pre_Core_Region", "<0.46%_Pre_Core", ">0.46%_Pre_Core", 
                            "Core_Region", "<0.46%_Core", ">0.46%_Core")

# Reshape Avidity data into long format
rt_avidity_long <- data.frame(Mutation = avidity_data$RT_Region, 
                              Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                              Count = c(avidity_data$`<0.46%_RT`, avidity_data$`>0.46%_RT`),
                              Region = "RT",
                              Measurement = "Avidity")

shb_avidity_long <- data.frame(Mutation = avidity_data$SHB_Region, 
                               Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                               Count = c(avidity_data$`<0.46%_SHB`, avidity_data$`>0.46%_SHB`),
                               Region = "SHB",
                               Measurement = "Avidity")

pre_core_avidity_long <- data.frame(Mutation = avidity_data$Pre_Core_Region, 
                                    Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                                    Count = c(avidity_data$`<0.46%_Pre_Core`, avidity_data$`>0.46%_Pre_Core`),
                                    Region = "Pre_Core",
                                    Measurement = "Avidity")

core_avidity_long <- data.frame(Mutation = avidity_data$Core_Region, 
                                Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                                Count = c(avidity_data$`<0.46%_Core`, avidity_data$`>0.46%_Core`),
                                Region = "Core",
                                Measurement = "Avidity")

# Combine Avidity data into one dataset
avidity_combined <- rbind(rt_avidity_long, shb_avidity_long, pre_core_avidity_long, core_avidity_long)

# Combine both IgM and Avidity data into one dataset
combined_data <- rbind(igm_combined, avidity_combined)

# Ensure the 'Count' column is numeric
combined_data$Count <- as.numeric(combined_data$Count)

# Remove any rows with NA in 'Mutation' or 'Count'
combined_data <- combined_data[!is.na(combined_data$Mutation) & !is.na(combined_data$Count), ]

# Set the correct order of the regions (RT, SHB, Pre Core, Core)
combined_data$Region <- factor(combined_data$Region, levels = c("RT", "SHB", "Pre_Core", "Core"))

# Plot using ggplot2 with different shapes for IgM and Avidity, but no x-axis labels
ggplot(combined_data, aes(x = Mutation, y = Count, color = Category, shape = Measurement)) +
  geom_point(size = 3) +  # Scatter plot with different shapes
  facet_wrap(~ Region, scales = "free_x") +
  labs(title = "IgM and Avidity by Mutation and Region",
       x = NULL,  # Remove the x-axis label
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  scale_color_manual(values = c("#FF9999", "#9999FF", "#66CC66", "#CC6666"), 
                     labels = c("<8.5%", ">8.5%", "<0.46%", ">0.46%")) +
  scale_shape_manual(values = c(16, 17))  # Circle for IgM, Triangle for Avidity
