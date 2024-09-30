# Load required libraries
library(ggplot2)
library(dplyr)

# Load the IgM data
igm_data <- read.csv("IgM_Mutations.csv", stringsAsFactors = FALSE)
colnames(igm_data) <- c("RT_Region", ">8.5%_RT", "<8.5%_RT", 
                        "SHB_Region", ">8.5%_SHB", "<8.5%_SHB", 
                        "Pre_Core_Region", ">8.5%_Pre_Core", "<8.5%_Pre_Core", 
                        "Core_Region", ">8.5%_Core", "<8.5%_Core")

# Load the Avidity data
avidity_data <- read.csv("Avidity_Mutations.csv", stringsAsFactors = FALSE)
colnames(avidity_data) <- c("RT_Region", "<0.46%_RT", ">0.46%_RT", 
                            "SHB_Region", "<0.46%_SHB", ">0.46%_SHB", 
                            "Pre_Core_Region", "<0.46%_Pre_Core", ">0.46%_Pre_Core", 
                            "Core_Region", "<0.46%_Core", ">0.46%_Core")

# Convert count columns to numeric for IgM
igm_data$`>8.5%_RT` <- as.numeric(igm_data$`>8.5%_RT`)
igm_data$`<8.5%_RT` <- as.numeric(igm_data$`<8.5%_RT`)
igm_data$`>8.5%_SHB` <- as.numeric(igm_data$`>8.5%_SHB`)
igm_data$`<8.5%_SHB` <- as.numeric(igm_data$`<8.5%_SHB`)
igm_data$`>8.5%_Pre_Core` <- as.numeric(igm_data$`>8.5%_Pre_Core`)
igm_data$`<8.5%_Pre_Core` <- as.numeric(igm_data$`<8.5%_Pre_Core`)
igm_data$`>8.5%_Core` <- as.numeric(igm_data$`>8.5%_Core`)
igm_data$`<8.5%_Core` <- as.numeric(igm_data$`<8.5%_Core`)

# Convert count columns to numeric for Avidity
avidity_data$`<0.46%_RT` <- as.numeric(avidity_data$`<0.46%_RT`)
avidity_data$`>0.46%_RT` <- as.numeric(avidity_data$`>0.46%_RT`)
avidity_data$`<0.46%_SHB` <- as.numeric(avidity_data$`<0.46%_SHB`)
avidity_data$`>0.46%_SHB` <- as.numeric(avidity_data$`>0.46%_SHB`)
avidity_data$`<0.46%_Pre_Core` <- as.numeric(avidity_data$`<0.46%_Pre_Core`)
avidity_data$`>0.46%_Pre_Core` <- as.numeric(avidity_data$`>0.46%_Pre_Core`)
avidity_data$`<0.46%_Core` <- as.numeric(avidity_data$`<0.46%_Core`)
avidity_data$`>0.46%_Core` <- as.numeric(avidity_data$`>0.46%_Core`)

# Reshape IgM data into long format
rt_igm_long <- data.frame(Mutation = igm_data$RT_Region, 
                          Category = c(rep(">8.5%", nrow(igm_data)), rep("<8.5%", nrow(igm_data))),
                          Count = c(igm_data$`>8.5%_RT`, igm_data$`<8.5%_RT`),
                          Region = "RT", Type = "IgM")

shb_igm_long <- data.frame(Mutation = igm_data$SHB_Region, 
                           Category = c(rep(">8.5%", nrow(igm_data)), rep("<8.5%", nrow(igm_data))),
                           Count = c(igm_data$`>8.5%_SHB`, igm_data$`<8.5%_SHB`),
                           Region = "SHB", Type = "IgM")

pre_core_igm_long <- data.frame(Mutation = igm_data$Pre_Core_Region, 
                                Category = c(rep(">8.5%", nrow(igm_data)), rep("<8.5%", nrow(igm_data))),
                                Count = c(igm_data$`>8.5%_Pre_Core`, igm_data$`<8.5%_Pre_Core`),
                                Region = "Pre_Core", Type = "IgM")

core_igm_long <- data.frame(Mutation = igm_data$Core_Region, 
                            Category = c(rep(">8.5%", nrow(igm_data)), rep("<8.5%", nrow(igm_data))),
                            Count = c(igm_data$`>8.5%_Core`, igm_data$`<8.5%_Core`),
                            Region = "Core", Type = "IgM")

# Reshape Avidity data into long format
rt_avidity_long <- data.frame(Mutation = avidity_data$RT_Region, 
                              Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                              Count = c(avidity_data$`<0.46%_RT`, avidity_data$`>0.46%_RT`),
                              Region = "RT", Type = "Avidity")

shb_avidity_long <- data.frame(Mutation = avidity_data$SHB_Region, 
                               Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                               Count = c(avidity_data$`<0.46%_SHB`, avidity_data$`>0.46%_SHB`),
                               Region = "SHB", Type = "Avidity")

pre_core_avidity_long <- data.frame(Mutation = avidity_data$Pre_Core_Region, 
                                    Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                                    Count = c(avidity_data$`<0.46%_Pre_Core`, avidity_data$`>0.46%_Pre_Core`),
                                    Region = "Pre_Core", Type = "Avidity")

core_avidity_long <- data.frame(Mutation = avidity_data$Core_Region, 
                                Category = c(rep("<0.46%", nrow(avidity_data)), rep(">0.46%", nrow(avidity_data))),
                                Count = c(avidity_data$`<0.46%_Core`, avidity_data$`>0.46%_Core`),
                                Region = "Core", Type = "Avidity")

# Combine IgM and Avidity data
combined_data <- rbind(rt_igm_long, shb_igm_long, pre_core_igm_long, core_igm_long, 
                       rt_avidity_long, shb_avidity_long, pre_core_avidity_long, core_avidity_long)

# Assign categories for combined conditions without the '%' symbol
combined_data <- combined_data %>%
  mutate(Combined_Category = case_when(
    (Type == "IgM" & Category == ">8.5%") | (Type == "Avidity" & Category == "<0.46%") ~ "IgM > 8.5 and Avidity < 0.46",
    (Type == "IgM" & Category == "<8.5%") | (Type == "Avidity" & Category == ">0.46%") ~ "IgM < 8.5 and Avidity > 0.46"
  ))

# Set the factor levels for `Combined_Category` to reorder the legend
combined_data$Combined_Category <- factor(combined_data$Combined_Category, 
                                          levels = c("IgM > 8.5 and Avidity < 0.46", 
                                                     "IgM < 8.5 and Avidity > 0.46"))

# Set the factor levels for `Type` to reorder the shapes legend
combined_data$Type <- factor(combined_data$Type, levels = c("IgM", "Avidity"))

# Set the factor levels for Region to ensure correct order
combined_data$Region <- factor(combined_data$Region, levels = c("RT", "SHB", "Pre_Core", "Core"))

# Plot using ggplot2
ggplot(combined_data, aes(x = Mutation, y = Count, color = Combined_Category, shape = Type)) +
  geom_point(size = 3) +  # Scatter plot
  facet_wrap(~ Region, scales = "free_x") +
  labs(title = "Anti-HBc differentiation threshold and mutation frequencies in various regions of HBV genome",
       x = "Mutations",
       y = "occurrence (%)") +
  theme_minimal() +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +  # Vary colors
  scale_shape_manual(values = c(16, 17)) +  # Different shapes for IgM and Avidity
  theme(legend.title = element_blank(),  # Remove legend titles
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank()) +
  # geom_text(data = combined_data %>%
  #             group_by(Mutation) %>%
  #             filter(Count == max(Count)) %>%
  #             ungroup(),
  #           aes(label = ifelse(Count >= 50, Mutation, "")), 
  #           hjust = 1.2, vjust = 0.5, size = 2.8)  # Shift labels to the left of the points


geom_text(data = combined_data %>%
            group_by(Mutation) %>%
            filter(Count == max(Count)) %>%
            ungroup(),
          aes(label = ifelse(Count >= 50, Mutation, ""), 
              hjust = ifelse(Region == "SHB" & Mutation == "A128V" | Mutation == "T189I", -0.2, 1.2)),  # Adjust hjust for A128V mutation in SHB
          vjust = 0.5, size = 2.85)  # Adjusted font size for the labels

