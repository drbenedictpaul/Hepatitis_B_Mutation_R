# Load required libraries
library(ggplot2)

# Load the data
data <- read.csv("Avidity_Mutations.csv", stringsAsFactors = FALSE)

# Clean the column names
colnames(data) <- c("RT_Region", "<0.46%_RT", ">0.46%_RT", 
                    "SHB_Region", "<0.46%_SHB", ">0.46%_SHB", 
                    "Pre_Core_Region", "<0.46%_Pre_Core", ">0.46%_Pre_Core", 
                    "Core_Region", "<0.46%_Core", ">0.46%_Core")

# Convert count columns to numeric
data$`<0.46%_RT` <- as.numeric(data$`<0.46%_RT`)
data$`>0.46%_RT` <- as.numeric(data$`>0.46%_RT`)
data$`<0.46%_SHB` <- as.numeric(data$`<0.46%_SHB`)
data$`>0.46%_SHB` <- as.numeric(data$`>0.46%_SHB`)
data$`<0.46%_Pre_Core` <- as.numeric(data$`<0.46%_Pre_Core`)
data$`>0.46%_Pre_Core` <- as.numeric(data$`>0.46%_Pre_Core`)
data$`<0.46%_Core` <- as.numeric(data$`<0.46%_Core`)
data$`>0.46%_Core` <- as.numeric(data$`>0.46%_Core`)

# Reshape each region into a long format
rt_long <- data.frame(Mutation = data$RT_Region, 
                      Avidity_Category = c(rep("<0.46%", nrow(data)), rep(">0.46%", nrow(data))),
                      Count = c(data$`<0.46%_RT`, data$`>0.46%_RT`),
                      Region = "RT")

shb_long <- data.frame(Mutation = data$SHB_Region, 
                       Avidity_Category = c(rep("<0.46%", nrow(data)), rep(">0.46%", nrow(data))),
                       Count = c(data$`<0.46%_SHB`, data$`>0.46%_SHB`),
                       Region = "SHB")

pre_core_long <- data.frame(Mutation = data$Pre_Core_Region, 
                            Avidity_Category = c(rep("<0.46%", nrow(data)), rep(">0.46%", nrow(data))),
                            Count = c(data$`<0.46%_Pre_Core`, data$`>0.46%_Pre_Core`),
                            Region = "Pre_Core")

core_long <- data.frame(Mutation = data$Core_Region, 
                        Avidity_Category = c(rep("<0.46%", nrow(data)), rep(">0.46%", nrow(data))),
                        Count = c(data$`<0.46%_Core`, data$`>0.46%_Core`),
                        Region = "Core")

# Combine all regions into one dataset
combined_data <- rbind(rt_long, shb_long, pre_core_long, core_long)

# Remove rows where Mutation is NA
combined_data <- combined_data[!is.na(combined_data$Mutation), ]

# Plot using ggplot2
ggplot(combined_data, aes(x = Mutation, y = Count, color = Avidity_Category)) +
  geom_point(size = 3) +  # Scatter plot
  facet_wrap(~ Region, scales = "free_x") +
  labs(title = "Avidity by Mutation and Region",
       x = "Mutation",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#FF9999", "#9999FF"), labels = c("<0.46%", ">0.46%"))
