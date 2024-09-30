# Load required libraries
library(ggplot2)

# Load the data
data <- read.csv("IgM_Mutations.csv", stringsAsFactors = FALSE)

# Clean the column names
colnames(data) <- c("RT_Region", ">8.5%_RT", "<8.5%_RT", 
                    "SHB_Region", ">8.5%_SHB", "<8.5%_SHB", 
                    "Pre_Core_Region", ">8.5%_Pre_Core", "<8.5%_Pre_Core", 
                    "Core_Region", ">8.5%_Core", "<8.5%_Core")

# Convert count columns to numeric
data$`>8.5%_RT` <- as.numeric(data$`>8.5%_RT`)
data$`<8.5%_RT` <- as.numeric(data$`<8.5%_RT`)
data$`>8.5%_SHB` <- as.numeric(data$`>8.5%_SHB`)
data$`<8.5%_SHB` <- as.numeric(data$`<8.5%_SHB`)
data$`>8.5%_Pre_Core` <- as.numeric(data$`>8.5%_Pre_Core`)
data$`<8.5%_Pre_Core` <- as.numeric(data$`<8.5%_Pre_Core`)
data$`>8.5%_Core` <- as.numeric(data$`>8.5%_Core`)
data$`<8.5%_Core` <- as.numeric(data$`<8.5%_Core`)

# Reshape each region into a long format
rt_long <- data.frame(Mutation = data$RT_Region, 
                      IgM_Category = c(rep(">8.5%", nrow(data)), rep("<8.5%", nrow(data))),
                      Count = c(data$`>8.5%_RT`, data$`<8.5%_RT`),
                      Region = "RT")

shb_long <- data.frame(Mutation = data$SHB_Region, 
                       IgM_Category = c(rep(">8.5%", nrow(data)), rep("<8.5%", nrow(data))),
                       Count = c(data$`>8.5%_SHB`, data$`<8.5%_SHB`),
                       Region = "SHB")

pre_core_long <- data.frame(Mutation = data$Pre_Core_Region, 
                            IgM_Category = c(rep(">8.5%", nrow(data)), rep("<8.5%", nrow(data))),
                            Count = c(data$`>8.5%_Pre_Core`, data$`<8.5%_Pre_Core`),
                            Region = "Pre_Core")

core_long <- data.frame(Mutation = data$Core_Region, 
                        IgM_Category = c(rep(">8.5%", nrow(data)), rep("<8.5%", nrow(data))),
                        Count = c(data$`>8.5%_Core`, data$`<8.5%_Core`),
                        Region = "Core")

# Combine all regions into one dataset
combined_data <- rbind(rt_long, shb_long, pre_core_long, core_long)

# Remove rows where Mutation is NA
combined_data <- combined_data[!is.na(combined_data$Mutation), ]

# Plot using ggplot2
ggplot(combined_data, aes(x = Mutation, y = Count, color = IgM_Category)) +
  geom_point(size = 3) +  # Removed jittering for proper alignment
  facet_wrap(~ Region, scales = "free_x") +
  labs(title = "IgM Concentration by Mutation and Region",
       x = "Mutation",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#FF9999", "#9999FF"), labels = c(">8.5%", "<8.5%"))
