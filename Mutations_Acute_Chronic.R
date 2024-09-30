# install.packages("VennDiagram")

# Load necessary libraries
library(VennDiagram)
library(dplyr)

# Read the data from the CSV file
mutation_data <- read.csv("Mutations_Acute_Chronic.csv", stringsAsFactors = FALSE)

# Rename the columns to reflect the regions in acute and chronic cases
colnames(mutation_data) <- c("Acute_RT", "Acute_SHB", "Acute_PreCore", "Acute_Core", 
                             "Chronic_RT", "Chronic_SHB", "Chronic_PreCore", "Chronic_Core")

# Function to clean and extract unique mutations from a given column
extract_mutations <- function(column) {
  # Extract all mutations separated by commas, and trim whitespace
  mutations <- unlist(strsplit(column, ",\\s*"))
  # Remove 'Nil' and 'NA' values, and any empty strings
  mutations <- mutations[mutations != "Nil" & mutations != "" & !is.na(mutations)]
  return(mutations)
}

# Combine and extract all mutations from the acute cases across all regions
acute_mutations <- c(extract_mutations(mutation_data$Acute_RT),
                     extract_mutations(mutation_data$Acute_SHB),
                     extract_mutations(mutation_data$Acute_PreCore),
                     extract_mutations(mutation_data$Acute_Core))

# Combine and extract all mutations from the chronic cases across all regions
chronic_mutations <- c(extract_mutations(mutation_data$Chronic_RT),
                       extract_mutations(mutation_data$Chronic_SHB),
                       extract_mutations(mutation_data$Chronic_PreCore),
                       extract_mutations(mutation_data$Chronic_Core))

# Convert them to sets to find shared and exclusive mutations
acute_set <- unique(acute_mutations)
chronic_set <- unique(chronic_mutations)

# Generate a Venn diagram using VennDiagram package
venn.plot <- venn.diagram(
  x = list(Acute = acute_set, Chronic = chronic_set),
  category.names = c("Acute", "Chronic"),
  filename = NULL, # This is to plot directly in R
  output = TRUE,
  fill = c("lightblue", "lightgreen"),
  alpha = 0.5,
  cex = 1.5,
  fontface = "bold",
  cat.cex = 1.5,
  cat.pos = c(-20, 20),
  cat.dist = 0.05,
  cat.fontface = "bold"
)

# Plot the Venn diagram
grid.draw(venn.plot)

