final_merged_data <- read.csv("/mnt/chromeos/GoogleDrive/SharedWithMe/68805/Data_Analytics_R/HBV_R/Final_Merged_Mutation_Data.csv")

# Required Libraries
library(pROC)

# Assuming binary outcome data (Acute = 1, Chronic = 0)
roc_curve_IgM <- roc(final_merged_data$Acute, final_merged_data$IgM_Greater85)
roc_curve_Avidity <- roc(final_merged_data$Acute, final_merged_data$AvI_Greater046)

# Plot the ROC curve for IgM
plot(roc_curve_IgM, col="blue", main="ROC Curve for IgM as Diagnostic Marker")
plot(roc_curve_Avidity, col="red", add=TRUE)  # Adding Avidity ROC on the same plot
legend("bottomright", legend=c("IgM", "Avidity Index"), col=c("blue", "red"), lwd=2)
