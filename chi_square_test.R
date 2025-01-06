# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the data
vg_data <- read.csv("/mnt/data/vgsales (1).csv")

# Define the platforms of interest
selected_platforms <- c("PS2", "X360", "PS3", "Wii", "DS")

# Filter the dataset for the selected platforms
filtered_vg_data <- vg_data %>% filter(Platform %in% selected_platforms)

# Aggregate sales data by platform and region
sales_summary <- filtered_vg_data %>%
  group_by(Platform) %>%
  summarise(
    NorthAmerica = sum(NA_Sales, na.rm = TRUE),
    Europe = sum(EU_Sales, na.rm = TRUE),
    Japan = sum(JP_Sales, na.rm = TRUE),
    Others = sum(Other_Sales, na.rm = TRUE)
  )

# Reshape data into a table format for the Chi-Square Test
table_data <- sales_summary %>%
  pivot_longer(cols = -Platform, names_to = "Region", values_to = "Sales") %>%
  pivot_wider(names_from = Platform, values_from = Sales)

# Convert to matrix and set row names
row.names(table_data) <- table_data$Region
table_data <- table_data[, -1]

# Perform the Chi-Square Test
chi_square_result <- chisq.test(as.matrix(table_data))

# Print the Chi-Square Test results
print("Chi-Square Test Results:")
print(chi_square_result)

# Extract and print the p-value
p_value <- chi_square_result$p.value
cat("P-Value:", p_value, "\n")