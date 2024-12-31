# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)


data <- read.csv("C:/Users/suran/OneDrive/Desktop/MSc Modules/Second Semister/Team Research/Team Project/Dataset/vgsales (1).csv")

platforms <- c("PS2", "X360", "PS3", "Wii", "DS")
filtered_data <- data %>% filter(Platform %in% platforms)

platform_sales <- filtered_data %>%
  group_by(Platform) %>%
  summarise(
    NA_Sales = sum(NA_Sales, na.rm = TRUE),
    EU_Sales = sum(EU_Sales, na.rm = TRUE),
    JP_Sales = sum(JP_Sales, na.rm = TRUE),
    Other_Sales = sum(Other_Sales, na.rm = TRUE)
  )


platform_sales_long <- platform_sales %>%
  pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales), 
               names_to = "Region", 
               values_to = "Sales") %>%
  group_by(Platform) %>%
  mutate(Proportion = Sales / sum(Sales))

# Create the stacked bar chart with labels
ggplot(platform_sales_long, aes(x = Platform, y = Proportion, fill = Region)) +
  geom_bar(stat = "identity") +
   geom_text(aes(label = round(Sales, 2)), 
            position = position_stack(vjust = 0.5), 
           size = 3, color = "white") +
  labs(title = "Sales Proportion by Region for Each Platform",
       x = "Platform",
       y = "Sales Proportion") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.title = element_text(size = 10))

