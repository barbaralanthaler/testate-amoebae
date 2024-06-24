# Load necessary libraries
library(tidyverse)
library(readxl)
library(xlsx)


# Load the data from the Excel file
amoeba <- read_excel("Store_mosse_amoeba_table.xlsx")

# Calculate species richness for each sample
# Species richness is the count of different species present in each row
species_columns <- names(amoeba)[4:ncol(amoeba)]  # Adjust the column index as needed
amoeba$Species_Richness <- rowSums(amoeba[ , species_columns] > 0, na.rm = TRUE)

# Create a new dataframe with Sample name, Depth, and Species Richness
result <- amoeba %>% select(`Sample name`, Depth, Species_Richness)

result <- drop_na(result)

# Print the new dataframe
print(result)


plot_SR <- ggplot(result, aes(x = Depth, y = Species_Richness)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(title = "Species Richness",
       x = "Sample Depth (cm)",
       y = "Species Richness (S)") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse(breaks = seq(0, max(result$Depth), by = 2)) +
  scale_y_continuous(breaks = seq(min((result$Species_Richness)), max(result$Species_Richness), by = 4))

# Print the plot
print(plot_SR)


# Save the result to a new XLSX file
write.xlsx(result, "species_richness.xlsx")

# Save the plot
ggsave("Species richness.png", plot = plot_SR, width = 6, height = 8, dpi = 300)

