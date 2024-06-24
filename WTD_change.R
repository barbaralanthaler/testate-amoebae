library(tidyverse)
library(readxl)
library(openxlsx)

# Import the dataframe containing the reconstructed WTD values
WTD <- read_excel("All_samples_no_A_flavum_WTD.xlsx")

# Clean the dataframe
WTDs <- WTD %>%
  select(CodeNum, CodeName, WAPLS_C5) %>%
  mutate(Depth = round(seq(1, 60, length.out = n()))) %>%
  filter(WAPLS_C5 != -99.900000)

# Add age data
age <- data.frame(
  Depth = c(1, 14, 20, 30, 40, 50, 55, 60),
  Age = c(0, 75, 136, 244, 352, 452, 505, 555)
)

WTDs <- merge(WTDs, age, by = "Depth", all.x = TRUE)

# Create labels containing information about depth and data
WTDs$Depth_Age_Label <- with(WTDs, ifelse(is.na(Age),
                                          paste0(Depth),
                                          paste0("(", Age, " cal yr BP) ", Depth)))

# Generate sequence with interval of 2 for the depth breaks
depth_breaks <- seq(0, max(WTDs$Depth), by = 2)

# Create a named vector for the custom labels
depth_age_labels <- setNames(WTDs$Depth_Age_Label, WTDs$Depth)

# Ensure all depth breaks have labels
all_labels <- sapply(depth_breaks, function(x) {
  if (x %in% names(depth_age_labels)) {
    depth_age_labels[as.character(x)]
  } else {
    as.character(x)
  }
})


# Plot the WTD reconstruction
plot_WTD <- ggplot(WTDs, aes(x = Depth, y = WAPLS_C5)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(title = "WTD Reconstruction",
       x = "Sample Depth (cm)",
       y = "Water Table Depth (cm)") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse(breaks = depth_breaks, labels = all_labels)

print(plot_WTD)
ggsave("All_samples_WTD_no_A_flavum.png", plot = plot_WTD, width = 6, height = 8, dpi = 300)

# Calculate Z-scores
WTDs <- WTDs %>%
  mutate(Z_Scores = (WAPLS_C5 - mean(WAPLS_C5, na.rm = TRUE)) / sd(WAPLS_C5, na.rm = TRUE))

# Plot the WTD reconstruction with Z-scores
plot_z_scores <- ggplot(WTDs, aes(x = Depth, y = Z_Scores)) +
  geom_line() +
  geom_point(size = 0.5) +
  labs(title = "WTD Reconstruction",
       x = "Sample Depth (cm)",
       y = "Water Table Depth (z-score)") +
  theme_bw() +
  coord_flip() +
  scale_x_reverse(breaks = depth_breaks, labels = all_labels)

print(plot_z_scores)
ggsave("All_samples_WTD__no_A_flavum_z.png", plot = plot_z_scores, width = 6, height = 8, dpi = 300)

