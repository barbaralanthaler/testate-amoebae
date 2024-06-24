library(tidyverse)
library(readxl)

amoeba <- read_excel("Store_mosse_amoeba_table.xlsx")

amoeba_conc <- amoeba %>% 
  select("Sample name", "Depth", "Volume", "lycopodiums", "total") %>%
  drop_na() %>%
  mutate("concentration" = ((total*10679)/(Volume*lycopodiums)))

age <- data.frame(
  Depth = c(1, 14, 20, 30, 40, 50, 55, 60),
  Age = c(0, 75, 136, 244, 352, 452, 505, 555)
)

amoeba_conc <- merge(amoeba_conc, age, by = "Depth", all.x = TRUE)

# Create labels containing information about depth and data
amoeba_conc$Depth_Age_Label <- with(amoeba_conc, ifelse(is.na(Age),
                                          paste0(Depth),
                                          paste0("(", Age, " cal yr BP) ", Depth)))

# Generate sequence with interval of 2 for the depth breaks
depth_breaks <- seq(0, max(amoeba_conc$Depth), by = 2)

# Create a named vector for the custom labels
depth_age_labels <- setNames(amoeba_conc$Depth_Age_Label, amoeba_conc$Depth)

# Ensure all depth breaks have labels
all_labels <- sapply(depth_breaks, function(x) {
  if (x %in% names(depth_age_labels)) {
    depth_age_labels[as.character(x)]
  } else {
    as.character(x)
  }
})


# Correct the plotting code
# Your existing plot code
plot_concentration <- ggplot(amoeba_conc, aes(x = `Depth`, y = concentration)) +
  geom_line() + # Add a line plot
  geom_point(size = 0.5) + 
  labs(
    title = "Variation of concentration along the core",
    x = "Sample Depth (cm)",
    y = expression(Concentration ~ (tests/cm^3))
  ) +
  theme_bw() +                   # Use a minimal theme for the plot
  coord_flip() +
  scale_x_reverse(breaks = depth_breaks, labels = all_labels)


# Print the plot
print(plot_concentration)
ggsave("concentration.png", plot = plot_concentration, width = 6, height = 8, dpi = 300)
