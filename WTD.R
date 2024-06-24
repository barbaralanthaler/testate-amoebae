library(tidyverse)
library(readxl)
library(openxlsx)

# Import the dataframe containg the reconstructec WTD values
WTD <- read_excel("WTD.xlsx")

# Clean the dataframe
WTDs <- WTD %>%
  select(CodeNum, CodeName, WAPLS_C5) %>% #select columns of interest
  filter(WAPLS_C5 != -99.900000) %>% #remove values
  mutate(Depth = seq(10, 60, length.out = n()))

# Add age data
age <- data.frame(
  Depth = c(0, 14, 20, 30, 40, 50, 55, 60),
  Age = c(0, 75, 136, 244, 352, 452, 505, 555)
)

WTDs <- merge(WTDs, age, by = "Depth", all.x = TRUE)

# Create labels containing information about depth and data
WTDs$Depth_Age_Label <- with(WTDs, ifelse(is.na(Age), 
                                          paste0(Depth), 
                                          paste0("(", Age, " cal yr BP) ", Depth)))

# Create a named vector for the custom labels
depth_age_labels <- setNames(WTDs$Depth_Age_Label, WTDs$Depth)

plot <- ggplot(WTDs, aes(x = Depth, y = WAPLS_C5)) +
  geom_line() +                  # Add a line plot
  geom_smooth(method = "lm", col = "blue", se = FALSE, size = 0.5) +
  labs(title = "WTD Reconstruction",
       x = "Sample Depth (cm)",
       y = "Water Table Depth (cm)") +
  theme_bw()        +        # Use a minimal theme for the plot
  coord_flip()  +
  scale_x_reverse(breaks = WTDs$Depth, labels = depth_age_labels)

print(plot)
ggsave("WTD.png", plot = plot, width = 6, height = 8, dpi = 300)

# Calculate Z-scores
WTDs <- WTDs %>%
  mutate(Z_Scores = (WAPLS_C5 - mean(WAPLS_C5, na.rm = TRUE)) / sd(WAPLS_C5, na.rm = TRUE))


# Create the ggplot with custom labels
plot_z_scores <- ggplot(WTDs, aes(x = Depth, y = Z_Scores)) +
  geom_line() +                  # Add a line plot
  geom_smooth(method = "lm", col = "blue", se = FALSE, size = 0.5) +
  labs(title = "WTD Reconstruction",
       x = "Sample Depth (cm)",
       y = "Water Table Depth (z-score)") +
  theme_bw() +                   # Use a minimal theme for the plot
  coord_flip() +
  scale_x_reverse(breaks = WTDs$Depth, labels = depth_age_labels)

print(plot_z_scores)
ggsave("WTD_z.png", plot = plot_z_scores, width = 6, height = 8, dpi = 300)
